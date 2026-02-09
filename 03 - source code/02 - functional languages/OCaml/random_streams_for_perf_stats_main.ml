(*
main.ml of random_streams_for_perf_stats

2025-05-31, 2025-06-22
2025-12-21: see below
2026-02-09: introduced extra variables bits_x_str and bits_hex_str to have a more common algorithmic implementation

build on Ubuntu 24 LTS: $ dune init proj random_streams_for_perf_stats
                        $ cd random_streams_for_perf_stats
                        $ dune build
                        $ dune build --display=verbose

run on Ubuntu 24 LTS:   $ sudo perf stat -r 20 ./_build/default/bin/main.exe


$ dune --version
3.20.2
$ ocaml --version
The OCaml toplevel, version 5.4.0
$

*)


let upper_limit  = 62501  (* 62501 for exactly 1M binary digits; a let binding is immutable in OCaml *)
(*let upper_limit  = 20 (* for testing *)*)
let m1   = upper_limit * 16
let k250 = upper_limit * 4

let m = 65521  (* = 2^16 - 15 *)
let a = 17364
let c = 0


let file_bits_x   = "random_bitstring.bin"
let file_bits_hex = "random_bitstring.byte"


let x = Array.make upper_limit 0 (* initialize fixed size array with 0's *)
let _ = Random.self_init ()    (* https://courses.cs.cornell.edu/cs3110/2021sp/textbook/testing/random.html *)

(*let bits_x = ref ""  (* make bits_x it mutable *)*)

let bits_x = Buffer.create m1  (* https://ocaml.org/manual/5.3/api/Buffer.html *)
let bits_hex = Buffer.create k250


(* https://www.bing.com/search?pc=MOZI&form=MOZLBR&q=OCaml+integer+in+binary+string+representation *)
(* partly some AI code with MS Bing*)
let integer_to_bin_string n =
  let j = 16 in
  let buffer = Buffer.create 16 in
  let rec aux (n, j) =
    if (n >= 0 && j >= 1) then (  (* the brackets in this function are absolutely crucial! *)
      aux (n / 2, j - 1);
      Buffer.add_char buffer (if n mod 2 = 0 then '0' else '1');
    )
  in
  if n = 0 then "0000000000000000"
  else (
    aux (n,j);
    Buffer.contents buffer
  )


let write_to_file filename content file_type =
  (* solution based on MS Bing prompt: "OCaml Out_channel.with_open_bin try with"
     using module Stdlib.Out_channel: https://ocaml.org/manual/5.3/api/Out_channel.html#examples *)

  try
    let out_channel = open_out_bin filename in  (*open_out_bin is OK for strings in Linux*)
    try
      output_string out_channel content;
      close_out out_channel;
      if (file_type = "bit") then (
        Printf.printf "\nBit stream has been written to disk under name:  %s" filename;
      ) else (
        Printf.printf "\nByte stream has been written to disk under name: %s\n" filename;
      )

    with e ->
      close_out_noerr out_channel; (* Ensures the channel is closed even if an error occurs *)
      raise e
  with
  | Sys_error e_msg -> Printf.printf "\ncould not write to file: %s -- %s\n" filename e_msg;
  | e               -> Printf.eprintf "\nUnexpected error: %s\n" (Printexc.to_string e);


;;


let main () =
  x.(0) <- Random.int (m - 1) + 1;  (* 0 (inclusive) and bound (exclusive); 2025-12-21 *)
  (* https://ocaml.org/manual/5.4/api/Random.html *)

  Printf.printf "\ngenerating a random bit stream...";  (* only one ; to get here stdout immediately! *)

  (* Order of Evaluation of Arguments
     https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora029.html
  *)

  (**********************  recursive master loop  ****************************)
  let rec masterloop i =
    x.(i) <- (a*x.(i-1) + c) mod m;
    (*Printf.printf "x.(i) = %d\n" x.(i);  (* for testing *)*)

    (* 2026-02-09: have extra string here like in the other language implementations *)
    let bits_x_str = (integer_to_bin_string(x.(i))) in
    Buffer.add_string bits_x bits_x_str;
    (*https://stackoverflow.com/questions/54517086/how-to-append-to-string-in-ocaml*)

    let bits_hex_str = (Printf.sprintf "%04x" x.(i)) in
    Buffer.add_string bits_hex bits_hex_str;

    if i >= upper_limit-1 then i
    else masterloop (i+1) (* brackets are essential here *)
  in
  ignore(masterloop 1);
  (**********************  end of recursive master loop  **********************)

  (*print_endline(Buffer.contents bits_x); (* for testing *)*)
  (*print_endline(Buffer.contents bits_hex); (* for testing *)*)

  (* write bit stream to disk *)
  let file_type = "bit" in
  (* 2026-02-09: have this name due to introduction of bits_x_str in the masterloop *)
  let bits_x_str_total = Buffer.contents bits_x in
  write_to_file file_bits_x bits_x_str_total file_type;

  (* write byte stream to disk *)
  let file_type = "byte" in
  let bits_hex_str_total = Buffer.contents bits_hex in  (* 2026-02-09 *)
  write_to_file file_bits_hex bits_hex_str_total file_type;;


main ();;  (* same as: let _ = main () *)
(* () is the single value of type unit *)


(* end of main.ml *)
