(*
main.ml of random_bitstring_and_flexible_password_generator

2025-05-24/25/26/27/31, 2025-06-22
2025-12-21: see below
2026-01-04: cosmetics

build on Ubuntu 24 LTS: $ dune init proj random_bitstring_and_flexible_password_generator
                        $ cd random_bitstring_and_flexible_password_generator
                        $ dune build

run on Ubuntu 24 LTS:   $ ./_build/default/bin/main.exe


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

let n_char_default = 12


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


(* see comments from the user_input_dialog working solution *)
let rec input_a_valid_number n_char =
  Printf.printf "\nPassword of %d printable chars OK? 'y' or another integer number >= 8: " n_char;
  flush stdout;
  let answer_str = input_line stdin in
  if answer_str = "y" then (
    n_char
  ) else
    match int_of_string_opt answer_str with
    | Some n -> if n < 8 then (
                  Printf.printf "enter an integer number >= 8 or 'y'\n";
                  flush stdout;
                  input_a_valid_number n_char_default;
                ) else n
    | None   -> (Printf.printf "enter an integer number >= 8 or 'y'\n";
                 flush stdout;
                 input_a_valid_number n_char_default;)


let answer_yes_or_no () =  (* make this a function with no argument!
                              This is very important to not have this being being executed immediately!! *)
  Printf.printf "\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ";
  flush stdout;
  let answer_str = input_line stdin in
  if answer_str = "y" then
    true
  else
    false


let char_range start_char end_char =
  let rec aux current acc =
    if current > end_char then acc
    else aux (Char.chr (Char.code current + 1)) (current :: acc) (*codepoints*)
  in
  String.of_seq (List.to_seq (List.rev (aux start_char [])))


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

  (**********************  recursive master loop  ****************************)
  let rec masterloop i =
    x.(i) <- (a*x.(i-1) + c) mod m;
    (*Printf.printf "x.(i) = %d\n" x.(i);  (* for testing *)*)

    Buffer.add_string bits_x (integer_to_bin_string(x.(i)));
    (*https://stackoverflow.com/questions/54517086/how-to-append-to-string-in-ocaml*)
    Buffer.add_string bits_hex (Printf.sprintf "%04x" x.(i));

    if i >= upper_limit-1 then i
    else masterloop (i+1) (* brackets are essential here *)
  in
  ignore(masterloop 1);
  (**********************  end of recursive master loop  **********************)

  (*print_endline(Buffer.contents bits_x); (* for testing *)*)
  (*print_endline(Buffer.contents bits_hex); (* for testing *)*)

  (* write bit stream to disk *)
  let file_type = "bit" in
  let bits_x_str = Buffer.contents bits_x in
  write_to_file file_bits_x bits_x_str file_type;

  (* write byte stream to disk *)
  let file_type = "byte" in
  let bits_hex_str = Buffer.contents bits_hex in
  write_to_file file_bits_hex bits_hex_str file_type;



  let n_char = input_a_valid_number n_char_default in
  (*Printf.printf "\nn_char = %d\n" n_char;  (*for testing*)*)

  let with_special_chars = (answer_yes_or_no ()) in
  (* answer_yes_or_no: return type is 'a -> bool because function with_special_chars is called with any type a' *)
  (* (answer_yes_or_no ()): return type is bool now! *)
  (* Printf.printf "\nwith_special_chars = %B\n" with_special_chars;  (*for testing*)*)

  let char_set =
    if with_special_chars then
      char_range '!' '~'
    else
      char_range 'a' 'z' ^
      char_range 'A' 'Z' ^
      char_range '0' '9'
  in
  (*Printf.printf "\nchar_set = %s\n" char_set;  (*for testing*)*)


  let i = ref 0 in (*referenced char counter for the password *)
  let pw_chars = ref "" in

  (* recursive solution instead of imperative while-loop *)
  let rec pw_generator j =
    if !i >= n_char then j
    else (
      let bin0 = integer_to_bin_string(x.(j)) in
      (*Printf.printf "\nbin0 = %s" bin0; (*for testing*)*)

      let bin0_0 = String.sub bin0 0 8 in  (* substring *)
      let bin0_1 = String.sub bin0 8 8 in
      (*Printf.printf "\nbin0_0 = %s -- bin0_1 = %s" bin0_0 bin0_1; (*for testing*)*)

      let char0 = int_of_string ("0b" ^ bin0_0) in
      let char1 = int_of_string ("0b" ^ bin0_1) in
      (*Printf.printf "\nchar0 = %d -- char1 = %d" char0 char1; (*for testing*)*)

      let char0a = Char.chr char0 in  (*codepoint*)
      let char1a = Char.chr char1 in  (*codepoint*)
      (*Printf.printf "\nchar0a = %c -- char1a = %c" char0a char1a; (*for testing*)*)

      if String.contains char_set char0a then (
        i := !i + 1;
        pw_chars := !pw_chars ^ String.make 1 char0a;
        ()
      );

      if String.contains char_set char1a && !i < n_char then (
        i := !i + 1;
        pw_chars := !pw_chars ^ String.make 1 char1a;
        ()
      );

      pw_generator (j+1)
    )
  in
  ignore(pw_generator 0);

  Printf.printf "\nYour password of %d characters is: %s\n" n_char !pw_chars;;
  (* Printf.printf "\nj = %d\n" !j; *)
  (* Printf.printf "\ni = %d\n" !i;; *)


main ();;  (* same as: let _ = main () *)
(* () is the single value of type unit *)


(* end of main.ml *)
