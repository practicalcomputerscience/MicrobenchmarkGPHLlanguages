(* random_streams_for_perf_stats.sml  -- for MLton Standard ML: this is with a Standard ML of New Jersey compliant random seed

2025-07-06/09/10
2025-12-21: see below
2026-08-05: renamed a couple of variables to be more compliant with other implementations
2026-10-24: no more RNG initialization at: val r = Random.rand (Int.fromLarge(secs mod maxInt), Int.fromLarge usecs),
            just using a hashed value from: val (secs, usecs) = getTime ().
            What really happens at function call Random.rand (int1, int2) is totally obscure to me in this MLton/(legacy?)SMLNJ mix.


build on Ubuntu 24 LTS: take mlton-20241230.x86_64-linux-gnu.tar.gz from: https://github.com/ii8/mlton-builds/releases/tag/20241230
                        unzip it and put path to: ./scripts/StandardML/mlton-20241230.x86_64-linux-gnu/mlton-on-20241230-release.x86_64-linux-gnu/bin/

                        $ mlton -verbose 1 ./random_streams_for_perf_stats.mlb

------
random_streams_for_perf_stats.mlb:
(* import libraries *)
$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb
$(SML_LIB)/basis/basis.mlb  (* this order is important! *)

(* program files *)
./random_streams_for_perf_stats.sml
------


run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ time ./random_streams_for_perf_stats
                        $ sudo perf stat -r 20 ./random_streams_for_perf_stats

$ mlton
MLton 20241230.203305-gb15e2d2
$

*)


(**********************************************************)
(* user defined functions                                 *)

(*
(* 1:1 solution from MS Bing AI *)
fun listToString [] = "[]"
    | listToString (x::xs) = "[" ^ elemToString x ^ restToString xs
  and restToString [] = "]"
    | restToString (x::xs) = ", " ^ elemToString x ^ restToString xs
  and elemToString x = Int.toString x (* Modify this for other types *)
*)

(* Function to print an array of integers *)
(* 1:1 solution from MS Bing AI *)
fun printIntArray arr =
    let
        val elements = Array.foldr (fn (x, acc) => Int.toString x :: acc) [] arr
        (* string (Int.toString x) is prepended with cons operator :: to list acc, which starts empty *)
        val result = String.concatWith ", " elements
    in
        print ("\n[" ^ result ^ "]\n")
    end


fun integer_to_bin_string (n : int): string =
  let
    fun toBinary 0 = ""
      | toBinary n = toBinary (n div 2) ^ (if n mod 2 = 0 then "0" else "1")
    val binary = toBinary n
    val paddedBinary = StringCvt.padLeft #"0" 16 binary
  in
    paddedBinary
  end


fun integer_to_hex_string (n : int): string =
  let
    fun toHex 0 = ""
      | toHex n = toHex (n div 16) ^
        (let
           val mod_ = n mod 16
         in
          case mod_ of
              0  => "0"
            | 1  => "1"
            | 2  => "2"
            | 3  => "3"
            | 4  => "4"
            | 5  => "5"
            | 6  => "6"
            | 7  => "7"
            | 8  => "8"
            | 9  => "9"
            | 10 => "a"
            | 11 => "b"
            | 12 => "c"
            | 13 => "d"
            | 14 => "e"
            | 15 => "f"
            | _ => ""
         end)
    val hexadec = toHex n
    val paddedHex = StringCvt.padLeft #"0" 4 hexadec
  in
    paddedHex
  end


val write_to_file = fn (filename: string, content: string, file_type: string) =>
  let
    val outStream = TextIO.openOut filename
  in
    TextIO.output (outStream, content);
    TextIO.closeOut outStream;
    if file_type = "bit" then
      print ("\nBit stream has been written to disk under name:  " ^ filename)  (* no val _ = ... here*)
    else
      print ("\nByte stream has been written to disk under name: " ^ filename)
  end
  handle IO.Io _ =>
    (print ("\ncould not write to file: " ^ filename))


(* end of user defined functions                          *)
(**********************************************************)


structure Main =
  struct
    val END = 62499  (* 62499 for exactly 1M binary digits; variable END is immutable *)
    (*val END = 10  (* for testing *)*)

    val m = 65521  (* = 2^16 - 15 *)
    val m_word : Word64.word = 0wx000000000000fff1  (* 2^16 - 15 as 64 bit hex word; correct typing with Word64.word is important for the LunarML compiler *)

    val a = 17364
    val c = 0

    val file_bits_x   = "random_bitstring.bin"
    val file_bits_hex = "random_bitstring.byte"


    val x = Array.array(END+1, 0)
    val bits_x = Array.array(END+1, "0000000000000000")
    val bits_hex = Array.array(END+1, "0000")

    (*val xx = MLton.Vector.create (END+1) (* OK *)*)

    (* 2026-10-24: constants for the 64 bit FNV-1a hashing algorithm: *)
    val offsetBasis64 : Word64.word = 0wxcbf29ce484222325  (* 64-bit, which is not prime! *)
    val prime64       : Word64.word = 0wx00000100000001B3  (* FNV prime in 64 bit *)
    (* source: https://mojoauth.com/security-guides/fnv-1a-in-c *)
    (* also see at: ./lib/smlnj-lib/Util/fnv-hash.sml *)


    (*
    (* http://www.mlton.org/MLtonRandom *)
    val seed_ = MLton.Random.seed ()  (* this is a new random number with every program start! *)
    val seed_word = valOf seed_       (* type conversion from word option to word *)
    (* http://www.mlton.org/DefineTypeBeforeUse *)

    (*val _ = print ("seed_word = " ^ Word.toString seed_word ^ "\n")  (* for testing *)*)
    (*  23EF4897, 3F741FB, 23D4B19D *)
    (* this can easily overflow if not taken care of => modulus operation: *)
    val seed_word_mod = seed_word mod m_word

    val start_seed = Word.toInt seed_word_mod
    *)

    fun getTime () = IntInf.divMod (Time.toMicroseconds(Time.now()), 1000000)  (* get the current time as (seconds, useconds)
    see from: https://github.com/smlnj/legacy/blob/main/smlnj-lib/UUID/gen-uuid.sml#L20 *)

    (* val maxInt = IntInf.fromInt (valOf Int.maxInt) + 1 *)
    val (secs, usecs) = getTime ()
    (* 2026-10-24: retired for obscurity: val r = Random.rand (Int.fromLarge(secs mod maxInt), Int.fromLarge usecs)  (* r of type Random.rand *) *)
    (*val start_seed = (Int.fromLarge(secs mod maxInt) + Int.fromLarge usecs) mod (m - 2) + 1*)

    (* 2026-10-24: instead, doing my own hashing and seeding: *)
    (* val start_seed1 = Int.fromLarge secs  (* for testing *) *)
    (* val _ = print ("\nstart_seed1 = " ^ Int.toString start_seed1)  (* for testing *) *)
    (* val start_seed2 = Int.fromLarge usecs  (* for testing *) *)
    (* val _ = print ("\nstart_seed2 = " ^ Int.toString start_seed2)  (* for testing *) *)
    val start_seed0 = Word64.xorb(offsetBasis64, Word64.fromLargeInt(secs)) * prime64
    val start_seed1 = Word64.xorb(start_seed0,   Word64.fromLargeInt(usecs)) * prime64
    (*val _ = print ("\nstart_seed1 = " ^ Word64.toString start_seed1)  (* for testing *)*)
    val start_seed2 = Word64.mod(start_seed1, m_word - 0wx0002) + 0wx0001
    (*val _ = print ("\nstart_seed2 = " ^ Word64.toString start_seed2)  (* for testing *)*)
    val start_seed  = Word64.toInt start_seed2
    (*val _ = print ("\nstart_seed = " ^ Int.toString start_seed)  (* for testing *)*)

    (* this is systematically causing an overflow in MLton, but works fine in SML/NJ: *)
    (* val seed1 = (Random.randInt r)  (* SML/NJ: this is a new random number in a wide range with every call: int *) *)

    (* 2026-10-24: retired: val start_seed = Random.randRange (1, m - 1) r  (* ending point is inclusive; 2025-12-21 *) *)
    (* https://github.com/smlnj/legacy/blob/main/smlnj-lib/Util/real-order-stats.sml#L40 *)


    val _ = print ("\ngenerating a random bit stream...")

    (**********************  recursive master loop  ****************************)
    fun masterloop (n : int, seed : int) =
      let
        fun loop (i, seed) =
          let
            (*val _ = print ("\ni = " ^ Int.toString i ^ "\n")  (* for testing *)*)

            val new_seed = (a * seed + c) mod m
            (*val _ = print ("  new_seed = " ^ Int.toString new_seed ^ "\n")  (* for testing *)*)

            val _ = Array.update(x, i, new_seed)

            val bits_x_str   = integer_to_bin_string new_seed
            val bits_hex_str = integer_to_hex_string new_seed
            (*val _ = print ("  bits_x_str = " ^ bits_x_str ^ "\n")      (* for testing *)
            val _ = print ("  bits_hex_str = " ^ bits_hex_str ^ "\n")  (* for testing *)*)

            val _ = Array.update(bits_x, i, bits_x_str)
            val _ = Array.update(bits_hex, i, bits_hex_str)

          in
            if i < END then
              loop ((i+1), new_seed) (* recursion *)
            else
              ()
          end
      in
        loop (0, start_seed)
      end

    val _ = masterloop (0, start_seed)


    val bits_x_rev = Array.foldr (fn (x, acc) => x :: acc) [] bits_x  (* new array with elements in reverse order *)
    val bits_x_str_total = String.concat bits_x_rev
    val bits_hex_rev = Array.foldr (fn (x, acc) => x :: acc) [] bits_hex  (* new array with elements in reverse order *)
    val bits_hex_str_total = String.concat bits_hex_rev

    (*val _ = printIntArray x (* for testing *)
    val _ = print ("bits_x_str_total = "   ^ bits_x_str_total ^ "\n")           (* for testing *)
    val _ = print ("bits_hex_str_total = " ^ bits_hex_str_total ^ "\n")         (* for testing *)*)

    (* write bit stream to disk *)
    val _ = write_to_file (file_bits_x, bits_x_str_total, "bit")

    (* write byte stream to disk *)
    val _ = write_to_file (file_bits_hex, bits_hex_str_total, "byte")


    val _ = print ("\n")

  end


(* end of random_streams_for_perf_stats.sml *)
