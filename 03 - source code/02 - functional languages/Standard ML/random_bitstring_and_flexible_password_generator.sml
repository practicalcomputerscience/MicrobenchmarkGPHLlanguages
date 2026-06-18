(*
random_bitstring_and_flexible_password_generator.sml -- for MLton Standard ML

!!! new block comment rules for better counting of SLOC:
    - put markers to start and end a block comment only into extra solo lines
    - don't put nested comments with a marker at the end of a line inside block comments !!!

2025-07-07/08
2025-12-21: see below
2026-01-04: cosmetics
2026-05-29/30: refactored from char_set to pattern (for regular expressions)
2026-06-18: define print_re and alnum_re, use the ternary operator with with_special_chars to set pattern


build on Ubuntu 24 LTS: take mlton-20241230.x86_64-linux-gnu.tar.gz from: https://github.com/ii8/mlton-builds/releases/tag/20241230
                        unzip it and put path to: ./scripts/StandardML/mlton-20241230.x86_64-linux-gnu/mlton-on-20241230-release.x86_64-linux-gnu/bin/
                        $ mlton -verbose 1 ./random_bitstring_and_flexible_password_generator.mlb

------
random_bitstring_and_flexible_password_generator.mlb:
$(SML_LIB)/smlnj-lib/Util/smlnj-lib.mlb
$(SML_LIB)/smlnj-lib/RegExp/regexp-lib.mlb
$(SML_LIB)/basis/basis.mlb
./random_bitstring_and_flexible_password_generator.sml
------


run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator

$ mlton
MLton 20241230.203305-gb15e2d2
$

*)


(**********************************************************)
(* user defined functions                                 *)

(*
help function, not in use now:
fun listToString [] = "[]"
    | listToString (x::xs) = "[" ^ elemToString x ^ restToString xs
  and restToString [] = "]"
    | restToString (x::xs) = ", " ^ elemToString x ^ restToString xs
  and elemToString x = Int.toString x
*)

(* Function to print an array of integers; only needed for testing *)
(* 1:1 solution from MS Bing AI *)
(*
fun printIntArray arr =
    let
        val elements = Array.foldr (fn (x, acc) => Int.toString x :: acc) [] arr
        val result = String.concatWith ", " elements
    in
        print ("\n[" ^ result ^ "]\n")
    end
*)

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


fun isAllDigits str =
    let
      val substring = Substring.full str
      (*
         see: https://smlfamily.github.io/Basis/substring.html#SIG:SUBSTRING.foldl:VAL
         we need a substring here first: val full : string -> substring
      *)
    in
      Substring.foldl (fn (ch, acc) => Char.isDigit ch andalso acc) true substring
    end

fun input_a_valid_number (n_char: int) =
  (print ("\nPassword of " ^ Int.toString n_char ^ " printable chars OK? 'y' or another integer number >= 8: "); (*;!!!*)
   let
     val answer_str_ = valOf (TextIO.inputLine TextIO.stdIn)  (* returns a string option; \n is part of answer_str_ *)
     val answer_str  = String.substring (answer_str_, 0, (String.size answer_str_) - 1)
   in
     (* print ("answer_str =" ^ answer_str ^ "--\n"); (* for testing *) *)

     if answer_str = "y" then
       n_char
     else
       if isAllDigits answer_str then
         let
           val n_char_ = valOf (Int.fromString answer_str)
         in
           if n_char_ >= 8 then
             n_char_
           else
             (print ("enter an integer number >= 8 or 'y'\n");
              input_a_valid_number n_char)
         end

       else
         (print ("enter an integer number >= 8 or 'y'\n");
                  input_a_valid_number n_char)
   end)


fun answer_yes_or_no () =
  (print ("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': "); (*;!!!*)
   let
     val answer_str_ = valOf (TextIO.inputLine TextIO.stdIn)  (* returns a string option; \n is part of answer_str_ *)
     val answer_str  = String.substring (answer_str_, 0, (String.size answer_str_) - 1)
   in
     if answer_str = "y" then
       true
     else
       false
   end)


(*
2026-05-29: helper for the old solution:
fun char_range (start_char: int, end_char: int) =
  if start_char > end_char then
    ""
  else
    Char.toString (Char.chr start_char) ^ char_range (start_char + 1, end_char);
*)


fun bin_string_to_integer binStr =
  let
    fun parseBinary [] acc = acc
      | parseBinary (c::cs) acc =
          if c = #"0" then parseBinary cs (2 * acc)
          else
            if c = #"1" then
              parseBinary cs (2 * acc + 1)
            else
              0
  in
      parseBinary (String.explode binStr) 0
  end;

(* end of user defined functions                          *)
(**********************************************************)


structure Main =
  struct
    val END = 62499  (* 62499 for exactly 1M binary digits; variable END is immutable *)
    (*val END = 25  (* for testing *)*)

    val m = 65521  (* = 2^16 - 15 *)
    val m_word = 0wx0000fff1  (* same number as hex word *)
    val m_word_ = m_word - 0wx00000001  (* seed adaption; 2025-12-21 *)

    val a = 17364
    val c = 0

    val file_bits_x   = "random_bitstring.bin"
    val file_bits_hex = "random_bitstring.byte"


    val x = Array.array(END+1, 0)
    val bits_xx = Array.array(END+1, "0000000000000000")
    val bits_hexx = Array.array(END+1, "0000")

    (*val xx = MLton.Vector.create (END+1) (* OK *)*)

    (* copied from random_streams_for_perf_stats3.sml *)
    fun getTime () = IntInf.divMod (Time.toMicroseconds(Time.now()), 1000000)
    val maxInt = IntInf.fromInt (valOf Int.maxInt) + 1
    val (secs, usecs) = getTime ()
    val r = Random.rand (Int.fromLarge(secs mod maxInt), Int.fromLarge usecs)  (* rand of type Random.rand *)
    val start_seed = Random.randRange (1, m - 1) r  (* ending point is inclusive; 2025-12-21 *)


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
            (* val _ = print ("  bits_x_str = " ^ bits_x_str ^ "\n")      (* for testing *) *)
            (* val _ = print ("  bits_hex_str = " ^ bits_hex_str ^ "\n")  (* for testing *) *)

            val _ = Array.update(bits_xx, i, bits_x_str)
            val _ = Array.update(bits_hexx, i, bits_hex_str)

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


    val bits_xx_el = Array.foldr (fn (x, acc) => x :: acc) [] bits_xx
    val bits_x = String.concat bits_xx_el
    val bits_hexx_el = Array.foldr (fn (x, acc) => x :: acc) [] bits_hexx
    val bits_hex = String.concat bits_hexx_el

    (*
      val _ = printIntArray x
      val _ = print ("bits_x = "   ^ bits_x ^ "\n")
      val _ = print ("bits_hex = " ^ bits_hex ^ "\n")
    *)

    (* write bit stream to disk *)
    val _ = write_to_file (file_bits_x, bits_x, "bit")

    (* write byte stream to disk *)
    val _ = write_to_file (file_bits_hex, bits_hex, "byte")
    val _ = print ("\n")


    val n_char_default = 12
    val n_char = input_a_valid_number n_char_default
    (* val _ = print ("main n_char = "   ^ Int.toString n_char)  (* for testing *)*)

    val with_special_chars = answer_yes_or_no ()
    (*val _ = print ("main with_special_chars = "   ^ Bool.toString with_special_chars)  (* for testing *)*)

    (*
    2026-05-29: old solution:
    val char_set =
      if with_special_chars then
        char_range (33, 126)
      else
        char_range (48, 57) ^
        char_range (65, 90) ^
        char_range (97, 122)
    *)
    (*val _ = print ("char_set = "   ^ char_set)  (* for testing *)*)

    (* 2026-05-29: new solution with regular expressions; Google AI *)
    (* Define the Regex structure: *)
    structure Regex = RegExpFn (
      structure P = AwkSyntax        (* P maps to the REGEXP_PARSER *)
      structure E = BackTrackEngine  (* E maps to the REGEXP_ENGINE *)
    )
    (*
       The SML of NJ Library: The AwkSyntax structure
       https://www.smlnj.org/doc/smlnj-lib/RegExp/str-AwkSyntax.html
       https://www.smlnj.org/doc/smlnj-lib/RegExp/regexp-lib.html
    *)
    (* 2026-06-18 *)
    val print_re = "^[!-~]$"
    val alnum_re = "^[A-Za-z0-9]$"
    val pattern = Regex.compileString (if with_special_chars then print_re else alnum_re);


    (**********************  recursive password loop  **********************)

    (*  similar to the  Racket solution *)
    fun pw_generator (n: int) =
      let
        fun loop (j: int, pw_str: string) =  (* j: counter for x *)
          let
            val bin0 = integer_to_bin_string (Array.sub (x, j))
            (*val _ = print ("\n\nbin0 = " ^ bin0) (* for testing *)*)

            val bin0_0 = String.substring (bin0, 0, 8)
            val bin0_1 = String.substring (bin0, 8, 8)
            (*val _ = print ("\nbin0_0 = " ^ bin0_0 ^ " -- bin0_1 = " ^ bin0_1) (* for testing *)*)

            val char0 = bin_string_to_integer bin0_0
            val char1 = bin_string_to_integer bin0_1
            (*val _ = print ("\nchar0 = " ^ Int.toString char0 ^ " -- char1 = " ^ Int.toString char1) (* for testing *)*)

            val char0a = Char.toString (Char.chr char0)
            val char1a = Char.toString (Char.chr char1)
            (*val _ = print ("\nchar0a = " ^ char0a ^ " -- char1 = " ^ char1a) (* for testing *)*)


            (* 2026-05-29: new solution with regular expressions *)
            val char0_add =
              if Option.isSome (Regex.find pattern Substring.getc (Substring.full char0a)) then
                char0a
              else
                ""

            val char1_add =
              if Option.isSome (Regex.find pattern Substring.getc (Substring.full char1a)) andalso ((String.size pw_str)+1) < n then
                char1a
              else
                ""
            (*val _ = print ("\nchar0_add = " ^ char0_add ^ " -- char1_add = " ^ char1_add) (* for testing *)*)

            val new_pw_str  = pw_str ^ char0_add ^ char1_add
            val new_pw_size = String.size new_pw_str

          in
            if new_pw_size >= n then
              new_pw_str
            else
              loop ((j+1), new_pw_str) (* recursion *)
          end
      in
        loop (0, "")
      end

    val pw_chars = pw_generator n_char

    val _ = print ("\nYour password of " ^ Int.toString n_char ^ " characters is: " ^ pw_chars ^ "\n")

  end

(* end of random_bitstring_and_flexible_password_generator.sml *)
