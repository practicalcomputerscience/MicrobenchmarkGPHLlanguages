(*
random_bitstring_and_flexible_password_generator.m3 is Main.cm3

2026-07-12

build on Ubuntu 24 LTS: make this only once:
                        $ mkdir random_bitstring_and_flexible_password_generator
                        $ cd random_bitstring_and_flexible_password_generator
                        # get the m3makefile ready in same project dir

                        make this after every source code change:
                        $ cm3

run on Ubuntu 24 LTS:   $ ./AMD64_LINUX/random_bitstring_and_flexible_password_generator


------------------------------------
m3makefile:
import("libm3")
implementation("Main")
program("random_bitstring_and_flexible_password_generator")
------------------------------------


$ cm3 --version
Critical Mass Modula-3 version d5.11.4
  GitInfo: unknown
  last updated: 2021-10-07
  compiled: 2026-07- 9 22:12:03
  configuration: ~/cm3/bin/cm3.cfg
  host: AMD64_LINUX
  target: AMD64_LINUX

$


*)


MODULE Main;

IMPORT Random, IO, Fmt, Wr, FileWr, Thread, TextWr;
IMPORT Stdio, OSError, Lex, Text, Scan, FloatMode, RegEx;



CONST
  upper_limit = 62501;  (* 62501 for exactly 1M binary digits *)
  (*upper_limit = 50;*)  (* for testing *)

  m           = 65521;  (* = 2^16 - 15 *)
  a           = 17364;
  c           = 0;

VAR
  file_bits_x   : TEXT := "random_bitstring.bin";
  file_bits_hex : TEXT := "random_bitstring.byte";

  x           : ARRAY [0 .. upper_limit-1] OF INTEGER;
  rnd         : Random.T;

  bits_x, bits_hex, pw_chars : TextWr.T;

  bits_x_str, bits_hex_str, answer_str, password : TEXT;
  bin0, bin0_0, bin0_1, char0b, char1b : TEXT;

  outWr        : Wr.T;

  N_CHAR       : INTEGER := 12;

  i, j, char0a, char1a : INTEGER;

  answer       : BOOLEAN := FALSE;
  WITH_SPECIAL_CHARS : BOOLEAN := TRUE;

  pattern      : RegEx.Pattern;


BEGIN
  TRY
    rnd := NEW(Random.Default).init();
    x[0] := rnd.integer(0, m - 2) + 1;

    bits_x := TextWr.New();
    bits_hex := TextWr.New();

    Wr.PutText(Stdio.stdout, "\ngenerating a random bit stream...\n");

    FOR i := 1 TO upper_limit - 1 DO
      x[i] := (a * x[i - 1] + c) MOD m;

      bits_x_str   := Fmt.Pad(Fmt.Int(x[i],  2), 16, '0', Fmt.Align.Right);
      Wr.PutText(bits_x, bits_x_str);

      bits_hex_str := Fmt.Pad(Fmt.Int(x[i], 16),  4, '0', Fmt.Align.Right);
      Wr.PutText(bits_hex, bits_hex_str);
    END;


    TRY
      outWr := FileWr.Open(file_bits_x);
      Wr.PutText(outWr, TextWr.ToText(bits_x));
      Wr.Close(outWr);
      Wr.PutText(Stdio.stdout, "Bit stream has been written to disk under name:  " & file_bits_x & "\n");
    EXCEPT
      | Wr.Failure, Thread.Alerted, OSError.E =>
          Wr.PutText(Stdio.stdout, "could not write to file: " & file_bits_x & " !\n");
    END;

    TRY
      outWr := FileWr.Open(file_bits_hex);
      Wr.PutText(outWr, TextWr.ToText(bits_hex));
      Wr.Close(outWr);
      Wr.PutText(Stdio.stdout, "Byte stream has been written to disk under name: " & file_bits_hex & "\n");
    EXCEPT
      | Wr.Failure, Thread.Alerted, OSError.E =>
          Wr.PutText(Stdio.stdout, "could not write to file: " & file_bits_hex & " !\n");
    END;


    (* make a password of N_CHAR printable chars: user input requested here *)
    WHILE NOT answer DO
      N_CHAR := 12;
      IO.Put("\nPassword of " & Fmt.Int(N_CHAR) & " printable chars OK? 'y' or another integer number >= 8: ");
      answer_str := IO.GetLine();

      IF Text.Equal(answer_str, "y") THEN
        answer := TRUE;
      ELSE
        TRY
          N_CHAR := Scan.Int(answer_str);
          IF (N_CHAR < 8) THEN
            IO.Put("enter an integer number >= 8 or 'y'\n")
          ELSE
            answer := TRUE;
          END;
        EXCEPT
          | Lex.Error, FloatMode.Trap =>
            IO.Put("enter an integer number >= 8 or 'y'\n")
        END;
      END;
    END;

    answer := FALSE;
    WHILE NOT answer DO
      IO.Put("\nDo you want me to use special characters like .;,+*... ? 'y' or 'n': ");
      answer_str := IO.GetLine();

      IF Text.Equal(answer_str, "y") THEN
        answer := TRUE;
      ELSE
        WITH_SPECIAL_CHARS := FALSE;
        answer := TRUE;
      END;
    END;

    IF WITH_SPECIAL_CHARS THEN
      pattern := RegEx.Compile("[!-~]+");
    ELSE
      pattern := RegEx.Compile("[A-Za-z0-9]+");
    END;


    pw_chars := TextWr.New();
    i := 0;  (* char counter for the password *)
    j := 0;  (* counter for x *)

    WHILE (i < N_CHAR) DO
      bin0 := Fmt.Pad(Fmt.Int(x[j],  2), 16, '0', Fmt.Align.Right);
      (*IO.Put("\n\n" & bin0);*)  (* for testing *)

      bin0_0 := Text.Sub(bin0, 0,  8);
      bin0_1 := Text.Sub(bin0, 8, 16);

      char0a := Scan.Int(bin0_0, 2);
      char1a := Scan.Int(bin0_1, 2);

      char0b := Text.FromChar(VAL(char0a, CHAR));
      char1b := Text.FromChar(VAL(char1a, CHAR));
      (*IO.Put("\n" & char0b & " -- " & char1b);*) (* for testing *)

      IF RegEx.Execute(pattern, char0b) >= 0 THEN
          Wr.PutText(pw_chars, char0b);
          INC(i);
      END;

      IF (RegEx.Execute(pattern, char1b) >= 0) AND (i < N_CHAR) THEN
          Wr.PutText(pw_chars, char1b);
          INC(i);
      END;

      INC(j);
    END;

    password := TextWr.ToText(pw_chars);
    IO.Put("\nYour password of " & Fmt.Int(N_CHAR) & " characters is: " & password & "\n");

  EXCEPT
    ELSE
  END;

END Main.

(* end of random_bitstring_and_flexible_password_generator.m3 *)
