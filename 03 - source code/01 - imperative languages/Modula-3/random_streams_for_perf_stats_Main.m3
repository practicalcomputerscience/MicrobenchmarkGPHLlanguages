(*
random_streams_for_perf_stats.m3 is Main.cm3

2026-07-09/12

build on Ubuntu 24 LTS: make this only once:
                        $ mkdir random_streams_for_perf_stats
                        $ cd random_streams_for_perf_stats
                        # get the m3makefile ready in same project dir
                        
                        make this after every source code change:
                        $ cm3

run on Ubuntu 24 LTS:   $ ./AMD64_LINUX/random_streams_for_perf_stats
                        $ time ./AMD64_LINUX/random_streams_for_perf_stats => real	0m0.082s


------------------------------------
m3makefile:
import("libm3")
implementation("Main")
program("random_streams_for_perf_stats")
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


mostly transpiled from random_streams_for_perf_stats.groovy with Google AI

*)


MODULE Main;

IMPORT Random, Fmt, Wr, FileWr, Thread, TextWr, Stdio, OSError;

CONST
  upper_limit = 62501;  (* 62501 for exactly 1M binary digits *)
  m           = 65521;  (* = 2^16 - 15 *)
  a           = 17364;
  c           = 0;

VAR
  file_bits_x   : TEXT := "random_bitstring.bin";
  file_bits_hex : TEXT := "random_bitstring.byte";
  
  x           : ARRAY [0 .. upper_limit-1] OF INTEGER;
  rnd         : Random.T;
  
  bits_x, bits_hex : TextWr.T;
  
  bits_x_str, bits_hex_str : TEXT;
  
  outWr       : Wr.T;


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

    (* write bit stream to disk *)
    TRY
      outWr := FileWr.Open(file_bits_x);
      Wr.PutText(outWr, TextWr.ToText(bits_x));
      Wr.Close(outWr);
      Wr.PutText(Stdio.stdout, "Bit stream has been written to disk under name:  " & file_bits_x & "\n");
    EXCEPT
      | Wr.Failure, Thread.Alerted, OSError.E => 
          Wr.PutText(Stdio.stdout, "could not write to file: " & file_bits_x & " !\n");
    END;

    (* write byte stream to disk *)
    TRY
      outWr := FileWr.Open(file_bits_hex);
      Wr.PutText(outWr, TextWr.ToText(bits_hex));
      Wr.Close(outWr);
      Wr.PutText(Stdio.stdout, "Byte stream has been written to disk under name: " & file_bits_hex & "\n");
    EXCEPT
      | Wr.Failure, Thread.Alerted, OSError.E => 
          Wr.PutText(Stdio.stdout, "could not write to file: " & file_bits_hex & " !\n");
    END;

  EXCEPT
    ELSE
  END;

END Main.

(* end of random_streams_for_perf_stats.m3 *)
