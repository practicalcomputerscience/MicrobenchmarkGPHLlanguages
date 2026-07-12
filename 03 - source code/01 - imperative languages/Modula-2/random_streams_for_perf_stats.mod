(*
random_streams_for_perf_stats.mod

This program is for GNU Modula-2 (ISO).

2026-07-09/10/12

build on Ubuntu 24 LTS: $ gm2 -fiso random_streams_for_perf_stats.mod -o random_streams_for_perf_stats  # for development
                        $ gm2 -O3 -Wall -fiso random_streams_for_perf_stats.mod -o random_streams_for_perf_stats  # for production
                        #               -fiso to turn on ISO standard features

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ time ./random_streams_for_perf_stats => real	0m0.006s


With this algorithm, also optimization switch -O3 is very effective and safe to use!
DynamicStrings are bad for fast Modula-2 programs.


$ gm2 --version
gm2 (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
Copyright (C) 2023 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

$

*)


MODULE random_streams_for_perf_stats;

FROM STextIO IMPORT WriteString, WriteLn;
(* FROM SWholeIO IMPORT WriteCard; *) (* for testing *)
FROM RandomNumber IMPORT RandomInt;
FROM StreamFile IMPORT Open, Close;
FROM ChanConsts IMPORT OpenResults;
FROM RndFile IMPORT ChanId, write, raw; (* 'write' flag is required for StreamFile.Open *)
FROM RawIO IMPORT Write;                (* Used for fast, raw block memory writing *)


CONST
  upper_limit = 62500;  (* 62500 for exactly 1M binary digits *)
  (*upper_limit = 10;*) (*for testing*)

  M1          = upper_limit * 16;
  K250        = upper_limit * 4;

  m           = 65521;  (* = 2^16 - 15 *)
  a           = 17364;
  c           = 0;

  file_bits_x   = "random_bitstring.bin";
  file_bits_hex = "random_bitstring.byte";

VAR
  x : ARRAY [0..upper_limit-1] OF CARDINAL;

  i, h, byte_nbr: CARDINAL;  (* 2026-07-12: one variable byte_nbr is enough *)

  bits_x_str   : ARRAY [0..15] OF CHAR;
  bits_hex_str : ARRAY [0..3] OF CHAR;

  bits_x     : ARRAY [0..M1-1] OF CHAR; (* M1-1 for no last 0C, "NULL", character *)
  bits_hex   : ARRAY [0..K250-1] OF CHAR;

  fileChan   : ChanId;
  openRes    : OpenResults;


(**********************************************************)
(* user defined functions                                 *)

(* Integer_to_bin_string + Integer_to_hex_string are transpilations
   from Ada by Google AI *)
PROCEDURE Integer_to_bin_string (n : CARDINAL; VAR binStr : ARRAY OF CHAR);
VAR
  j : INTEGER;
  k : CARDINAL;
BEGIN
  (* 1. Safely initialize the unbounded array with default '0' padding
        HIGH(binStr) returns the maximum available index of the passed array *)
  FOR j := 0 TO VAL(INTEGER, HIGH(binStr)) DO
    binStr[j] := '0';
  END;

  (* 2. Set the initial index tracker.
        In Modula-2, arrays are 0-indexed, so the 16th character is at index 15. *)
  j := 15;
  k := n;

  (* 3. Process the bit segments sequentially *)
  WHILE (k > 0) AND (j >= 0) DO
    IF (k MOD 2) > 0 THEN
      binStr[j] := '1';
    END;
    k := k / 2;
    DEC(j);
  END;
END Integer_to_bin_string;


PROCEDURE Integer_to_hex_string (n : CARDINAL; VAR hexStr : ARRAY OF CHAR);
VAR
  j         : INTEGER;
  k         : CARDINAL;
  remainder : CARDINAL;
BEGIN
  (* 1. Safely initialize the unbounded array with default '0' padding
        HIGH(binStr) returns the maximum available index of the passed array *)
  FOR j := 0 TO VAL(INTEGER, HIGH(hexStr)) DO
    hexStr[j] := '0';
  END;

  (* 2. Set the initial index tracker.
        In Modula-2, 0-indexing means the 4th character is at index 3. *)
  j := 3;
  k := n;

  (* 3. Process the hexadecimal segments sequentially *)
  WHILE (k > 0) AND (j >= 0) DO
    remainder := k MOD 16;

    CASE remainder OF
        0  : hexStr[j] := '0';
      | 1  : hexStr[j] := '1';
      | 2  : hexStr[j] := '2';
      | 3  : hexStr[j] := '3';
      | 4  : hexStr[j] := '4';
      | 5  : hexStr[j] := '5';
      | 6  : hexStr[j] := '6';
      | 7  : hexStr[j] := '7';
      | 8  : hexStr[j] := '8';
      | 9  : hexStr[j] := '9';
      | 10 : hexStr[j] := 'a';
      | 11 : hexStr[j] := 'b';
      | 12 : hexStr[j] := 'c';
      | 13 : hexStr[j] := 'd';
      | 14 : hexStr[j] := 'e';
      | 15 : hexStr[j] := 'f';
    ELSE
      (* Corresponds to 'when others => null' in Ada *)
    END;

    k := k / 16;
    DEC(j);
  END;
END Integer_to_hex_string;

(* end of user defined functions                          *)
(**********************************************************)


BEGIN
  x[0] := RandomInt(1, m - 1);

  WriteLn; WriteString("generating a random bit stream..."); WriteLn;

  FOR i := 1 TO upper_limit DO
    x[i] := (a * x[i - 1] + c) MOD m;
    (* WriteLn; WriteLn; WriteCard(x[i], 0); *)  (* for testing *)

    Integer_to_bin_string(x[i], bits_x_str);
    (* WriteLn; WriteString(bits_x_str); *)  (* for testing *)
    byte_nbr := (i - 1) * 16;
    FOR h := 0 TO 15 DO
      bits_x[byte_nbr + h] := bits_x_str[h];
    END;

    Integer_to_hex_string(x[i], bits_hex_str);
    (* WriteLn; WriteString(bits_hex_str); *)  (* for testing *)
    byte_nbr := (i - 1) * 4;
    FOR h := 0 TO 3 DO
      bits_hex[byte_nbr + h] := bits_hex_str[h];
    END;
  END;

  (* WriteLn; WriteLn; WriteString(bits_x); *)  (* for testing *)
  (* WriteLn; WriteLn; WriteString(bits_hex); *)  (* for testing *)


  Open(fileChan, file_bits_x, write + raw, openRes);
  IF openRes = opened THEN
    Write(fileChan, bits_x);
    Close(fileChan);

    WriteString("Bit stream has been written to disk under name:  ");
    WriteString(file_bits_x); WriteLn;
  ELSE
    WriteString("could not write to file: "); WriteString(file_bits_x);
    WriteString(" !"); WriteLn;
  END;

  Open(fileChan, file_bits_hex, write + raw, openRes);
  IF openRes = opened THEN
    Write(fileChan, bits_hex);
    Close(fileChan);

    WriteString("Byte stream has been written to disk under name: ");
    WriteString(file_bits_hex); WriteLn;
  ELSE
    WriteString("could not write to file: "); WriteString(file_bits_hex);
    WriteString(" !"); WriteLn;
  END;

END random_streams_for_perf_stats.
