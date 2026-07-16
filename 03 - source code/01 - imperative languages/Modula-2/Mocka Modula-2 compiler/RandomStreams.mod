(*
RandomStreams.mod
!! no _ character allowed (underline) !!
!! RandomStreamsForPerfStats is too long !!


This program is for the Mocka Modula-2 compiler for 32-bit Linux.

2026-07-16


OS: xubuntu-18.04.5-desktop-i386 as a virtual machine (Oracle VirtualBox) on a Ubuntu 24 LTS host (Intel Core Ultra 7 270K Plus)


build on Ubuntu 24 LTS: $ echo "p RandomStreams" | mocka

run on Ubuntu 24 LTS:   $ ./RandomStreams
                        $ time ./RandomStreams => real  0m0,026s
                        $ multitime -n 10 ./RandomStreams
                        1: ./RandomStreams
                                    Mean        Std.Dev.    Min         Median      Max
                        real        0.025       0.001       0.021       0.025       0.026


Compiler version: Mocka 0608m

*)


MODULE RandomStreams;

IMPORT InOut, SysLib, TextIO;

CONST
  upper_limit = 62501;  (* 62501 for exactly 1M binary digits *)
  (*upper_limit = 10;*) (*for testing*)

  M1       =  upper_limit * 16;
  K250     =  upper_limit * 4;

  m        = 65521;  (* = 2^16 - 15 *)
  a        = 17364;
  c        = 0;

  file_bits_x   = "random_bitstring.bin";
  file_bits_hex = "random_bitstring.byte";


VAR
  x : ARRAY [0..upper_limit-1] OF CARDINAL;

  i, h, byte_nbr : CARDINAL;

  t, file1     : INTEGER;

  bits_x_str   : ARRAY [0..15] OF CHAR;
  bits_hex_str : ARRAY [0..3] OF CHAR;

  bits_x     : ARRAY [0..M1-1] OF CHAR; (* M1-1 for no last 0C, "NULL", character *)
  bits_hex   : ARRAY [0..K250-1] OF CHAR;



(**********************************************************)
(* user defined functions                                 *)

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
    k := k DIV 2;  (* forward slash / is exclusively used for REAL division *)
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

    k := k DIV 16;  (* 2026-07-16: forward slash / is exclusively used for REAL division *)
    DEC(j);
  END;
END Integer_to_hex_string;


(* end of user defined functions                          *)
(**********************************************************)


BEGIN
  SysLib.time (t);
  (*InOut.WriteLn; InOut.WriteCard(t, 0);*)  (* for testing *)
  (* t is the time in seconds since the epoch, specifically January 1, 1970 *)

  x[0] := CARDINAL(t) MOD (m - 2) + 1;
  (*InOut.WriteLn; InOut.WriteCard(x[0], 0);*)  (* for testing *)

  InOut.WriteLn; InOut.WriteString("generating a random bit stream...");
  InOut.WriteLn;

  FOR i := 1 TO upper_limit - 1 DO
    x[i] := (a * x[i - 1] + c) MOD m;
    (*InOut.WriteLn; InOut.WriteLn; InOut.WriteCard(x[i], 0);*)  (* for testing *)

    Integer_to_bin_string(x[i], bits_x_str);
    (*InOut.WriteLn; InOut.WriteString(bits_x_str);*)   (* for testing *)
    byte_nbr := (i - 1) * 16;
    FOR h := 0 TO 15 DO
      bits_x[byte_nbr + h] := bits_x_str[h];
    END;

    Integer_to_hex_string(x[i], bits_hex_str);
    (*InOut.WriteLn; InOut.WriteString(bits_hex_str);*)  (* for testing *)
    byte_nbr := (i - 1) * 4;
    FOR h := 0 TO 3 DO
      bits_hex[byte_nbr + h] := bits_hex_str[h];
    END;
  END;
  (*InOut.WriteLn;*)

  (*InOut.WriteLn; InOut.WriteLn; InOut.WriteString(bits_x);*)  (* for testing *)
  (*InOut.WriteLn; InOut.WriteLn; InOut.WriteString(bits_hex);*)  (* for testing *)

  TextIO.OpenOutput(file1, file_bits_x);
  IF TextIO.Done() THEN
    TextIO.PutString(file1, bits_x);
    TextIO.Close(file1);
    InOut.WriteString("Bit stream has been written to disk under name:  ");
    InOut.WriteString(file_bits_x); InOut.WriteLn;
  ELSE
    InOut.WriteString("could not write to file: "); InOut.WriteString(file_bits_x);
    InOut.WriteString(" !"); InOut.WriteLn;
  END;

  TextIO.OpenOutput(file1, file_bits_hex);
  IF TextIO.Done() THEN
    TextIO.PutString(file1, bits_hex);
    TextIO.Close(file1);
    InOut.WriteString("Byte stream has been written to disk under name: ");
    InOut.WriteString(file_bits_hex); InOut.WriteLn;
  ELSE
    InOut.WriteString("could not write to file: "); InOut.WriteString(file_bits_hex);
    InOut.WriteString(" !"); InOut.WriteLn;
  END;

END RandomStreams.
