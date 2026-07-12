(*
random_bitstring_and_flexible_password_generator.mod

This program is for GNU Modula-2 (ISO).

!!! new block comment rules for better counting of SLOC:
    - put markers to start and end a block comment only into extra solo lines
    - don't put nested comments with a marker at the end of a line inside block comments !!!


2026-07-11/12


build on Ubuntu 24 LTS: $ gm2 -fiso random_bitstring_and_flexible_password_generator.mod -o random_bitstring_and_flexible_password_generator  # for development
                        $ gm2 -O3 -Wall -fiso random_bitstring_and_flexible_password_generator.mod -o random_bitstring_and_flexible_password_generator  # for production
                        #               -fiso to turn on ISO standard features

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator


With this algorithm, also optimization switch -O3 is very effective and safe to use!
DynamicStrings are bad for fast Modula-2 programs.


$ gm2 --version
gm2 (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0
Copyright (C) 2023 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

$

*)


MODULE random_bitstring_and_flexible_password_generator;

FROM STextIO IMPORT WriteString, WriteLn, ReadString, SkipLine, WriteChar;
FROM SWholeIO IMPORT WriteCard, WriteInt;
FROM RandomNumber IMPORT RandomInt;
FROM StreamFile IMPORT Open, Close;
FROM ChanConsts IMPORT OpenResults;
FROM RndFile IMPORT ChanId, write, raw; (* 'write' flag is required for StreamFile.Open *)
FROM RawIO IMPORT Write;                (* Used for fast, raw block memory writing *)
FROM Strings IMPORT Equal, Append, Extract, FindNext;
FROM ConvTypes IMPORT ConvResults, strAllRight;
FROM WholeStr IMPORT StrToInt, StrToCard;


CONST
  upper_limit = 62500;  (* 62500 for exactly 1M binary digits *)
  (*upper_limit = 50;*) (*for testing*)

  M1          = upper_limit * 16;
  K250        = upper_limit * 4;

  m           = 65521;  (* = 2^16 - 15 *)
  a           = 17364;
  c           = 0;

  file_bits_x   = "random_bitstring.bin";
  file_bits_hex = "random_bitstring.byte";

VAR
  x : ARRAY [0..upper_limit-1] OF CARDINAL;

  h, i, j, byte_nbr, char0a, char1a, pos : CARDINAL;

  char0b, char1b : ARRAY [0..1] OF CHAR;

  bits_x_str   : ARRAY [0..15] OF CHAR;
  bits_hex_str : ARRAY [0..3] OF CHAR;

  bits_x     : ARRAY [0..M1-1] OF CHAR; (* M1-1 for no last 0C, "NULL", character *)
  bits_hex   : ARRAY [0..K250-1] OF CHAR;

  fileChan   : ChanId;
  openRes    : OpenResults;

  N_CHAR, i_ : INTEGER;
  answer, WITH_SPECIAL_CHARS, char_found : BOOLEAN;

  answer_str : ARRAY [0..255] OF CHAR;  (* do like C solution *)

  res        : ConvResults;

  char_set   : ARRAY [0..94] OF CHAR;

  bin0       : ARRAY [0..15] OF CHAR;
  bin0_0     : ARRAY [0..7] OF CHAR;
  bin0_1     : ARRAY [0..7] OF CHAR;

  pw_chars   : ARRAY [0..99] OF CHAR;


(**********************************************************)
(* user defined functions                                 *)

(* Integer_to_bin_string + Integer_to_hex_string are transpilations *)
(* from Ada by Google AI                                            *)
PROCEDURE Integer_to_bin_string (n : CARDINAL; VAR binStr : ARRAY OF CHAR);
VAR
  j : INTEGER;
  k : CARDINAL;
BEGIN
  (* 1. Safely initialize the unbounded array with default '0' padding       *)
  (*    HIGH(binStr) returns the maximum available index of the passed array *)
  FOR j := 0 TO VAL(INTEGER, HIGH(binStr)) DO
    binStr[j] := '0';
  END;

  (* 2. Set the initial index tracker. *)
  (*   In Modula-2, arrays are 0-indexed, so the 16th character is at index 15. *)
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
  (* 1. Safely initialize the unbounded array with default '0' padding *)
  (*    HIGH(binStr) returns the maximum available index of the passed array *)
  FOR j := 0 TO VAL(INTEGER, HIGH(hexStr)) DO
    hexStr[j] := '0';
  END;

  (* 2. Set the initial index tracker. *)
  (*    In Modula-2, 0-indexing means the 4th character is at index 3. *)
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


(* Google AI *)
PROCEDURE Is_all_digits (str: ARRAY OF CHAR) : BOOLEAN;
VAR
  i : CARDINAL;
BEGIN
  i := 0;

  (* 1. Ensure the string is not completely empty *)
  IF str[0] = "" THEN
    RETURN FALSE;
  END;

  (* 2. Check every single character up to the null terminator *)
  WHILE str[i] <> "" DO
    IF (str[i] < "0") OR (str[i] > "9") THEN
      RETURN FALSE; (* Instantly rejects spaces, signs, and trailing garbage *)
    END;
    INC(i);
  END;

  (* Every character in the string is a valid decimal digit *)
  RETURN TRUE;
END Is_all_digits;


(* transpiled with Duck.ai from Chapel procedure bin_string_to_int *)
PROCEDURE Bin_string_to_int (s: ARRAY OF CHAR): CARDINAL;
VAR
  i, n: CARDINAL;
BEGIN
  n := 0;
  i := 0;
  WHILE (i <= HIGH(s)) AND (s[i] # 0C) DO
    n := n * 2;
    IF s[i] = '1' THEN INC(n) END;
    INC(i)
  END;
  RETURN n
END Bin_string_to_int;


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


  (* make a password of N_CHAR printable chars: user input requested here *)
  answer := FALSE;
  WHILE NOT answer DO
    N_CHAR := 12;
    WriteLn; WriteString("Password of "); WriteCard(N_CHAR, 0);
    WriteString(" printable chars OK? 'y' or another integer number >= 8: ");
    ReadString(answer_str);
    (*WriteString(answer_str); WriteLn;*) (*for testing*)

    IF Equal(answer_str, "y") THEN
      answer := TRUE;
    ELSE
      IF Is_all_digits(answer_str) THEN
        StrToInt(answer_str, N_CHAR, res);  (* this allows: 66 ggg *)

        IF  (N_CHAR < 8) OR (N_CHAR > 99) THEN
          WriteString("enter an integer number 8 <= i <= 99 or 'y'"); WriteLn;
          (* Clear the remaining newline/buffer character to prevent looping bugs; *)
          (* super-important! *)
          SkipLine;
        ELSE  (* success path *)
          answer := TRUE;
        END;
      ELSE
        WriteString("enter an integer number 8 <= i <= 99 or 'y'"); WriteLn;
        (* Clear the remaining newline/buffer character to prevent looping bugs; *)
        (* super-important! *)
        SkipLine;
      END;
    END;
  END;
  (* WriteInt(N_CHAR, 0); *)  (* for testing *)

  SkipLine;  (* super-important! *)
  WITH_SPECIAL_CHARS := TRUE;
  answer := FALSE;
  WHILE NOT answer DO
    WriteLn; WriteString("Do you want me to use special characters like .;,+*... ? 'y' or 'n': ");
    ReadString(answer_str);

    IF Equal(answer_str, "y") THEN
      answer := TRUE;
    ELSE
      WITH_SPECIAL_CHARS := FALSE;
      answer := TRUE;
    END;
  END;
  (* WriteCard(ORD(WITH_SPECIAL_CHARS), 1); *)  (* for testing: ORD(FALSE) yields 0, ORD(TRUE) yields 1 *)

  char_set[0] := 0C;
  IF WITH_SPECIAL_CHARS THEN
    FOR i := 33 TO 126 DO  (* Loop from 33 ('!') to 126 ('~') *)
      char_set[i - 33] := CHR(i);
    END;
    char_set[94] := 0C;  (* Manually set the null terminator at index 94 *)
  ELSE
    (* Equivalent to: strcat(charSet, "01234...") *)
    Append("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", char_set);
  END;
  (* WriteString(char_set); *)  (* for testing *)


  pw_chars[0] := 0C;  (* get a really empty string here *)
  i_ := 0;  (* char counter for the password is i_ to be of INTEGER type *)
  j  := 0;  (* counter for x *)

  WHILE (i_ < N_CHAR) DO
      Integer_to_bin_string(x[j], bin0);
      (*WriteLn; WriteLn; WriteString(bin0);*)  (* for testing *)

      Extract(bin0, 0,  8, bin0_0);
      Extract(bin0, 8, 16, bin0_1);
      (*WriteLn; WriteString(bin0_0);*)  (* for testing *)
      (*WriteString(" -- "); WriteString(bin0_1);*)  (* for testing *)

      char0a := Bin_string_to_int(bin0_0);
      char1a := Bin_string_to_int(bin0_1);
      (*WriteLn; WriteCard(char0a, 0);*)  (* for testing *)
      (*WriteString(" -- "); WriteCard(char1a, 0);*)  (* for testing *)

      char0b[0] := CHR(char0a);
      char0b[1] := 0C;
      char1b[0] := CHR(char1a);
      char1b[1] := 0C;
      (*WriteLn; WriteString(char0b);*)  (* for testing *)
      (*WriteString(char1b);*)  (* for testing *)

      FindNext(char0b, char_set, 0, char_found, pos);
      IF char_found THEN
          Append(char0b, pw_chars);
          INC(i_);
      END;

      FindNext(char1b, char_set, 0, char_found, pos);
      IF char_found AND (i_ < N_CHAR) THEN
          Append(char1b, pw_chars);
          INC(i_);
      END;

      INC(j);
      (*WriteLn; WriteString(pw_chars);*)  (* for testing *)
  END;


  WriteLn; WriteString("Your password of "); WriteCard(N_CHAR, 0);
  WriteString(" characters is: "); WriteString(pw_chars); WriteLn;

END random_bitstring_and_flexible_password_generator.
