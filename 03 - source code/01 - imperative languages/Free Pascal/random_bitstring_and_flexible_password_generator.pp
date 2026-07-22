(*
random_bitstring_and_flexible_password_generator.pp

This program is for the Free Pascal compiler in Object Free Pascal mode (objfpc).

2026-07-19
2026-07-20: restrain exception handling only when writing to files;
            no ErrorCode (Integer) at Val('%' + bin0_0, char0a)
2026-07-23: less code at if-then-else's


build on Ubuntu 24 LTS: $ fpc random_bitstring_and_flexible_password_generator.pp

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator


$ fpc -V
Free Pascal Compiler version 3.2.2 [2021/05/16] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
...
$

*)


program Main;

{$mode objfpc}{$H+}{$M+}  // Object Free Pascal mode set to use some Delphi extensions,
                          // while some Free Pascal constructs are still available,
                          // a Google AI suggestion to "enforce a cleaner,
                          // more rigid syntax that closely mirrors Modula-3's philosophy"
                          // $H+: $LONGSTRINGS ON: strings behave as AnsiString's:
                          //                       dynamically allocated, virtually unlimited size
                          // $M+: $TYPEINFO ON: generate RTTI (run time type information) for classes

uses
  SysUtils, StrUtils, Classes, RegExpr;
  // Delphi extensions with TStringStream, TStringList, IntToBin, IntToHex, Exception classes

const
  upper_limit = 62501;  // 62501 for exactly 1M binary digits
  // upper_limit = 50;  // for testing

  m           = 65521;  // = 2^16 - 15
  a           = 17364;
  c           = 0;

  file_bits_x   = 'random_bitstring.bin';
  file_bits_hex = 'random_bitstring.byte';

var
  x             : array [0 .. upper_limit-1] of Integer;

  bits_x, bits_hex, pw_chars : TStringStream;

  bits_x_str, bits_hex_str, answer_str : String;
  bin0, bin0_0, bin0_1, char0b, char1b : String;

  i, j, N_CHAR, char0a, char1a : Integer;

  answer, WITH_SPECIAL_CHARS : Boolean;

  pattern       : TRegExpr;  // https://wiki.freepascal.org/RegEx_packages


begin
  // Initialize random number generator seed
  Randomize;

  x[0] := Random(m - 1) + 1;
  // Random(m - 1) returns a random number larger or equal to 0 and strictly less than m - 1

  bits_x   := TStringStream.Create('');
  bits_hex := TStringStream.Create('');

  WriteLn(#10'generating a random bit stream...');  // #10 is the Line Feed (LF) character

  for i := 1 to upper_limit - 1 do
  begin
    x[i] := (a * x[i - 1] + c) mod m;

    // Convert to binary base-2 string and pad to 16 chars with '0'
    bits_x_str := IntToBin(x[i], 16);
    bits_x.WriteString(bits_x_str);

    // Convert to hex base-16 string and pad to 4 chars with '0'
    bits_hex_str := LowerCase(IntToHex(x[i], 4));
    bits_hex.WriteString(bits_hex_str);
  end;


  // write bit stream to disk:
  try
    bits_x.SaveToFile(file_bits_x);  // SaveToFile() for no trailing newline or dot
    WriteLn('Bit stream has been written to disk under name:  ', file_bits_x);
  except
    on E: Exception do
      WriteLn('could not write to file: ', file_bits_x, ' ! -- ', E.Message);
  end;

  // write byte stream to disk:
  try
    bits_hex.SaveToFile(file_bits_hex);
    WriteLn('Byte stream has been written to disk under name: ', file_bits_hex);
  except
    on E: Exception do
      WriteLn('could not write to file: ', file_bits_hex, ' ! -- ', E.Message);
  end;

  // Free allocated memory buffers
  bits_x.Free;
  bits_hex.Free;


  // make a password of N_CHAR printable chars: user input requested here:
  answer := false;
  while not answer do
  begin
    N_CHAR := 12;
    Write(#10'Password of ', N_CHAR, ' printable chars OK? "y" or another integer number >= 8: ');
    ReadLn(answer_str);

    if answer_str = 'y' then
        answer := true
    else
      begin
        try
          N_CHAR := StrToInt(answer_str);
          if N_CHAR < 8 then
            WriteLn('enter an integer number >= 8 or "y"')
          else
            answer := true;
        except
          WriteLn('enter an integer number >= 8 or "y"');
        end;
      end;
  end;
  // WriteLn('N_CHAR = ', N_CHAR);  // for testing

  answer := false;
  WITH_SPECIAL_CHARS := true;
  while not answer do
  begin
    Write(#10'Do you want me to use special characters like .;,+*... ? "y" or "n": ');
    ReadLn(answer_str);

    if answer_str = 'y' then
        answer := true
    else
      begin
        WITH_SPECIAL_CHARS := false;
        answer := true;
      end;
  end;
  // WriteLn('WITH_SPECIAL_CHARS = ', WITH_SPECIAL_CHARS);  // for testing

  if WITH_SPECIAL_CHARS then
    pattern := TRegExpr.Create('[!-~]')
  else
    pattern := TRegExpr.Create('[A-Za-z0-9]');


  pw_chars := TStringStream.Create('');
  i        := 0;  // char counter for the password
  j        := 0;  // counter for x

  while i < N_CHAR do
  begin
    // WriteLn; WriteLn(x[j]);  // for testing
    bin0 := IntToBin(x[j], 16);
    // WriteLn(bin0);  // for testing

    bin0_0 := copy(bin0, 1, 8);
    bin0_1 := copy(bin0, 9, 16);
    // WriteLn(bin0_0, ' -- ', bin0_1);  // for testing

    Val('%' + bin0_0, char0a);
    Val('%' + bin0_1, char1a);
    // WriteLn(char0a, ' -- ', char1a);  // for testing

    char0b := Chr(char0a);
    char1b := Chr(char1a);
    // WriteLn(char0b, ' -- ', char1b);  // for testing

    if pattern.Exec(String(char0b)) then  // explicitly using String() is safer here
      begin
        pw_chars.WriteString(char0b);
        inc(i);
      end;

    if (pattern.Exec(String(char1b)) and (i < N_CHAR)) then
      begin
        pw_chars.WriteString(char1b);
        inc(i);
      end;

    inc(j);
  end;

  WriteLn(#10'Your password of ', N_CHAR, ' characters is: ', pw_chars.DataString);

  pattern.Free;  // Always free object instances to prevent memory leaks
  pw_chars.Free;

end.

(* end of random_bitstring_and_flexible_password_generator.pp *)
