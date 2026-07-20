(*
random_streams_for_perf_stats.pp

This program is for the Free Pascal compiler in Object Free Pascal mode (objfpc).

2026-07-18
2026-07-20: restrain exception handling only when writing to files


build on Ubuntu 24 LTS: $ fpc random_streams_for_perf_stats.pp
                          switch -O3 is not making this program faster

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats
                        $ time ./random_streams_for_perf_stats => real	0m0.017s
                        $ multitime -n 10 ./random_streams_for_perf_stats
                        1: ./random_streams_for_perf_stats
                                    Mean        Std.Dev.    Min         Median      Max
                        real        0.016       0.000       0.016       0.016       0.017


$ fpc -V
Free Pascal Compiler version 3.2.2 [2021/05/16] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
...
$


mostly transpiled from random_streams_for_perf_stats.m3 for the CM3 compiler with Google AI with manual corrections

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
  SysUtils, StrUtils, Classes;  // Delphi extensions with TStringStream, TStringList, IntToBin, IntToHex, Exception classes

const
  upper_limit = 62501;  // 62501 for exactly 1M binary digits

  m           = 65521;  // = 2^16 - 15
  a           = 17364;
  c           = 0;

  file_bits_x   = 'random_bitstring.bin';
  file_bits_hex = 'random_bitstring.byte';

var
  x             : array [0 .. upper_limit-1] of Integer;

  bits_x, bits_hex : TStringStream;

  bits_x_str,  bits_hex_str : String;

  outWr         : TStringList;
  i             : Integer;

begin
  // Initialize random number generator seed
  Randomize;

  x[0] := Random(m - 1) + 1;
  // Random(m - 1) returns a random number larger or equal to 0 and strictly less than m - 1

  bits_x   := TStringStream.Create('');
  bits_hex := TStringStream.Create('');
  outWr    := TStringList.Create;

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
  outWr.Free;

end.

(* end of random_streams_for_perf_stats.pp *)
