(*
random_streams_for_perf_stats_fp.pp

This program is for the Free Pascal compiler in Free Pascal mode (fpc).

2026-07-18

build on Ubuntu 24 LTS: $ fpc random_streams_for_perf_stats_fp.pp
                          switch -O3 is not making this program faster

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats_fp
                        $ time ./random_streams_for_perf_stats_fp => real	0m0.017s
                        $ multitime -n 10 ./random_streams_for_perf_stats_fp
                        1: ./random_streams_for_perf_stats_fp
                                    Mean        Std.Dev.    Min         Median      Max
                        real        0.017       0.001       0.016       0.017       0.019       


$ fpc -V
Free Pascal Compiler version 3.2.2 [2021/05/16] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
...
$


transpiled from random_streams_for_perf_stats.pp (Object Free Pascal mode) with Google AI

*)

program Main;

{$mode fpc}{$H+}        // Free Pascal dialect, Free Pascal's default dialect
                        // $H+: $LONGSTRINGS ON: strings behave as AnsiString's:
                        //                       dynamically allocated, virtually unlimited size

const
  upper_limit = 62501;  // 62501 for exactly 1M binary digits

  m           = 65521;  // = 2^16 - 15
  a           = 17364;
  c           = 0;

  file_bits_x   = 'random_bitstring.bin';
  file_bits_hex = 'random_bitstring.byte';

var
  // Custom fixed-size array mapping to prevent stack overflow
  x             : array [0 .. upper_limit-1] of LongInt;

  bits_x, bits_hex : String;

  bits_x_str    : String[16];
  bits_hex_str  : String[4];

  outWr         : Text;
  i             : LongInt;
  ioErr         : Integer;


(**********************************************************)
(* user defined functions                                 *)

function Integer_to_bin_string (n: LongInt): String;
var
  res: String;
  j: Integer;
begin
  SetLength(res, 16);
  for j := 16 downto 1 do
  begin
    if (n and 1) = 1 then
      res[j] := '1'
    else
      res[j] := '0';
    n := n shr 1;
  end;
  Integer_to_bin_string := res;
end;


function Integer_to_hex_string (n: LongInt): String;
const
  HexChars: array[0..15] of Char = '0123456789abcdef';
var
  res: String;
  j: Integer;
begin
  SetLength(res, 4);
  for j := 4 downto 1 do
  begin
    res[j] := HexChars[n and $F];
    n := n shr 4;
  end;
  Integer_to_hex_string := res;
end;

(* end of user defined functions                          *)
(**********************************************************)


begin
  Randomize;

  x[0] := Random(m - 1) + 1;
  // Random(m - 1) returns a random number larger or equal to 0 and strictly less than m - 1

  // Initialize accumulators
  bits_x   := '';
  bits_hex := '';

  WriteLn(#10'generating a random bit stream...');

  for i := 1 to upper_limit - 1 do
  begin
    x[i] := (a * x[i - 1] + c) mod m;

    bits_x_str   := Integer_to_bin_string(x[i]);
    bits_x       := bits_x + bits_x_str;

    bits_hex_str := Integer_to_hex_string(x[i]);
    bits_hex     := bits_hex + bits_hex_str;
  end;


  // write bit stream to disk:
  Assign(outWr, file_bits_x);
  {$I-}                  // Turn off runtime errors for I/O
  Rewrite(outWr);
  ioErr := IOResult;
  if ioErr = 0 then
    begin
      Write(outWr, bits_x);
      Close(outWr);
      {$I+}                // Turn runtime errors back on
      WriteLn('Bit stream has been written to disk under name:  ', file_bits_x);
    end
  else
    begin
      {$I+}
      WriteLn('could not write to file: ', file_bits_x, ' !');
    end;

  // write byte stream to disk:
  Assign(outWr, file_bits_hex);
  {$I-}  // {$I-} or {$IOCHECKS OFF} directive to not generate input/output checking code in the program.
         // By default, the compiler generates I/O checking code.
  Rewrite(outWr);
  ioErr := IOResult;
  if ioErr = 0 then
    begin
      Write(outWr, bits_hex);
      Close(outWr);
      {$I+}
      WriteLn('Byte stream has been written to disk under name: ', file_bits_hex);
    end
  else
    begin
      {$I+}
      WriteLn('could not write to file: ', file_bits_hex, ' !');
    end;

end.

(* end of random_streams_for_perf_stats_fp.pp *)
