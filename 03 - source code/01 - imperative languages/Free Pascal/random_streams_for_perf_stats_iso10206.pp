(*
random_streams_for_perf_stats_iso10206.pp

This program is for the Free Pascal compiler in ISO 10206 mode (extendedpascal).
See: https://wiki.freepascal.org/Standard_Pascal

It looks like that the Free Pascal compiler is not fully ISO 10206 compliant yet.
The difference between programs:
  - random_streams_for_perf_stats_iso7185.pp
  - random_streams_for_perf_stats_iso10206.pp
is too small and it's only that:
  - procedures became functions
  - TimeStamp type and GetTimeStamp procedure implemented
    (instead of leveraging the Address Space Layout Randomization, ASLR, for a random seed)


2026-07-18

build on Ubuntu 24 LTS: $ fpc -Mextendedpascal random_streams_for_perf_stats_iso10206.pp  # -Mextendedpascal is important here!
                          switch -O3: Fatal: Internal error 200307043

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats_iso10206
                        $ time ./random_streams_for_perf_stats_iso10206 =>
                        $ multitime -n 10 ./random_streams_for_perf_stats_iso10206
                        1: ./random_streams_for_perf_stats_iso10206
                                    Mean        Std.Dev.    Min         Median      Max
                        real        0.014       0.000       0.014       0.015       0.015


$ fpc -V
Free Pascal Compiler version 3.2.2 [2021/05/16] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
...
$


This program is a soft adaption of random_streams_for_perf_stats_iso7185.pp for ISO 7185 mode.

*)

program Main;

{$mode extendedpascal}

const
  upper_limit = 62501;  (* 62501 for exactly 1M binary digits *)
  (*upper_limit = 10;*)  (* for testing *)

  M1          = upper_limit * 16;
  K250        = upper_limit * 4;

  m           = 65521;  (* = 2^16 - 15 *)
  a           = 17364;
  c           = 0;

  file_bits_x   = 'random_bitstring.bin';
  file_bits_hex = 'random_bitstring.byte';

type
  { ISO 10206 fixed-string representations }
  BinStrType   = array [0..15] of char;
  HexStrType   = array [0..3] of char;

  HexCharTable = array [0..15] of char;

var
  x            : array [0 .. upper_limit-1] of integer;

  i, h, byte_nbr, ioErr : integer;

  bits_x       : array [0..M1-1] of char;
  bits_hex     : array [0..K250-1] of char;

  bits_x_str   : BinStrType;
  bits_hex_str : HexStrType;

  outWr        : TextFile;

  t            : TimeStamp;


(**********************************************************)
(* user defined procedures                                *)

{ Extended Pascal natively allows functions to return structured array values }
function Integer_to_bin_string(n: integer): BinStrType;
var
  j, temp: integer;
  res: BinStrType;
begin
  temp := n;
  for j := 15 downto 0 do
  begin
    if (temp mod 2) <> 0 then
      res[j] := '1'
    else
      res[j] := '0';
    temp := temp div 2;
  end;
  Integer_to_bin_string := res;
end;


function Integer_to_hex_string(n: integer): HexStrType;
var
  HexChars: HexCharTable;
  j, temp, rem: integer;
  res: HexStrType;
begin
  HexChars := '0123456789abcdef';
  temp := n;
  for j := 3 downto 0 do
  begin
    rem := temp mod 16;
    res[j] := HexChars[rem];
    temp := temp div 16;
  end;
  Integer_to_hex_string := res;
end;

(* end of user defined procedures                         *)
(**********************************************************)


begin
  GetTimeStamp (t);
  (*
    https://www.gnu-pascal.org/gpc/TimeStamp.html
    TimeStamp is an ISO 10206 Extended Pascal extension.
    The fields DateValid, TimeValid, Year, Month, Day, Hour, Minute, Second are required by Extended Pascal...
  *)
  (*writeln(t.Second);*)  (* for testing *)
  x[0] := (t.Second + t.Minute + t.Hour + t.Day + t.Month + t.Year) mod (m - 2) + 1;
  (*writeln(x[0]);*)  (* for testing *)

  writeln; writeln('generating a random bit stream...');

  for i := 1 to upper_limit - 1 do
  begin
    x[i] := (a * x[i - 1] + c) mod m;
    (*writeln; writeln(x[i]);*)  (* for testing *)

    bits_x_str   := Integer_to_bin_string(x[i]);
    (*writeln(bits_x_str);*)  (* for testing *)
    byte_nbr := (i - 1) * 16;
    for h := 0 to 15 do
    begin
      bits_x[byte_nbr + h] := bits_x_str[h];
    end;

    bits_hex_str := Integer_to_hex_string(x[i]);
    (*writeln(bits_hex_str);*)  (* for testing *)
    byte_nbr := (i - 1) * 4;
    for h := 0 to 3 do
    begin
      bits_hex[byte_nbr + h] := bits_hex_str[h];
    end;
  end;

  (*writeln; writeln(bits_x);*)  (* for testing *)
  (*writeln; writeln(bits_hex);*)  (* for testing *)


  (* write bit stream to disk *)
  Assign(outWr, file_bits_x);  (* Assign a name to the file *)
  {$I-}  (* {$I-} or {$IOCHECKS OFF} directive to not generate input/output checking code in the program *)
         (* By default, the compiler generates I/O checking code *)
  Rewrite(outWr);
  ioErr := IOResult;
  if ioErr = 0 then
    begin
      Write(outWr, bits_x);  (* Write the string to the file *)
      Close(outWr);          (* Close the file *)
      {$I+}
      WriteLn('Bit stream has been written to disk under name:  ', file_bits_x);
    end
  else
    begin
      {$I+}
      WriteLn('could not write to file: ', file_bits_x, ' !');
    end;

  (* write byte stream to disk *)
  Assign(outWr, file_bits_hex);  (* Assign a name to the file *)
  {$I-}  (* {$I-} or {$IOCHECKS OFF} directive to not generate input/output checking code in the program *)
         (* By default, the compiler generates I/O checking code *)
  Rewrite(outWr);
  ioErr := IOResult;
  if ioErr = 0 then
    begin
      Write(outWr, bits_hex);  (* Write the string to the file *)
      Close(outWr);            (* Close the file *)
      {$I+}
      WriteLn('Byte stream has been written to disk under name: ', file_bits_hex);
    end
  else
    begin
      {$I+}
      WriteLn('could not write to file: ', file_bits_hex, ' !');
    end;

end.

(* end of random_streams_for_perf_stats_iso10206.pp *)
