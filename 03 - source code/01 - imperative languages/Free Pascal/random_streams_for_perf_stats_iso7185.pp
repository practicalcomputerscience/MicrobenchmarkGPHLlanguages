(*
random_streams_for_perf_stats_iso7185.pp

This program is for the Free Pascal compiler in ISO 7185 mode (iso).
See: https://wiki.freepascal.org/Standard_Pascal

2026-07-18

build on Ubuntu 24 LTS: $ fpc -Miso random_streams_for_perf_stats_iso7185.pp  # -Miso is important here!
                          switch -O3: Fatal: Internal error 200307043

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats_iso7185
                        $ time ./random_streams_for_perf_stats_iso7185 => real	0m0.015s
                        $ multitime -n 10 ./random_streams_for_perf_stats_iso7185
                        1: ./random_streams_for_perf_stats_iso7185
                                    Mean        Std.Dev.    Min         Median      Max
                        real        0.016       0.002       0.014       0.014       0.019


$ fpc -V
Free Pascal Compiler version 3.2.2 [2021/05/16] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
...
$


This program is based on ideas in random_streams_for_perf_stats.mod for GNU Modula-2 in ISO mode.

*)

program Main;

{$mode iso}

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
  BinStrType   = array [0..15] of char;
  HexStrType   = array [0..3]  of char;

  HexCharTable = array [0..15] of char;

var
  x            : array [0 .. upper_limit-1] of integer;

  i, h, seed, byte_nbr, ioErr : integer;

  bits_x       : array [0..M1-1] of char;
  bits_hex     : array [0..K250-1] of char;

  bits_x_str   : BinStrType;
  bits_hex_str : HexStrType;

  outWr        : TextFile;


(**********************************************************)
(* user defined procedures                                *)

procedure Integer_to_bin_string(n: integer; var res: BinStrType);
var
  j, temp: integer;
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
end;


procedure Integer_to_hex_string (n: integer; var res: HexStrType);
var
  HexChars: HexCharTable;
  j, temp, rem: integer;
begin
  HexChars := '0123456789abcdef';
  temp := n;
  for j := 3 downto 0 do
  begin
    rem := temp mod 16;
    res[j] := HexChars[rem];
    temp := temp div 16;
  end;
end;


procedure ASLR_seed(var ResultSeed: integer);
{Google AI:
  Leverage Address Space Layout Randomization (ASLR),
  a security feature built into most modern operating systems.}
type
  IntPtr = ^integer;

  { An ISO-compliant variant record maps different types to the same RAM bytes }
  EntropyBridge = record
    case boolean of
      true:  (PtrValue: IntPtr);
      { On 64-bit systems, a pointer fits across two 32-bit integers }
      false: (RawBytes: array[1..2] of integer);
  end;
var
  Bridge: EntropyBridge;
  Seed: integer;

begin
  { 1. Dynamically allocate a variable on the heap }
  { ASLR will assign this variable a completely unpredictable address }
  new(Bridge.PtrValue);

  { 2. Read the address out using the overlayed integer array variant }
  { This safely captures the random address bits without unsafe type casting }
  Seed := Bridge.RawBytes[1] + Bridge.RawBytes[2];

  { 3. Dispose of the dynamic variable safely }
  dispose(Bridge.PtrValue);

  { Enforce a clean, positive seed range }
  if Seed < 0 then
    ResultSeed := -Seed
  else if Seed > 0 then
      ResultSeed := Seed
  else
    ResultSeed := 31415;  { Fallback constant }

  (*writeln('Successfully generated seed from ASLR address: ', ResultSeed);*)  (*for testing*)
end;


(* end of user defined procedures                         *)
(**********************************************************)


begin
  ASLR_seed(seed);
  x[0] := seed mod (m - 2) + 1;
  (*writeln(x[0]);*)  (* for testing *)

  writeln; writeln('generating a random bit stream...');

  for i := 1 to upper_limit - 1 do
  begin
    x[i] := (a * x[i - 1] + c) mod m;
    (*writeln; writeln(x[i]);*)  (* for testing *)

    Integer_to_bin_string(x[i], bits_x_str);
    (*writeln(bits_x_str);*)  (* for testing *)
    byte_nbr := (i - 1) * 16;
    for h := 0 to 15 do
    begin
      bits_x[byte_nbr + h] := bits_x_str[h];
    end;

    Integer_to_hex_string(x[i], bits_hex_str);
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

(* end of random_streams_for_perf_stats_iso7185.pp *)
