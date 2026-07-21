(*
random_streams_for_perf_stats_blaise.pp

This program is for the Blaise Pascal Compiler.

2026-07-21


build on Ubuntu 24 LTS: $ blaise --unit-path $HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/stdlib-src \
                          --backend qbe \
                          --source random_streams_for_perf_stats_blaise.pp \
                          --output random_streams_for_perf_stats_blaise_qbe
                        # use the --backend qbe switch as long as it works, otherwise use: --linker external

run on Ubuntu 24 LTS:   $ ./random_streams_for_perf_stats_blaise_qbe
                        $ time ./random_streams_for_perf_stats_blaise_qbe => real	0m0.028s
                        $ multitime -n 10 ./random_streams_for_perf_stats_blaise_qbe
                        1: ./random_streams_for_perf_stats_blaise_qbe
                                    Mean        Std.Dev.    Min         Median      Max
                        real        0.028       0.000       0.028       0.028       0.028


$ blaise
Error: --source is required
Blaise Compiler v0.13.0
Copyright (c) 2026 Graeme Geldenhuys
...
$


Loosely based on random_streams_for_perf_stats.pp for the Free Pascal compiler in Object Free Pascal mode (objfpc).

*)


program Main;

uses strutils;  // declares TStringBuilder

const
  upper_limit = 62501;  // 62501 for exactly 1M binary digits
  // upper_limit = 10;  // for testing

  m           = 65521;  // = 2^16 - 15
  a           = 17364;
  c           = 0;

  file_bits_x   = 'random_bitstring.bin';
  file_bits_hex = 'random_bitstring.byte';

type
  timespec = record
    tv_sec  : Int64;  // Seconds
    tv_nsec : Int64;  // Nanoseconds
  end;

var
  x             : array [0 .. upper_limit-1] of Integer;

  bits_x, bits_hex                 : TStringBuilder;

  bits_x_str, bits_hex_str, OutStr : String;

  i, FD         : Integer;

  RandSeedState : Cardinal;
  Ts            : timespec;


(**********************************************************)
(* user defined procedures                                *)

// Bind clock_gettime directly to standard libc
// Change 'libc' to 'c' so the external linker maps it to -lc
function sys_clock_gettime(clock_id: Integer; tp: Pointer): Integer; cdecl; external 'c' name 'clock_gettime';

function sys_open(path: PChar; flags: Integer; mode: Integer): Integer; cdecl; external 'c' name 'open';
function sys_write(fd: Integer; buf: Pointer; count: Integer): Integer; cdecl; external 'c' name 'write';
function sys_close(fd: Integer): Integer; cdecl; external 'c' name 'close';


procedure Integer_to_bin_string(n: integer; var res: String);
var
  j, temp: integer;
begin
  // Allocate exactly 16 bytes upfront in ONE step: very important for good exe speed!
  SetLength(res, 16);
  temp := n;
  for j := 15 downto 0 do
  begin
    if (temp mod 2) <> 0 then
      res[j] := 49
    else
      res[j] := 48;
    temp := temp div 2;
  end;
end;


procedure Integer_to_hex_string (n: integer; var res: String);
var
  HexChars: String;
  j, temp, rem: integer;
begin
  // Allocate exactly 4 bytes upfront in ONE step: very important for good exe speed!
  SetLength(res, 4);
  HexChars := '0123456789abcdef';
  temp := n;
  for j := 3 downto 0 do
  begin
    rem  := temp mod 16;

    // Direct character assignment: extract the raw numeric ASCII value
    // from HexChars and write it straight into res[j]. Zero heap churn!
    res[j] := HexChars[rem];

    temp := temp div 16;
  end;
end;

(* end of user defined procedures                         *)
(**********************************************************)


begin
  // CLOCK_MONOTONIC = Fetches uptime down to nanosecond precision
  RandSeedState := Cardinal(Ts.tv_nsec);  // Use the lower 32-bits of the nanosecond count

  x[0] := RandSeedState mod (m - 2) + 1;
  // WriteLn(x[0]);  // for testing

  bits_x   := TStringBuilder.Create();
  bits_hex := TStringBuilder.Create();

  SetLength(bits_x_str, 16);
  SetLength(bits_hex_str, 4);


  WriteLn(#10'generating a random bit stream...');  // #10 is the Line Feed (LF) character

  for i := 1 to upper_limit - 1 do
  begin
    x[i] := (a * x[i - 1] + c) mod m;
    // WriteLn(); WriteLn(x[i]);  // for testing

    // Convert to binary base-2 string and pad to 16 chars with '0'
    Integer_to_bin_string(x[i], bits_x_str);
    // WriteLn(bits_x_str);  // for testing
    bits_x.Append(bits_x_str);

    // Convert to hex base-16 string and pad to 4 chars with '0'
    Integer_to_hex_string(x[i], bits_hex_str);
    // WriteLn(bits_hex_str);  // for testing
    bits_hex.Append(bits_hex_str);
  end;

  // WriteLn(); WriteLn(bits_x.ToString());  // for testing
  // WriteLn(bits_hex.ToString());  // for testing

  // write bit stream to disk:
  OutStr := bits_x.ToString();
  FD := sys_open(PChar(file_bits_x), 1 or 64 or 512, 420);
  if FD >= 0 then
    begin
      try
        sys_write(FD, Pointer(OutStr), Length(OutStr));
      finally
        sys_close(FD);
      end;
      WriteLn('Bit stream has been written to disk under name:  ', file_bits_x);
    end
  else
    WriteLn('could not write to file: ', file_bits_x, ' !');

  // write byte stream to disk:
  OutStr := bits_hex.ToString();
  FD := sys_open(PChar(file_bits_hex), 1 or 64 or 512, 420);
  if FD >= 0 then
    begin
      try
        sys_write(FD, Pointer(OutStr), Length(OutStr));
      finally
        sys_close(FD);
      end;
      WriteLn('Byte stream has been written to disk under name: ', file_bits_hex);
    end
  else
    WriteLn('could not write to file: ', file_bits_hex, ' !');

end.

(* end of random_streams_for_perf_stats_blaise.pp *)
