(*
random_bitstring_and_flexible_password_generator_blaise.pp

This program is for the Blaise Pascal Compiler.

2026-07-23

build on Ubuntu 24 LTS: $ blaise --unit-path $HOME/scripts/Blaise_Pascal_Compiler/blaise-v0.13.0-linux-x86_64/stdlib-src \
                          --backend qbe \
                          --source random_bitstring_and_flexible_password_generator_blaise.pp \
                          --output random_bitstring_and_flexible_password_generator_blaise_qbe
                        # use the --backend qbe switch as long as it works, otherwise use: --linker external

run on Ubuntu 24 LTS:   $ ./random_bitstring_and_flexible_password_generator_blaise_qbe


$ blaise
Error: --source is required
Blaise Compiler v0.13.0
Copyright (c) 2026 Graeme Geldenhuys
...
$


Loosely based on random_streams_for_perf_stats.pp for the Free Pascal compiler in Object Free Pascal mode (objfpc).

*)


program Main;

uses StrUtils;    // StrUtils declares TStringBuilder, ContainsStr(const S, Sub: string): Boolean;
     // Text.Regex;  // Text.Regex declares TRegex: I didn't get this working! => using normal string to string matching


const
  upper_limit = 62501;  // 62501 for exactly 1M binary digits
  // upper_limit = 25;  // for testing

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

  bits_x, bits_hex, pw_chars       : TStringBuilder;

  bits_x_str, bits_hex_str, OutStr, answer_str   : String;
  char_set, bin0, bin0_0, bin0_1, char0b, char1b : String;

  i, j, FD, N_CHAR, char0a, char1a : Integer;

  answer, WITH_SPECIAL_CHARS       : Boolean;

  // Allocate a static hardware character buffer array
  buffer        : array[0..255] of Byte;  // Using Byte for safe raw hardware bindings
  bytes_read    : Integer;

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

// Declare the primitive POSIX read binding (perfectly supported by Blaise)
function c_read(fd: Integer; buf: Pointer; count: Integer): Integer; cdecl; external 'c' name 'read';


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



// Helper to parse unsigned binary string back to an integer
// This function is based on function bin_string_to_integer
// of Eiffel implementation random_bitstring_and_flexible_password_generator.e
function Bin_string_to_integer(binary_string: String): Integer;
var
  j, binary_as_integer, base: integer;
begin
  binary_as_integer := 0;
  base := 1;  // This represents 2^0
  for j := 7 downto 0 do
  begin
    if binary_string[j] = '1' then
      binary_as_integer := binary_as_integer + base;
    base := base * 2;  // Move to the next position (2^n)
  end;
  Result := binary_as_integer;
end;


(* end of user defined procedures                         *)
(**********************************************************)


begin
  sys_clock_gettime(0, @Ts);
  RandSeedState := Cardinal(Ts.tv_nsec);  // Use the lower 32-bits of the nanosecond count
  x[0] := RandSeedState mod (m - 2) + 1;

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


  // make a password of N_CHAR printable chars: user input requested here:
  answer := false;
  while not answer do
  begin
    N_CHAR := 12;
    Write(#10'Password of ', N_CHAR, ' printable chars OK? "y" or another integer number >= 8: ');

    // 0 is the universal file descriptor for standard input (stdin)
    // We leave 1 byte free at the end of the buffer for the string null-terminator
    bytes_read         := c_read(0, @buffer, SizeOf(buffer) - 1);
    buffer[bytes_read] := 0;  // Enforce safe null termination
    answer_str         := Trim(string(PChar(@buffer)));
    // WriteLn(answer_str);  // for testing

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

  WITH_SPECIAL_CHARS := true;
  answer := false;
  while not answer do
  begin
    Write(#10'Do you want me to use special characters like .;,+*... ? "y" or "n": ');

    bytes_read         := c_read(0, @buffer, SizeOf(buffer) - 1);
    buffer[bytes_read] := 0;  // Enforce safe null termination
    answer_str         := Trim(string(PChar(@buffer)));

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
    begin
      char_set := '';
      for i := 33 to 126 do
        char_set := char_set + Chr(i);
    end
  else
    char_set := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';


  pw_chars := TStringBuilder.Create();
  i        := 0;  // char counter for the password
  j        := 0;  // counter for x

  while i < N_CHAR do
  begin
    // WriteLn(); WriteLn(x[j]);  // for testing
    Integer_to_bin_string(x[j], bin0);
    // WriteLn(bin0);  // for testing

    bin0_0 := copy(bin0, 0, 8);
    // bin0_0[8] := 0;  // Enforce safe null termination
    bin0_1 := copy(bin0, 8, 8);  // last 8 is length!!
    // bin0_1[8] := 0;  // Enforce safe null termination
    // WriteLn(bin0_0, ' -- ', bin0_1);  // for testing

    char0a := Bin_string_to_integer(bin0_0);
    char1a := Bin_string_to_integer(bin0_1);
    // WriteLn(char0a, ' -- ', char1a);  // for testing

    char0b := Chr(char0a);
    char1b := Chr(char1a);
    // WriteLn(char0b, ' -- ', char1b);  // for testing

    if ContainsStr(char_set, char0b) then
      begin
        pw_chars.Append(char0b);
        inc(i);
        // WriteLn('match char0b !');  // for testing
      end;

    if (ContainsStr(char_set, char1b) and (i < N_CHAR)) then
      begin
        pw_chars.Append(char1b);
        inc(i);
        // WriteLn('match char1b !');  // for testing
      end;

    inc(j);
  end;

  WriteLn(#10'Your password of ', N_CHAR, ' characters is: ', pw_chars.ToString());

end.

(* end of random_bitstring_and_flexible_password_generator_blaise.pp *)
