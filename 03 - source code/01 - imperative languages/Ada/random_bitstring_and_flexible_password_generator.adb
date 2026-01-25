--  random_bitstring_and_flexible_password_generator.adb
--
--  2025-05-09/10/11/12/13/18/31
--  2025-07-13: repaired Exception Handling when writing to files => program must not stop at an exception here!
--  2025-07-16: speed improved Integer_to_bin_string => change not measurable, but leave changed code!
--  2025-12-14: see below
--  2026-01-25: fixing some warnings
--
--
--  build on Ubuntu 24 LTS: $ alr init --bin random_bitstring_and_flexible_password_generator
--                          $ cd random_bitstring_and_flexible_password_generator
--                          $ alr build
--                          $ alr run
--
--  run on Ubuntu 24 LTS:   $ ./bin/random_bitstring_and_flexible_password_generator
--
--  changes to random_bitstring_and_flexible_password_generator_config.gpr:
--    "-O3" <-- https://gcc.gnu.org/onlinedocs/gnat_ugn/Optimization-Levels.html
--    commented: --  "-Og" -- Optimize for debug

with Ada.Text_IO; use Ada.Text_IO;  --  with use clause
--  => Put("5 + 9 - 3 = "); Put(5 + 9 - 3, 3); New_Line;
--     instead of Ada.Text_IO.Put(), Ada.Integer_Text_IO.Put()
--  with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
--  with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

with Ada.Numerics.Discrete_Random;

with Ada.Sequential_IO;  --  a generic package is not allowed in a use clause!!
--  Ada.Sequential_IO:
--    this is used for binary I/O and is always in a sequential mode of op's
--  write stream like binary data, not text data
--  with a final line feed character at the end of files:
--  https://perso.telecom-paristech.fr/pautet/Ada95/e_c24_p1.ada

--  with Ada.Real_Time; use Ada.Real_Time;

procedure random_bitstring_and_flexible_password_generator is

   upper_limit : constant Integer := 62501;
   --  62501 for exactly 1M binary digits
   --  upper_limit : constant Integer := 10;  --  for testing
   --  M1 : constant Integer := 1_000_000;
   --  K250 : constant Integer := 250_000;

   STR_LENGTH_BIN : constant Integer := 16;
   STR_LENGTH_HEX : constant Integer := 4;

   subtype UInt16 is Integer range 0 .. 2**STR_LENGTH_BIN - 1;

   m : constant Integer := 65521;  --  = 2^16 - 15
   a : constant Integer := 17364;
   c : constant Integer := 0;

   file_bits_x : constant String := "random_bitstring.bin";
   file_bits_hex : constant String := "random_bitstring.byte";

   package Seq_IO is new Ada.Sequential_IO (Character);
   use Seq_IO;
   F : Seq_IO.File_Type;

   subtype Random_Range is Integer range 1 .. m - 1;  --  2025-12-14: don't start with 0 or m!
   --  Learning Ada - the complete contents of learn.adacore.com, 2025,
   --  23.2. Random Number Generation
   package R is new
     Ada.Numerics.Discrete_Random (Random_Range);
   use R;
   G : Generator;
   x : array (1 .. upper_limit) of Random_Range;

   --  bits_x : array (1 .. M1) of Character;
   bits_x : array (1 .. upper_limit * STR_LENGTH_BIN) of Character;
   bits_x_str : String (1 .. STR_LENGTH_BIN);
   bin_str : String (1 .. STR_LENGTH_BIN);

   --  bits_hex : array (1 .. K250) of Character;
   bits_hex : array (1 .. upper_limit * STR_LENGTH_HEX) of Character;
   bits_hex_str : String (1 .. STR_LENGTH_HEX);
   hex_str : String (1 .. STR_LENGTH_HEX);

   byte_nbr, i, j, remainder : Integer;
   k : UInt16;

   --  char counter for x: don't take j, it's not working!?!
   jj : Integer := 0;

   subtype PW_Range is Integer range 8 .. 99;
   N_CHAR : PW_Range := 25;
   answer : Boolean := False;
   WITH_SPECIAL_CHARS : Boolean := True;
   answer_str : Unbounded_String := To_Unbounded_String ("");

   package SU renames Ada.Strings.Unbounded;
   pw_chars : SU.Unbounded_String;

   subtype char_upper_letter is Character range 'A' .. 'Z';
   subtype char_lower_letter is Character range 'a' .. 'z';
   subtype char_dec_digit is Character range '0' .. '9';
   subtype char_printable is Character range '!' .. '~';

   bin0_0, bin0_1 : String (1 .. 8);
   bin0_0a, bin0_1a : String (1 .. 11);  --  2#11100000#

   char0_nbr, char1_nbr : Integer;
   char0, char1 : Character;

   function Integer_to_bin_string (N : UInt16) return String is
   begin
      bin_str := "0000000000000000";
      j := STR_LENGTH_BIN;
      k := N;
      while k > 0 and then j >= 1 loop
         if k mod 2 > 0 then
            bin_str (j) := '1';  --  Standard Character here, not String
         end if;
         k := k / 2;  --  no remainder here
         j := j - 1;
      end loop;
      return bin_str;
   end Integer_to_bin_string;

   function Integer_to_hex_string (N : UInt16) return String is
   begin
      hex_str := "0000";
      j := STR_LENGTH_HEX;
      k := N;
      while k > 0 and then j <= STR_LENGTH_HEX loop
         remainder := k mod 16;
         case remainder is
            --  Standard Character here, not String
            when 0  => hex_str (j) := '0';
            when 1  => hex_str (j) := '1';
            when 2  => hex_str (j) := '2';
            when 3  => hex_str (j) := '3';
            when 4  => hex_str (j) := '4';
            when 5  => hex_str (j) := '5';
            when 6  => hex_str (j) := '6';
            when 7  => hex_str (j) := '7';
            when 8  => hex_str (j) := '8';
            when 9  => hex_str (j) := '9';
            when 10 => hex_str (j) := 'a';
            when 11 => hex_str (j) := 'b';
            when 12 => hex_str (j) := 'c';
            when 13 => hex_str (j) := 'd';
            when 14 => hex_str (j) := 'e';
            when 15 => hex_str (j) := 'f';
            when others => null;
         end case;
         k := k / 16;  --  no remainder here
         j := j - 1;
      end loop;
      return hex_str;
   end Integer_to_hex_string;

begin
   Reset (G); --  Start the generator in a unique state in each run
   --             --> Ada 2022 Reference Manual: A.5.2 Random Number Generation
   x (1) := Random (G);  --  initialize x(1)

   --  for testing:
   --  Put ("x (1) = "); Ada.Integer_Text_IO.Put (x (1)); New_Line;

   New_Line; Put_Line ("generating a random bit stream...");
   for i in 2 .. upper_limit loop
      x (i) := (a * x (i - 1) + c) mod m;
      --  for testing: formatted printing:
      --  New_Line; Ada.Integer_Text_IO.Put(x (i), Width => 5); New_Line;

      bits_x_str := Integer_to_bin_string (x (i));
      --  for testing:
      --  Put ("bits_x_str = "); Put (bits_x_str); New_Line;
      byte_nbr := (i - 2) * STR_LENGTH_BIN;
      bits_x (byte_nbr + 1)  := bits_x_str (1);
      bits_x (byte_nbr + 2)  := bits_x_str (2);
      bits_x (byte_nbr + 3)  := bits_x_str (3);
      bits_x (byte_nbr + 4)  := bits_x_str (4);
      bits_x (byte_nbr + 5)  := bits_x_str (5);
      bits_x (byte_nbr + 6)  := bits_x_str (6);
      bits_x (byte_nbr + 7)  := bits_x_str (7);
      bits_x (byte_nbr + 8)  := bits_x_str (8);
      bits_x (byte_nbr + 9)  := bits_x_str (9);
      bits_x (byte_nbr + 10) := bits_x_str (10);
      bits_x (byte_nbr + 11) := bits_x_str (11);
      bits_x (byte_nbr + 12) := bits_x_str (12);
      bits_x (byte_nbr + 13) := bits_x_str (13);
      bits_x (byte_nbr + 14) := bits_x_str (14);
      bits_x (byte_nbr + 15) := bits_x_str (15);
      bits_x (byte_nbr + 16) := bits_x_str (16);

      bits_hex_str := Integer_to_hex_string (x (i));
      --  for testing:
      --  Put ("bits_hex_str = "); Put (bits_hex_str); New_Line;
      byte_nbr := (i - 2) * STR_LENGTH_HEX;
      bits_hex (byte_nbr + 1) := bits_hex_str (1);
      bits_hex (byte_nbr + 2) := bits_hex_str (2);
      bits_hex (byte_nbr + 3) := bits_hex_str (3);
      bits_hex (byte_nbr + 4) := bits_hex_str (4);
   end loop;

   --  for testing:
   --  New_Line; Put ("bits_x = "); New_Line;
   --  for i in 1 .. (upper_limit - 1) * STR_LENGTH_BIN loop
   --     Put (bits_x (i));
   --  end loop;

   --  for testing:
   --  New_Line; Put ("bits_hex = "); New_Line;
   --  for i in 1 .. (upper_limit - 1) * STR_LENGTH_HEX loop
   --     Put (bits_hex (i));
   --  end loop;

   --  write byte stream to disk:
   begin
      Create (F, Out_File, file_bits_x);
         for i in 1 .. (upper_limit - 1) * STR_LENGTH_BIN loop
            Write (F, bits_x (i));
         end loop;
      Close (F);
      Put_Line ("Bit stream has been written to disk under name:  " & file_bits_x);
      exception
         when Seq_IO.Status_Error =>
            Put_Line ("could not write to file: " & file_bits_x);
         when others =>
            Put_Line ("could not write to file: " & file_bits_x);
   end;
   begin
      Create (F, Out_File, file_bits_hex);
         for i in 1 .. (upper_limit - 1) * STR_LENGTH_HEX loop
            Write (F, bits_hex (i));
         end loop;
      Close (F);
      Put_Line ("Byte stream has been written to disk under name: " & file_bits_hex);
      exception
         when Seq_IO.Status_Error =>
            Put_Line ("could not write to file: " & file_bits_hex);
         when others =>
            Put_Line ("could not write to file: " & file_bits_hex);
   end;

   --  make a password of N_CHAR printable chars: user input requested here
   while not answer loop
      N_CHAR := 12;
      New_Line;
      Put ("Password of" & Integer'Image (N_CHAR));
      Put (" printable chars OK? 'y' or another integer number >= 8: ");
      answer_str := Get_Line;

      if answer_str = "y" then
         answer := True;
      else
         begin
            N_CHAR := Integer'Value (To_String (answer_str));
            answer := True;

            exception
               when Constraint_Error =>
                  Put_Line ("enter an integer number 8 <= i <= 99 or 'y'");
               when others => null;
         end;
      end if;
   end loop;

   answer := False;
   answer_str := To_Unbounded_String ("");
   while not answer loop
      New_Line;
      Put
   ("Do you want me to use special characters like .;,+*... ? 'y' or 'n': ");
      answer_str := Get_Line;
      if answer_str = "y" then
         answer := True;
      else
         WITH_SPECIAL_CHARS := False;
         answer := True;
      end if;
   end loop;

   i := 1;   --  char counter for the password
   jj := 1;  --  char counter for x:
   --  don't take j from Integer_to_bin_string(), it's not working!?!

   while i <= N_CHAR loop
      --  for testing: formatted printing:
      --  Ada.Integer_Text_IO.Put(x (jj), Width => 5); New_Line;
      bits_x_str := Integer_to_bin_string (x (jj));  -- type is String
      --  Put_Line (bits_x_str);  --  for testing
      bin0_0 := bits_x_str (1 .. 8);
      bin0_1 := bits_x_str (9 .. 16);
      --  https://sites.radford.edu/~nokie/classes/320/strings.html

      --  join Strings, concatenate Strings: -- 2#01010001#
      bin0_0a := "2#" & bin0_0 & "#";
      bin0_1a := "2#" & bin0_1 & "#";

      char0_nbr := Integer'Value (bin0_0a);
      char1_nbr := Integer'Value (bin0_1a);
      --  for testing: formatted printing:
      --  Ada.Integer_Text_IO.Put(char0_nbr, Width => 3); Put ("  ");
      --  Ada.Integer_Text_IO.Put(char1_nbr, Width => 3); New_Line;

      char0 := Character'Val (char0_nbr);
      char1 := Character'Val (char1_nbr);
      --  Put (char0); Put (char1); New_Line;  --  for testing

      if WITH_SPECIAL_CHARS then
         if char0 in char_printable
         then
            pw_chars := pw_chars & char0;
            i := i + 1;
         end if;

         if char1 in char_printable
            and then i < N_CHAR
         then
            pw_chars := pw_chars & char1;
            i := i + 1;
         end if;

      else
         if char0 in char_upper_letter
            or else char0 in char_lower_letter
            or else char0 in char_dec_digit
         then
            pw_chars := pw_chars & char0;
            i := i + 1;
         end if;

         if (char1 in char_upper_letter
            or else char1 in char_lower_letter
            or else char1 in char_dec_digit)
            and then i < N_CHAR
         then
            pw_chars := pw_chars & char1;
            i := i + 1;
         end if;
      end if;

      jj := jj + 1;

   end loop;

   New_Line; Put_Line ("Your password of" & Integer'Image (N_CHAR)
            & " characters is: " & pw_chars);

end random_bitstring_and_flexible_password_generator;
