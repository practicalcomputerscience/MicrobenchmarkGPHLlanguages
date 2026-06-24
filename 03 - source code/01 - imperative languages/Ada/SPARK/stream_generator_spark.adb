package body Stream_Generator_Spark with SPARK_Mode => On is

   ---------------------------
   -- Integer_To_Bin_String --
   ---------------------------
   function Integer_To_Bin_String (N : UInt16) return Bin_String_Type is
      Result : Bin_String_Type := (others => '0');
      K      : UInt16 := N;
      J      : Integer := Str_Length_Bin;
   begin
      while K > 0 and then J >= 1 loop
         if K mod 2 > 0 then
            Result (J) := '1';
         end if;
         K := K / 2;

         pragma Loop_Invariant (J in 1 .. Str_Length_Bin);
         --  FIX: Proves to SPARK that the loop decreases and will terminate
         pragma Loop_Variant (Decreases => K);

         exit when J = 1;
         J := J - 1;
      end loop;
      return Result;
   end Integer_To_Bin_String;

   ---------------------------
   -- Integer_To_Hex_String --
   ---------------------------
   function Integer_To_Hex_String (N : UInt16) return Hex_String_Type is
      Result    : Hex_String_Type := (others => '0');
      K         : UInt16 := N;
      J         : Integer := Str_Length_Hex;
      Remainder : Integer;
   begin
      while K > 0 and then J >= 1 loop
         Remainder := K mod 16;
         case Remainder is
            when 0  => Result (J) := '0';
            when 1  => Result (J) := '1';
            when 2  => Result (J) := '2';
            when 3  => Result (J) := '3';
            when 4  => Result (J) := '4';
            when 5  => Result (J) := '5';
            when 6  => Result (J) := '6';
            when 7  => Result (J) := '7';
            when 8  => Result (J) := '8';
            when 9  => Result (J) := '9';
            when 10 => Result (J) := 'a';
            when 11 => Result (J) := 'b';
            when 12 => Result (J) := 'c';
            when 13 => Result (J) := 'd';
            when 14 => Result (J) := 'e';
            when 15 => Result (J) := 'f';
            when others => null;
         end case;
         K := K / 16;

         pragma Loop_Invariant (J in 1 .. Str_Length_Hex);
         --  FIX: Proves termination
         pragma Loop_Variant (Decreases => K);

         exit when J = 1;
         J := J - 1;
      end loop;
      return Result;
   end Integer_To_Hex_String;

   ---------------------------
   -- Generate_Data_Streams --
   ---------------------------
   procedure Generate_Data_Streams
     (X        : X_Array;
      Bits_X   : out Bits_X_Array;
      Bits_Hex : out Bits_Hex_Array)
   is
      M : constant Integer := 65521;
      A : constant Integer := 17364;
      C : constant Integer := 0;

      Bits_X_Str   : Bin_String_Type;
      Bits_Hex_Str : Hex_String_Type;
      Byte_Nbr_Bin : Integer;
      Byte_Nbr_Hex : Integer;
      X_Val        : Random_Range;
      Next_X       : X_Array := X;
   begin
      Bits_X   := (others => '0');
      Bits_Hex := (others => '0');

      for I in 2 .. Upper_Limit loop
         --  FIX: The mathematical result of mod M is guaranteed to be 0..65520
         --  SPARK now accepts this because we strictly prove
         --  Next_X data bounds below.
         X_Val := (A * Next_X (I - 1) + C) mod M;

         Next_X (I) := X_Val;

         Bits_X_Str   := Integer_To_Bin_String (X_Val);
         Byte_Nbr_Bin := (I - 2) * Str_Length_Bin;

         for K in 1 .. Str_Length_Bin loop
            --  FIX: Inner invariant ensures K stays bounded during slicing
            pragma Loop_Invariant (K in 1 .. Str_Length_Bin);
            Bits_X (Byte_Nbr_Bin + K) := Bits_X_Str (K);
         end loop;

         Bits_Hex_Str := Integer_To_Hex_String (X_Val);
         Byte_Nbr_Hex := (I - 2) * Str_Length_Hex;

         for K in 1 .. Str_Length_Hex loop
            --  FIX: Inner invariant ensures K stays bounded during slicing
            pragma Loop_Invariant (K in 1 .. Str_Length_Hex);
            Bits_Hex (Byte_Nbr_Hex + K) := Bits_Hex_Str (K);
         end loop;

         --  FIX: Enhanced contracts proving math tracking
         --  history across loop steps
         pragma Loop_Invariant
           (Byte_Nbr_Bin = (I - 2) * Str_Length_Bin and then
            Byte_Nbr_Hex = (I - 2) * Str_Length_Hex and then
            (for all K in 1 .. I => Next_X (K) in Random_Range));
      end loop;
   end Generate_Data_Streams;

end Stream_Generator_Spark;
