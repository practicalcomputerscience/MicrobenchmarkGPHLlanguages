package Stream_Generator_Spark with SPARK_Mode => On is

   Upper_Limit : constant Integer := 62501;
   Str_Length_Bin : constant Integer := 16;
   Str_Length_Hex : constant Integer := 4;

   subtype UInt16 is Integer range 0 .. 2**Str_Length_Bin - 1;
   subtype Random_Range is Integer range 1 .. 65520; -- m - 1

   type Bin_String_Type is new String (1 .. Str_Length_Bin);
   type Hex_String_Type is new String (1 .. Str_Length_Hex);

   type Bits_X_Array is array
     (1 .. (Upper_Limit - 1) * Str_Length_Bin) of Character;
   type Bits_Hex_Array is array
     (1 .. (Upper_Limit - 1) * Str_Length_Hex) of Character;
   type X_Array is array (1 .. Upper_Limit) of Random_Range;

   --  Pure SPARK functions: No side effects, bounds are guaranteed
   function Integer_To_Bin_String (N : UInt16) return Bin_String_Type;

   function Integer_To_Hex_String (N : UInt16) return Hex_String_Type;

   --  Pure algorithmic procedure ready for formal proof
   procedure Generate_Data_Streams
     (X        : X_Array;
      Bits_X   : out Bits_X_Array;
      Bits_Hex : out Bits_Hex_Array)
   with
     Post => True;

end Stream_Generator_Spark;
