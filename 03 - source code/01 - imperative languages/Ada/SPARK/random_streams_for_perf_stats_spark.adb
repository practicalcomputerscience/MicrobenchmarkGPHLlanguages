with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Sequential_IO;
with Stream_Generator_Spark;      use Stream_Generator_Spark;

procedure Random_Streams_For_Perf_Stats_Spark is

   File_Bits_X   : constant String := "random_bitstring.bin";
   File_Bits_Hex : constant String := "random_bitstring.byte";

   package Seq_IO is new Ada.Sequential_IO (Character);
   use Seq_IO;
   F : Seq_IO.File_Type;

   package R is new Ada.Numerics.Discrete_Random (Random_Range);
   use R;
   G : Generator;

   X        : X_Array;
   Bits_X   : Bits_X_Array;
   Bits_Hex : Bits_Hex_Array;
begin
   Reset (G);
   X (1) := Random (G);

   New_Line;
   Put_Line ("Generating a random bit stream safely via SPARK core...");

   --  Fire the formally proven computing engine
   Generate_Data_Streams (X, Bits_X, Bits_Hex);

   --  Safely handle standard system I/O effects in container boundaries
   begin
      Create (F, Out_File, File_Bits_X);
      for I in Bits_X'Range loop
         Write (F, Bits_X (I));
      end loop;
      Close (F);
      Put_Line ("Bit stream written to disk:  " & File_Bits_X);
   exception
      when others => Put_Line ("Could not write to file: " & File_Bits_X);
   end;

   begin
      Create (F, Out_File, File_Bits_Hex);
      for I in Bits_Hex'Range loop
         Write (F, Bits_Hex (I));
      end loop;
      Close (F);
      Put_Line ("Byte stream written to disk: " & File_Bits_Hex);
   exception
      when others => Put_Line ("Could not write to file: " & File_Bits_Hex);
   end;

end Random_Streams_For_Perf_Stats_Spark;
