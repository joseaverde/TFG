--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-fast_fourier_transform_details.adb            |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Numerics.Elementary_Functions;

package body Detector.Signals.Fast_Fourier_Transform_Details with SPARK_Mode is

   use Detector.Numerics.Elementary_Functions;

   function ω (
      K : in Count_Type'Base;
      N : in Count_Type'Base)
      return Complex is ((
      declare
         θ : constant Trigonometric_Input_Type :=
            Fixed_Integer (K) / Fixed_Integer (N);
      begin Complex'(
         Re => Sample_Type (Cospi (θ)),
         Im => Sample_Type (Sinpi (θ)))));

   procedure Operation (
      Left_Input   : in     Complex;
      Right_Input  : in     Complex;
      K, N         : in     Count_Type'Base;
      Left_Output  :    out Complex;
      Right_Output :    out Complex) is
      use all type Complex;
      L : constant Complex := Left_Input / 2;
      R : constant Complex := (Right_Input * ω (K, N)) / 2;
   begin
      Left_Output := L + R;
      Right_Output := L - R;
   end Operation;

   procedure Conquer (
      Buffer : in out Double_Buffer;
      Scaled :    out Boolean;
      Chunk  : in     Positive_Count_Type;
      Input  : in     Boolean) is
      Count     : constant Count_Type := Buffer'Length (2) / Chunk;
      First     : constant Count_Type := Buffer'First (2);
      In_Left   : Count_Type;
      In_Right  : Count_Type;
      Out_Left  : Count_Type;
      Out_Right : Count_Type;
   begin
      Scaled := True;
      for Chunk_Index in 0 .. Count / 2 - 1 loop
         In_Left := First + Chunk_Index * Chunk;
         In_Right := First + (Chunk_Index + Count / 2) * Chunk;
         Out_Left := First + 2 * Chunk_Index;
         Out_Right := First + (2 * Chunk_Index + 1);
         for Index in 0 .. Chunk - 1 loop
            Operation (
               Left_Input   => Buffer (Input, In_Left + Index),
               Right_Input  => Buffer (Input, In_Right + Index),
               K            => Index,
               N            => Chunk * 2,
               Left_Output  => Buffer (not Input, Out_Left + Index),
               Right_Output => Buffer (not Input, Out_Right + Index));
         end loop;
      end loop;
   end Conquer;

   function Chunk_Sizes return Chunk_Size_Array is
      Result : Chunk_Size_Array := [others => 1];
   begin
      for Index in 1 .. Bits - 2 loop
         pragma Loop_Invariant (Result (0) = 1);
         pragma Loop_Invariant (Result (0) = 2 ** 0);
         pragma Loop_Invariant (
            (for all I in 1 .. Index - 1 =>
               Result (I) = Result (I - 1) * 2));
         Result (Index) := Result (Index - 1) * 2;
      end loop;
      pragma Assert (Result (0) = 1);
      pragma Assert (
         (for all I in 1 .. Bits - 2 =>
            Result (I) = Result (I - 1) * 2));              -- B
      pragma Assert (Result (0) = 2 ** 0);                  -- A(0)
      pragma Assert (
         (for all I in 1 .. Bits - 2 =>
            2 ** (I - 1) * 2 = 2 ** I));                    -- C
      pragma Assert (
         (for all I in 1 .. Bits - 2 =>
            (if Result (I - 1) = 2 ** (I - 1)               -- A
               and then Result (I) = Result (I - 1) * 2     -- B
               and then 2 ** (I - 1) * 2 = 2 ** I           -- C
               then Result (I) = 2 ** I)));
      pragma Assert (
         (for all I in 1 .. Bits - 2 => Result (I) = 2 ** I));
      return Result;
   end Chunk_Sizes;

   procedure Lemma_Power_Of_Two_Module_Another_Lower_Power_Of_Two_Is_Zero (
      Left  : in Natural;
      Right : in Natural) is
      Sizes : constant Chunk_Size_Array := Chunk_Sizes;
   begin
      pragma Assert (2 ** Left = Sizes (Left));
      pragma Assert (2 ** Right = Sizes (Right));
      pragma Assert (2 ** Left >= 2 ** Right);
      pragma Assert (Left >= Right);
      if Left = Right then
         pragma Assert (2 ** Left mod 2 ** Right = 0);
      else
         pragma Assert (Left > Right);
         pragma Assert (
            (for all I in Left + 1 .. Right =>
               Sizes (I) = Sizes (I - 1) * 2
               and then Sizes (I) mod Sizes (I - 1) = 0
               and then Sizes (I) mod 2 ** Left = 0));
         pragma Assert (Sizes (Right) mod 2 ** Left = 0);
         pragma Assert (2 ** Left mod 2 ** Right = 0);
      end if;
   end Lemma_Power_Of_Two_Module_Another_Lower_Power_Of_Two_Is_Zero;

end Detector.Signals.Fast_Fourier_Transform_Details;
