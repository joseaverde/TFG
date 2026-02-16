--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-fast_fourier_transform_details.adb            |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Numerics.Elementary_Functions;
with Detector.Numerics.Complex_Types_Operations;

pragma Overflow_Mode (General => Strict, Assertions => Eliminated);

package body Detector.Signals.Fast_Fourier_Transform_Details with SPARK_Mode is

   use Detector.Numerics.Elementary_Functions;

   package Trigonometric_Output_Complex is
      new Detector.Numerics.Generic_Complex_Types (
      Fixed_Type => Trigonometric_Output_Type);

   function "*" is
      new Detector.Numerics.Complex_Types_Operations.Multiply (
      Left_Complex   => Complex_Types,
      Right_Complex  => Trigonometric_Output_Complex,
      Result_Complex => Complex_Types);

   function ω (
      K : in Count_Type;
      N : in Positive_Count_Type)
      return Trigonometric_Output_Complex.Complex with
      Global => null,
      Inline => True,
      Pre    => K < N;

   procedure Lemma_X_Mod_X_Is_Always_Zero (X, Y : in Positive_Count_Type) with
      Pre  => X = Y,
      Post => X mod Y = 0,
      Global => null, Ghost, Always_Terminates;

   procedure Lemma_Modulo_Is_Transitive (X, Y, Z : in Positive_Count_Type) with
      Pre  => X mod Y = 0 and then Y mod Z = 0,
      Post => X mod Z = 0,
      Global => null, Ghost, Always_Terminates;

   procedure Lemma_Expand_Denominator (X, Y, Z : in Positive_Count_Type) with
      Pre  => Y = Z,
      Post => X / Y = X / Z,
      Global => null, Ghost, Always_Terminates;

   procedure Lemma_Lower_Positive_Factor_In_Product_Is_Lower (
      Left      : in Positive_Count_Type;
      Right_1   : in Count_Type;
      Right_2   : in Count_Type;
      Result    : in Count_Type) with
      Pre    => Left * Right_1 = Result and then Right_2 <= Right_1,
      Post   => Left * Right_2 <= Result,
      Global => null, Ghost, Always_Terminates;

   procedure Lemma_Lower_Positive_Factor_In_Product_Is_Lower (
      Left      : in Positive_Count_Type;
      Right_1   : in Count_Type;
      Right_2   : in Count_Type;
      Result    : in Count_Type) is
      null;

   -->> Implementation <<-

   function ω (
      K : in Count_Type;
      N : in Positive_Count_Type)
      return Trigonometric_Output_Complex.Complex is ((
      declare
         θ : constant Trigonometric_Input_Type :=
            (Fixed_Integer (K)) / Fixed_Integer (N);
      begin (Cospi (θ), Sinpi (θ))));

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
      Chunk_Size : constant Positive_Count_Type := Chunk;
      Count      : constant Positive_Count_Type :=
         Buffer'Length (2) / Chunk_Size;
      Last_Chunk : constant Count_Type := Count / 2 - 1;
      First      : constant Count_Type := Buffer'First (2);
      In_Left    : Count_Type;
      In_Right   : Count_Type;
      Out_Left   : Count_Type;
      Out_Right  : Count_Type;
   begin
      Scaled := True;
      pragma Assert (First = 0);
      pragma Assert (Buffer'Length (2) mod Chunk_Size = 0);
      pragma Assert (Count mod 2 = 0);
      pragma Assert (Count * Chunk_Size = Buffer'Length (2));
      pragma Assert (Count * Chunk_Size / 2 = Buffer'Length (2) / 2);
      for Chunk in 0 .. Last_Chunk loop
         Lemma_Lower_Positive_Factor_In_Product_Is_Lower (
            Chunk_Size, Last_Chunk, Chunk, Buffer'Length (2) / 2 - Chunk_Size);
         In_Left := First + Chunk * Chunk_Size;
         In_Right := First + (Chunk + Count / 2) * Chunk_Size;
         Out_Left := First + (2 * Chunk) * Chunk_Size;
         Out_Right := Out_Left + Chunk_Size;
         pragma Assert (
            In_Left in First .. First + Buffer'Length (2) / 2 - Chunk_Size);
         pragma Assert (
            In_Right in First + Buffer'Length (2) / 2
                     .. First + Buffer'Length (2) - Chunk_Size);
         pragma Assert (
            Out_Left in First .. First + Buffer'Length (2) - 2 * Chunk_Size);
         pragma Assert (
            Out_Right in First + Chunk_Size
                      .. First + Buffer'Length (2) - Chunk_Size);
         for Index in 0 .. Chunk_Size - 1 loop
            Operation (
               Left_Input   => Buffer (Input, In_Left + Index),
               Right_Input  => Buffer (Input, In_Right + Index),
               K            => Index,
               N            => Chunk_Size,
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

   procedure Lemma_X_Mod_X_Is_Always_Zero (X, Y : in Positive_Count_Type) is
      null;

   procedure Lemma_Expand_Denominator (X, Y, Z : in Positive_Count_Type) is
      null;

   procedure Lemma_Modulo_Is_Transitive (X, Y, Z : in Positive_Count_Type) is
      A : constant Positive_Count_Type := X / Y;
      B : constant Positive_Count_Type := Y / Z;
   begin
      pragma Assert (X mod Y = 0);
      pragma Assert (X = A * Y);
      pragma Assert (Y mod Z = 0);
      pragma Assert (Y = B * Z);
      Lemma_Expand_Denominator (X, Y, B * Z);
      pragma Assert (X / Y = X / (B * Z));

      pragma Assert (X = A * B * Z);
      pragma Assert (X mod Z = 0);
   end Lemma_Modulo_Is_Transitive;

   procedure Lemma_Power_Of_Two_Module_Another_Lower_Power_Of_Two_Is_Zero (
      Left  : in Natural;
      Right : in Natural) is
      Sizes : constant Chunk_Size_Array := Chunk_Sizes with Ghost;
   begin
      pragma Assert (2 ** Left = Sizes (Left));
      pragma Assert (2 ** Right = Sizes (Right));
      pragma Assert (2 ** Left >= 2 ** Right);
      pragma Assert (Left >= Right);

      if Left = Right then
         pragma Assert (2 ** Left = 2 ** Right);
         Lemma_X_Mod_X_Is_Always_Zero (2 ** Left, 2 ** Right);
         pragma Assert (2 ** Left mod 2 ** Right = 0);
      elsif Right = 0 then
         pragma Assert (2 ** Right = 1);
         pragma Assert (2 ** Left mod 2 ** Right = 0);
      else
         pragma Assert (Left > Right);
         pragma Assert (Right > 0);

         for I in Right + 1 .. Left loop
            pragma Loop_Invariant (Sizes (I) = 2 ** I);
            pragma Loop_Invariant (Sizes (I) = Sizes (I - 1) * 2);
            pragma Loop_Invariant (Sizes (I) mod Sizes (I - 1) = 0);
            pragma Loop_Invariant (
               (for all J in Right .. I - 1 =>
                  Sizes (J) mod Sizes (Right) = 0));
            pragma Loop_Invariant (Sizes (I - 1) mod Sizes (Right) = 0);
            Lemma_Modulo_Is_Transitive (
               Sizes (I), Sizes (I - 1), Sizes (Right));
         end loop;

         pragma Assert (2 ** Left mod 2 ** Right = 0);
      end if;
   end Lemma_Power_Of_Two_Module_Another_Lower_Power_Of_Two_Is_Zero;

   procedure Lemma_Division_Then_Multiply (
      X : in Positive_Count_Type;
      Y : in Positive_Count_Type;
      Z : in Positive_Count_Type;
      W : in Positive_Count_Type) with
      Pre      => X / Z = X / (Y * W) and then Z = Y * W
         and then X mod Z = 0 and then X mod Y = 0,
      Post     => X / Z * W = X / Y,
      Global => null, Ghost, Always_Terminates;

   procedure Lemma_Division_Then_Multiply (
      X : in Positive_Count_Type;
      Y : in Positive_Count_Type;
      Z : in Positive_Count_Type;
      W : in Positive_Count_Type) is
      A : constant Positive_Count_Type := X / Z;
      B : constant Positive_Count_Type := X / Y;
   begin
      pragma Assert (A = X / (Y * W));
      pragma Assert (B = X / Y);
      pragma Assert (A = B / W);
      pragma Assert (A * W = B);
   end Lemma_Division_Then_Multiply;

   procedure Lemma_Quotient_Of_Powers_Of_Two_Is_Sometimes_A_Power_Of_Two (
      Left  : in Natural;
      Right : in Natural) is
      Sizes : constant Chunk_Size_Array := Chunk_Sizes with Ghost;
   begin
      pragma Assert (Left < Right);
      pragma Assert (Left + 1 <= Right);
      pragma Assert (Sizes (Left) = 2 ** Left);
      pragma Assert (Sizes (Right) = 2 ** Right);
      pragma Assert (Sizes (Left + 1) = 2 ** (Left + 1));
      pragma Assert (Sizes (Left + 1) = Sizes (Left) * 2);
      Lemma_Power_Of_Two_Module_Another_Lower_Power_Of_Two_Is_Zero (
         Left => Right, Right => Left);
      Lemma_Power_Of_Two_Module_Another_Lower_Power_Of_Two_Is_Zero (
         Left => Right, Right => Left + 1);
      pragma Assert (Sizes (Right) mod Sizes (Left + 1) = 0);
      pragma Assert (Sizes (Right) mod Sizes (Left) = 0);
      pragma Assert (Sizes (Right) / Sizes (Left + 1)
                     = Sizes (Right) / (Sizes (Left) * 2));
      Lemma_Division_Then_Multiply (
         Sizes (Right), Sizes (Left), Sizes (Left + 1), 2);
      pragma Assert (Sizes (Right) / Sizes (Left + 1) * 2
                     = Sizes (Right) / Sizes (Left));

      pragma Assert ((Sizes (Right) / Sizes (Left)) mod 2 = 0);
      pragma Assert ((2 ** Right / 2 ** Left) mod 2 = 0);
   end Lemma_Quotient_Of_Powers_Of_Two_Is_Sometimes_A_Power_Of_Two;

end Detector.Signals.Fast_Fourier_Transform_Details;
