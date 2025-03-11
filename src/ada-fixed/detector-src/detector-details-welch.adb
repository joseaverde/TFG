with Detector.Details.Trigonometric; use Detector.Details.Trigonometric;
with Detector.Details.Fourier_Transform;
use Detector.Details.Fourier_Transform;

package body Detector.Details.Welch with SPARK_Mode => Off is

   type Trigonometric_Output_Array is
      array (Count_Type range 0 .. Welch_Size - 1)
      of Trigonometric_Output_Type;

   subtype TOT is Trigonometric_Output_Type;
   subtype TIT is Trigonometric_Input_Type;

   Hann_Window : constant Trigonometric_Output_Array :=
      [for I in Trigonometric_Output_Array'Range =>
         TOT (0.5) - TOT (0.5)
         * Cos (TIT (TIT (TIT (2.0) * TIT (I)) * TIT (π)) / TIT (Welch_Size))];
   pragma Assert (
      (for all I in Hann_Window'Range =>
         Hann_Window (I) in 0.0 .. 1.0));

   Normalisation_Factor : constant Feature_Type :=
      [for C of Hann_Window =>
         Feature_Type (C * C)]'Reduce ("+", Feature_Type (0.0));

   function Squared (Item : in Feature_Type) return Feature_Type is (
      Item * Item);

   function Norm_Squared (Item : in Complex) return Feature_Type is (
      Squared (Feature_Type (Item.Re)) + Squared (Feature_Type (Item.Im)));

   procedure Welch (
      Signal : in     Sample_Array;
      Pxx    :    out Welch_Array) is
      Factor : constant Feature_Type := Feature_Type (2.0)
                     / Feature_Type (Normalisation_Factor * Frequency);
      Input  : Fourier_Transform_Real_Array;
      Output : Complex_Array;
      Index  : Count_Type := Signal'First;
   begin
      Pxx := [others => 0.0];
      while Index <= Signal'Last - Welch_Size + 1 loop
         Input := [for I in Input'Range =>
                     Hann_Window (Hann_Window'First + I - Input'First) *
                     Signal (I - Input'First + Index)];
         Fourier_Transform.Fourier_Transform (Input, Output);
         for I in Pxx'Range loop
            Pxx (I) := @ + Norm_Squared (Output (I)) * Factor;
         end loop;
         Index := Index + (Welch_Size - Overlap);
      end loop;
      Pxx := [for I in Pxx'Range => Pxx (I) / Steps];
   end Welch;

end Detector.Details.Welch;
