--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    default_detector.ads                                           |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Batches;

package Default_Detector with SPARK_Mode => On is

   use Detector;

   Min_Sample           : constant := -10_000.0;
   Max_Sample           : constant := 10_000.0;
   Sample_Bits          : constant := Detector.Bits;
   Sample_Whole_Bits    : constant :=
      1 + Log_2 (Count_Type'Max (Count_Type (abs Min_Sample),
                                 Count_Type (abs Max_Sample)));
   Sample_Fraction_Bits : constant := Bits - Sample_Whole_Bits - 1;
   Sample_Delta         : constant := 2.0 ** (-Sample_Fraction_Bits);
   type Sample_Type is
      delta Sample_Delta
      range Min_Sample .. Max_Sample with
      Size => Sample_Bits;

   Feature_Bits          : constant := Detector.Bits * 2;
   Feature_Fraction_Bits : constant := Sample_Bits;
   Feature_Whole_Bits    : constant := Feature_Bits - Sample_Bits - 1;
   Feature_Delta         : constant := 2.0 ** (-Feature_Fraction_Bits);
   type Feature_Type is
      delta Feature_Delta
      range 0.0 .. 2.0 ** Feature_Whole_Bits - Feature_Delta with
      Size => Feature_Bits;

   Samples_Per_Stride : constant := 256;
   Stride_Size        : constant := Samples_Per_Stride;
   Strides_Per_Epoch  : constant := 5;
   Epoch_Size         : constant := Strides_Per_Epoch * Stride_Size;
   Welch_Size         : constant := 512;

   package Batches is
      new Detector.Batches (
      Sample_Type        => Sample_Type,
      Feature_Type       => Feature_Type,
      Samples_Per_Stride => Samples_Per_Stride,
      Strides_Per_Epoch  => Strides_Per_Epoch,
      Welch_Window_Size  => Welch_Size,
      Max_Patterns       => 5);

end Default_Detector;
