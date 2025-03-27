with Detector.Batches;

package Default_Detector with SPARK_Mode => Off is

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

   Min_Feature           : constant := 0.0;
   Max_Feature           : constant := 4.0 * (Max_Sample - Min_Sample);
   Feature_Bits          : constant := Detector.Bits;
   Feature_Whole_Bits    : constant := 1 + Log_2 (Count_Type (Max_Feature));
   Feature_Fraction_Bits : constant := Feature_Bits - Feature_Whole_Bits - 1;
   Feature_Delta         : constant := 2.0 ** (-Feature_Fraction_Bits);
   type Feature_Type is
      delta Feature_Delta
      range 0.0 .. 2.0 ** Feature_Whole_Bits - Feature_Delta with
      Size => Feature_Bits;

   package Batches is
      new Detector.Batches (
      Sample_Type        => Sample_Type,
      Feature_Type       => Feature_Type,
      Samples_Per_Stride => 256,
      Strides_Per_Epoch  => 5,
      Max_Patterns       => 5);

end Default_Detector;
