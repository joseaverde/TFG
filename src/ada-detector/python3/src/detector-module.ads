with Interfaces.C;
with Detector.Batches;

package Detector.Module is

   -- Sample type

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

   -- Feature type

   Feature_Bits          : constant := Detector.Bits * 2;
   Feature_Fraction_Bits : constant := Sample_Bits;
   Feature_Whole_Bits    : constant := Feature_Bits - Sample_Bits - 1;
   Feature_Delta         : constant := 2.0 ** (-Feature_Fraction_Bits);
   type Feature_Type is
      delta Feature_Delta
      range 0.0 .. 2.0 ** Feature_Whole_Bits - Feature_Delta with
      Size => Feature_Bits;

   -- For interfacing with C

   Long_Bits : constant := Interfaces.C.long'Size;
   type Fixed_Long is delta 1.0
      range -2.0 ** (Long_Bits - 1) .. 2.0 ** (Long_Bits - 1) - 1.0 with
   Size => Long_Bits;

   -- To share classes with 

end Detector.Module;
