--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-mean.adb                                      |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals.Generic_Accumulation;
with SPARK.Lemmas.Fixed_Point_Arithmetic;

function Detector.Signals.Mean (
   Item : in Signal_Type)
   return Sample_Type with SPARK_Mode is

   Result_Bits          : constant := Sample_Bits * 2;
   Result_Fraction_Bits : constant := Sample_Fraction_Bits;
   Result_Whole_Bits    : constant := Result_Bits - Result_Fraction_Bits - 1;
   Result_Delta         : constant := 2.0 ** (-Result_Fraction_Bits);
   type Result_Type is
      delta Result_Delta
      range -2.0 ** Result_Whole_Bits
         .. 2.0 ** Result_Whole_Bits - Result_Delta with
      Size => Result_Bits;
   type Result_Array is array (Index_Type range <>) of Result_Type;
   subtype Uniform_Result is Result_Type
      range Result_Type (Sample_Type'First) .. Result_Type (Sample_Type'Last);

   -- The reduction

   function Acc_Sum is
      new Detector.Signals.Generic_Accumulation (
      Fixed_Type => Result_Type,
      Index_Type => Index_Type,
      Array_Type => Result_Array,
      First      => Uniform_Result'First,
      Last       => Uniform_Result'Last);

   package Lemmas is new SPARK.Lemmas.Fixed_Point_Arithmetic (Result_Type);

   Mapped : constant Result_Array (Item'Range) :=
      [for I in Item'Range => Uniform_Result (Item (I))] with
      Ghost => True;
   Result : Result_Type := 0.0;
begin
   for Index in Item'Range loop
      Result := Result + Uniform_Result (Item (Index));
      pragma Loop_Invariant (Result = Acc_Sum (Mapped) (Index));
   end loop;
   Lemmas.GNAT_Lemma_Div_Is_Monotonic (
      Result, Uniform_Result'Last * Item'Length, Item'Length);
   Lemmas.GNAT_Lemma_Div_Is_Monotonic (
      Uniform_Result'First * Item'Length, Result, Item'Length);
   pragma Assert (Result / Item'Length in Uniform_Result);
   Result := Result / Item'Length;
   return Sample_Type (Result);
end Detector.Signals.Mean;
