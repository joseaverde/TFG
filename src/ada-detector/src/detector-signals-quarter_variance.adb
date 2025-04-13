--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-quarter_variance.adb                          |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals.Lemmas, Detector.Signals.Mean;
with Detector.Signals.Generic_Accumulation;
with SPARK.Lemmas.Fixed_Point_Arithmetic;

function Detector.Signals.Quarter_Variance (
   Item : in Signal_Type)
   return Big_Sample_Type with SPARK_Mode is

   -- Create a type for the accumulator that has enough bits in the whole part
   -- to add as many as Max_Signal_Length elements. One bit for the sign. And
   -- the rest is the max precision we are working with.

   Result_Bits          : constant := Big_Sample_Bits;
   Result_Whole_Bits    : constant := Log_2 (Max_Signal_Length) + 1;
   Result_Fraction_Bits : constant := Result_Bits - Result_Whole_Bits - 1;
   Result_Delta         : constant := 2.0 ** (-Result_Fraction_Bits);
   type Result_Type is
      delta Result_Delta
      range -2.0 ** Result_Whole_Bits
         .. 2.0 ** Result_Whole_Bits - Result_Delta with
      Size => Result_Bits;
   type Result_Array is array (Index_Type range <>) of Result_Type;

   package RLemmas is new SPARK.Lemmas.Fixed_Point_Arithmetic (Result_Type);

   subtype Uniform_Result is Result_Type
      range 0.0 .. Result_Type (Sample_Type'Last);

   -- We compute square of the difference between the element and the mean. We
   -- want to do it as fast as possible. And we want to prove it doesn't
   -- overflow.

   function Difference_Squared (
      Left  : in Sample_Type;
      Right : in Sample_Type)
      return Uniform_Result with
      Global => null,
      Post   => Difference_Squared'Result
                  = (Left / 2 - Right / 2) * (Left / 2 - Right / 2);

   function Difference_Squared (
      Left  : in Sample_Type;
      Right : in Sample_Type)
      return Uniform_Result is
      Item : Sample_Type;
   begin
      Lemmas.Lemma_Half_Halves (Left);
      Lemmas.Lemma_Half_Halves (Right);
      Item := Left / 2 - Right / 2;
      return Item * Item;
   end Difference_Squared;

   -- The reduction is done following the usual modus-operandi. A ghost
   -- function that returns the partial results of the accumulation of
   -- normalised numbers.

   function Acc_Sum is new Detector.Signals.Generic_Accumulation (
      Fixed_Type => Result_Type,
      Index_Type => Index_Type,
      Array_Type => Result_Array,
      First      => 0.0,
      Last       => Uniform_Result'Last);

   -- Finally we create a ghost function that applies the transformation to
   -- all the elements of the array so the accumulator function can use it.

   function Map (
      Item : in Signal_Type;
      μ    : in Sample_Type)
      return Result_Array with
      Ghost    => True,
      Global   => null,
      Pre      => Item'Length > 0,
      Post     => Map'Result'First = Item'First
         and then Map'Result'Length = Item'Length
         and then (for all Index in Item'Range =>
                     Map'Result (Index) = Difference_Squared (Item (Index), μ)
                     and then Map'Result (Index) in Uniform_Result);

   function Map (
      Item : in Signal_Type;
      μ    : in Sample_Type)
      return Result_Array is
      Result : Result_Array (Item'Range) := [others => 0.0];
   begin
      for Index in Item'Range loop
         Result (Index) := Difference_Squared (Item (Index), μ);
         pragma Loop_Invariant (
            (for all I in Item'First .. Index =>
               Result (I) = Difference_Squared (Item (I), μ)
               and then Result (I) in Uniform_Result));
      end loop;
      return Result;
   end Map;

   -- Declare the variables and the ghost ones for the proof.

   μ      : constant Sample_Type := Mean (Item);
   Mapped : constant Result_Array := Map (Item, μ) with Ghost;
   Result : Result_Type;

begin

   Result := Difference_Squared (Item (Item'First), μ);
   for Index in Item'First + 1 .. Item'Last loop
      pragma Loop_Invariant (Result = Acc_Sum (Mapped) (Index - 1));
      pragma Loop_Invariant (
         Mapped (Index) = Difference_Squared (Item (Index), μ));
      Result := Result + Difference_Squared (Item (Index), μ);
   end loop;
   pragma Assert (Result in 0.0 .. Uniform_Result'Last * Item'Length);
   RLemmas.GNAT_Lemma_Div_Is_Monotonic (
      Result, Uniform_Result'Last * Item'Length, Item'Length);
   pragma Assert (Result / Item'Length in 0.0 .. Uniform_Result'Last);
   Result := Result / Item'Length;
   pragma Assert (Result in 0.0 .. Uniform_Result'Last);

   return Big_Sample_Type (Result);

end Detector.Signals.Quarter_Variance;
