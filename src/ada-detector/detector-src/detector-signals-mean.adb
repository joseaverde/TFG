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
   type Result_Array is array (Positive_Count_Type range <>) of Result_Type;
   subtype Uniform_Result is Result_Type
      range Result_Type (Sample_Type'First) .. Result_Type (Sample_Type'Last);

   -- The reduction

   function Acc_Sum (
      Item : in Signal_Type)
      return Result_Array with
      Ghost    => True,
      Global   => null,
      Pre      => Item'Length > 0,
      Post     => Acc_Sum'Result'First = Item'First
         and then Acc_Sum'Result'Length = Item'Length
         and then Acc_Sum'Result (Item'First) = Result_Type (Item (Item'First))
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Sum'Result (I - 1)
                        in Uniform_Result'First * Positive (I - Item'First)
                        .. Uniform_Result'Last * Positive (I - Item'First)
                     and then Acc_Sum'Result (I) = Acc_Sum'Result (I - 1)
                                                 + Result_Type (Item (I)))
         and then Acc_Sum'Result (Item'Last)
                     in Uniform_Result'First * Item'Length
                     .. Uniform_Result'Last * Item'Length;

   function Acc_Sum (
      Item : in Signal_Type)
      return Result_Array is
      Result : Result_Array (Item'Range) := [others => 0.0];
   begin
      Result (Item'First) := Result_Type (Item (Item'First));
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (
            Result (Item'First) = Result_Type (Item (Item'First)));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I - 1)
                  in Uniform_Result'First * Positive (I - Item'First)
                  .. Uniform_Result'Last * Positive (I - Item'First)
               and then Result_Type (Item (I)) in Uniform_Result
               and then Result (I) = Result (I - 1) + Result_Type (Item (I))
               and then Result (I)
                  in Uniform_Result'First * Positive (I - Item'First + 1)
                  .. Uniform_Result'Last * Positive (I - Item'First + 1)));
         Result (Index) := Result (Index - 1) + Result_Type (Item (Index));
      end loop;
      return Result;
   end Acc_Sum;

   package Lemmas is new SPARK.Lemmas.Fixed_Point_Arithmetic (Result_Type);

-- procedure Lemma_Division_Is_Monotonic (
--    Left  : in Result_Type;
--    Right : in Result_Type;
--    Denom : in Positive) with
--    Global => null,
--    Ghost  => True,
--    Pre    => Left <= Right,
--    Post   => Left / Denom <= Right / Denom;
-- pragma Annotate (GNATprove, Intentional, "postcondition",
--                  "GNAT-specific lemma, as Ada RM does not guarantee it");
-- -- Source: spark-lemmas-fixed_point_arithmetic.ads
-- -- I didn't want to include the lemma

-- procedure Lemma_Division_Is_Monotonic (
--    Left  : in Result_Type;
--    Right : in Result_Type;
--    Denom : in Positive) is
-- begin
--    null;
-- end Lemma_Division_Is_Monotonic;

   Result : Result_Type := 0.0;
begin
   for Index in Item'Range loop
      Result := Result + Result_Type (Item (Index));
      pragma Loop_Invariant (Result = Acc_Sum (Item) (Index));
   end loop;
   Lemmas.GNAT_Lemma_Div_Is_Monotonic (
      Result, Uniform_Result'Last * Item'Length, Item'Length);
   Lemmas.GNAT_Lemma_Div_Is_Monotonic (
      Uniform_Result'First * Item'Length, Result, Item'Length);
   pragma Assert (Result / Item'Length in Uniform_Result);
   Result := Result / Item'Length;
   return Sample_Type (Result);
end Detector.Signals.Mean;
