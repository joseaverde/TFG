with SPARK.Lemmas.Fixed_Point_Arithmetic;

private package Detector.Signals.Lemmas with Ghost, Pure, SPARK_Mode is

   package Sample_Lemmas is
      new SPARK.Lemmas.Fixed_Point_Arithmetic (Sample_Type);

   package Big_Sample_Lemmas is
      new SPARK.Lemmas.Fixed_Point_Arithmetic (Big_Sample_Type);

   procedure Lemma_Division_Reduces_Range (
      Num   : in Sample_Type;
      Den   : in Positive;
      First : in Sample_Type;
      Last  : in Sample_Type) with
      Ghost  => True,
      Global => null,
      Pre    => Num in First .. Last,
      Post   => Num / Den in First / Den .. Last / Den;

   Half_Sample_Type_First : constant Sample_Type := Sample_Type'First / 2;
   Half_Sample_Type_Last  : constant Sample_Type := Sample_Type'Last / 2;

   procedure Lemma_Half_Halves (Item : in Sample_Type) with
      Ghost  => True,
      Global => null,
      Post   => Item / 2 in Half_Sample_Type_First .. Half_Sample_Type_Last;

end Detector.Signals.Lemmas;
