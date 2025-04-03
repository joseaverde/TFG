package body Detector.Signals.Lemmas with SPARK_Mode is

   procedure Lemma_Division_Reduces_Range (
      Num   : in Sample_Type;
      Den   : in Positive;
      First : in Sample_Type;
      Last  : in Sample_Type) is
   begin
      Sample_Lemmas.GNAT_Lemma_Div_Is_Monotonic (First, Num, Den);
      Sample_Lemmas.GNAT_Lemma_Div_Is_Monotonic (Num, Last, Den);
   end Lemma_Division_Reduces_Range;

   procedure Lemma_Half_Halves (Item : in Sample_Type) is
      First : constant Sample_Type := Sample_Type'First;
      Last  : constant Sample_Type := Sample_Type'Last;
      Den   : constant Positive    := 2;
   begin
      pragma Assert (Item in First .. Last);
      Lemma_Division_Reduces_Range (Item, Den, First, Last);
      pragma Assert (Item / Den in First / Den .. Last / Den);
      pragma Annotate (
         GNATprove,
         False_Positive,
         "assertion might fail",
         "Just see the postcondition of ""Lemma_Division_Reduces_Range""");
      pragma Assert (First / Den = Half_Sample_Type_First);
      pragma Assert (Last / Den = Half_Sample_Type_Last);
   end Lemma_Half_Halves;

end Detector.Signals.Lemmas;
