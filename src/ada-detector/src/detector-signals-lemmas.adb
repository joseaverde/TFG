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

   function Generic_Accumulation (
      Item : in Array_Type)
      return Array_Type is
      Result : Array_Type (Item'Range) := [others => 0.0];
   begin
      Result (Item'First) := Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I - 1) in Positive (I - Item'First) * First
                              .. Positive (I - Item'First) * Last
               and then Result (I) = Result (I - 1) + Item (I)
               and then Result (I) in Positive (I - Item'First + 1) * First
                                 .. Positive (I - Item'First + 1) * Last));
         Result (Index) := Result (Index - 1) + Item (Index);
      end loop;
      return Result;
   end Generic_Accumulation;

end Detector.Signals.Lemmas;
