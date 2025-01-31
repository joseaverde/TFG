package body Fold_N with SPARK_Mode is

   procedure Lemma_Add_Increases (
      Result : in Output_Real;
      Other  : in Input_Real;
      Index  : in Positive) with
      Ghost,
      Pre      => Result in First * Real (Index) .. Last * Real (Index)
         and then Index in 1 .. Size,
      Post     => Result + Other in First * Real (Index + 1)
                                 .. Last * Real (Index + 1);

   procedure Lemma_Add_Increases (
      Result : in Output_Real;
      Other  : in Input_Real;
      Index  : in Positive) is
   begin
      pragma Assert (Index in 1 .. Size);
   end Lemma_Add_Increases;

   procedure Lemma_Addition_Increases_Range (
      Accumulator : in Output_Real;
      Increment   : in Input_Real;
      Additions   : in Positive) with
      Ghost,
      Pre      => Additions in 1 .. Size
         and then Accumulator in First * Real (Additions)
                              .. Last  * Real (Additions),
      Post     => Accumulator + Increment in First * Real (Additions + 1)
                                          .. Last  * Real (Additions + 1);

   procedure Lemma_Addition_Increases_Range (
      Accumulator : in Output_Real;
      Increment   : in Input_Real;
      Additions   : in Positive) is
   begin
      null;
   end Lemma_Addition_Increases_Range;

   pragma Warnings (Off,
      "postcondition does not check the outcome of calling ""Lemma_Addition_Increases_Range""");
   function Lemma_Addition_Increases_Range (
      Accumulator : in Output_Real;
      Increment   : in Input_Real;
      Additions   : in Positive)
      return Boolean is (True) with
      Ghost,
      Pre      => Additions in 1 .. Size
         and then Accumulator in First * Real (Additions)
                              .. Last  * Real (Additions),
      Post     => Accumulator + Increment in First * Real (Additions + 1)
                                          .. Last  * Real (Additions + 1);

   function Fold_Acc (Item : in Real_Array) return Output_Array is
      Result : Output_Array (Item'Range) := [others => 0.0];
   begin
      Result (Item'First) := Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
      -- pragma Assume (
      --    (for all I in Item'First + 1 .. Index - 1 =>
      --       Result (I) in Real (I - Item'First + 1) * First
      --                  .. Real (I - Item'First + 1) * Last));
         pragma Loop_Invariant (Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (Result (Item'First) in First .. Last);
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Lemma_Addition_Increases_Range (
                  Accumulator => Result (I - 1),
                  Increment   => Item (I),
                  Additions   => Positive (I - Item'First)) and then
               Result (I - 1) + Item (I)
                  in Real (I - Item'First + 1) * First
                  .. Real (I - Item'First + 1) * Last and then
               Result (I) = Result (I - 1) + Item (I)));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I) in Real (I - Item'First + 1) * First
                          .. Real (I - Item'First + 1) * Last));
      -- pragma Loop_Invariant (
      --    (for all I in Item'First + 1 .. Index - 1 =>
      --       (Result (I - 1) in Real (I - Item'First) * First
      --                       .. Real (I - Item'First) * Last) and then 
      --       (Item (I) in Input_Real) and then
      --       (Result (I - 1) + Item (I) in Real (I - Item'First + 1) * First
      --                                  .. Real (I - Item'Last + 1) * Last)
      --       and then Result (I) = Result (I - 1) + Item (I)));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I) = Result (I - 1) + Item (I)));
      -- Lemma_Addition_Increases_Range (
      --    Result (Index - 1), Item (Index), Positive (Index - Item'First));
         Result (Index) := Result (Index - 1) + Item (Index);
      end loop;
      return Result;
   end Fold_Acc;

   function Fold_N (Item : in Real_Array) return Output_Real is
      Result : Output_Real := Item (Item'First);
   begin
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result = Fold_Acc (Item) (Index - 1));
         Result := Result + Item (Index);
      end loop;
      return Result;
   end Fold_N;

end Fold_N;
