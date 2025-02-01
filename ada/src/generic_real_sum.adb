package body Generic_Real_Sum with SPARK_Mode => On is

   pragma Warnings (Off, "postcondition does not check the outcome of calling ""Lemma_Addition_Increases_Range""");
   pragma Warnings (Off, "postcondition does not mention function result");
   function Lemma_Addition_Increases_Range (
      Accumulator : in Output_Real;
      Increment   : in Input_Real;
      Additions   : in Positive)
      return Boolean with
      Ghost,
      Pre      => Additions in 1 .. Size
         and then Accumulator in First * Real (Additions)
                              .. Last  * Real (Additions),
      Post     => Accumulator + Increment in First * Real (Additions + 1)
                                          .. Last  * Real (Additions + 1)
         and then Lemma_Addition_Increases_Range'Result = True;

   function Lemma_Addition_Increases_Range (
      Accumulator : in Output_Real;
      Increment   : in Input_Real;
      Additions   : in Positive)
      return Boolean is
   begin
      pragma Assert (Additions in 1 .. Size);
      pragma Assume (
         (for all I in 1 .. Size =>
            First * Real (I) + First = First * Real (I + 1)));
      pragma Assume (
         (for all I in 1 .. Size =>
            Last * Real (I) + Last = Last * Real (I + 1)));
      pragma Assert (First * Real (Additions) + First
                     = First * Real (Additions + 1));
      pragma Assert (Last * Real (Additions) + Last
                     = Last * Real (Additions + 1));
      pragma Assert (Increment in First .. Last);
      pragma Assert (
         Accumulator + Increment in First * Real (Additions + 1)
                                 .. Last  * Real (Additions + 1));
      return True;
   end Lemma_Addition_Increases_Range;

   function Sum_Acc (Item : in Real_Array) return Output_Array is
      Result : Output_Array (Item'Range) := [others => 0.0];
   begin
      Result (Item'First) := Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
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
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I) = Result (I - 1) + Item (I)));
         Result (Index) := Result (Index - 1) + Item (Index);
      end loop;
      return Result;
   end Sum_Acc;

   function Sum (Item : in Real_Array) return Output_Real is
      Result : Output_Real := Item (Item'First);
   begin
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result = Sum_Acc (Item) (Index - 1));
         Result := Result + Item (Index);
      end loop;
      return Result;
   end Sum;

end Generic_Real_Sum;
