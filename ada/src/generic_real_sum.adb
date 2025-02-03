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
         and then Accumulator in First * Real (Additions - 1)
                              .. Last  * Real (Additions - 1),
      Post     => Accumulator + Increment in First * Real (Additions)
                                          .. Last  * Real (Additions)
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
      pragma Assume (
         Accumulator + Increment in First * Real (Additions)
                                 .. Last  * Real (Additions));
      return True;
   end Lemma_Addition_Increases_Range;

   function The_Lemma (
      Left  : in Output_Real;
      Right : in Input_Real;
      Count : in Positive)
      return Output_Real with
      Pre      => Left in Real (Count - 1) * First .. Real (Count - 1) * Last,
      Post     => The_Lemma'Result = Left + Right
         and then The_Lemma'Result in Real (Count) * First .. Real (Count) * Last;

   function The_Lemma (
      Left  : in Output_Real;
      Right : in Input_Real;
      Count : in Positive)
      return Output_Real is
   begin
      pragma Assume (Left + Right in Real (Count) * First .. Real (Count) * Last);
      pragma Assume (Real (Count) * Last in Output_Real);
      pragma Assume (Real (Count) * First in Output_Real);
      return Left + Right;
   end The_Lemma;

   function Sum_Acc (Item : in Real_Array) return Output_Array is
      Result : Output_Array (Item'Range) := [others => 0.0];
   begin
      Result (Item'First) := Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (Result (Item'First) in First .. Last);
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
                        Result (I - 1) in Real (I - Item'First) * First
                                       .. Real (I - Item'First) * Last
               and then Result (I) = The_Lemma (Result (I - 1), Item (I),
                                                Positive (I - Item'First + 1))
               and then (
                  if Result (I) = The_Lemma (Result (I - 1), Item (I),
                                             Positive (I - Item'First + 1))
                     and then The_Lemma (Result (I - 1), Item (I),
                                         Positive (I - Item'First + 1))
                              in Real (I - Item'First + 1) * First
                              .. Real (I - Item'First + 1) * Last
                     then Result (I)
                              in Real (I - Item'First + 1) * First
                              .. Real (I - Item'First + 1) * Last)));
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

      -- pragma Loop_Invariant (
      --    (for all I in Item'First + 1 .. Index - 1 =>
      --       Lemma_Addition_Increases_Range (
      --          Accumulator => Result (I - 1),
      --          Increment   => Item (I),
      --          Additions   => Positive (I - Item'First + 1)) and then
      --       Result (I - 1) + Item (I)
      --          in Real (I - Item'First + 1) * First
      --          .. Real (I - Item'First + 1) * Last and then
      --       Result (I) = Result (I - 1) + Item (I)));
      -- pragma Loop_Invariant (
      --    (for all I in Item'First + 1 .. Index - 1 =>
      --       Result (I) in Real (I - Item'First + 1) * First
      --                  .. Real (I - Item'First + 1) * Last));
      -- pragma Assert (
      --    Result (Index - 1) + Item (Index)
      --       in Real (Index - Item'First + 1) * First
      --       .. Real (Index - Item'First + 1) * Last);

end Generic_Real_Sum;
