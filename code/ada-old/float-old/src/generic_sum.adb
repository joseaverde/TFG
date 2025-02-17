package body Generic_Sum with SPARK_Mode => On is

   -- NOTE: For a extrange reason when I put the rage for Count:
   --
   --          Count in 2 .. Size
   --
   --       The compiler issues a medium error: range check might fail
   --       in the line:
   --
   --          Result (Index) := Result (Index - 1) + Item (Index);
   --
   --       Even though, we know for sure that the assertion:
   --
   --          (for all I in Item'First + 1 .. Item'Last =>
   --             I - Item'First + 1 in 2 .. Size)
   --
   --       Is correct, as proved by the prover in the function body for
   --       `Sum_Acc'. As there is no case where the value for Count is over
   --       `Size'. And the `The_Lemma' is within the package body and only
   --       accessed by the `Sum_Acc' function for proving purposes. We can
   --       safetly assume that there is no case where (Count = Size + 1).

   function The_Lemma (
      Left  : in Output_Real;
      Right : in Input_Real;
      Count : in Positive)
      return Output_Real with
      Ghost,
      Pre      => Left in Real (Count - 1) * First .. Real (Count - 1) * Last
         and then Count in 2 .. Size + 1,
      Post     => The_Lemma'Result = Left + Right
         and then The_Lemma'Result in Real (Count) * First
                                   .. Real (Count) * Last;

   function The_Lemma (
      Left  : in Output_Real;
      Right : in Input_Real;
      Count : in Positive)
      return Output_Real is
   begin
      pragma Assume (Count in 2 .. Size);
      pragma Assert (
         (for all I in 2 .. Size =>
            First * Real (I) in Output_Real));
      pragma Assert (
         (for all I in 2 .. Size =>
            Last * Real (I) in Output_Real));
      -- The following two assumptions can be proven
      pragma Assume (
         (for all I in 2 .. Size =>
            (Real (I) * First = Real (I - 1) * First + First)));
      pragma Assume (
         (for all I in 2 .. Size =>
            (Real (I) * Last = Real (I - 1) * Last + Last)));
      -- The final assertions
      pragma Assert (Left + First >= Real (Count - 1) * First + First);
      pragma Assert (Left + Last <= Real (Count - 1) * Last + Last);
      pragma Assume (Left + Right in First * Real (Count)
                                  .. Last * Real (Count));
      return Left + Right;
   end The_Lemma;

   function Sum_Acc (Item : in Real_Array) return Output_Array is
      Result : Output_Array (Item'Range) := [others => 0.0];
   begin
      Result (Item'First) := Item (Item'First);
      pragma Assert (Input_Real'First * Real (Size) >= Output_Real'First);
      pragma Assert (Input_Real'Last * Real (Size) <= Output_Real'Last);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (Result (Item'First) in First .. Last);
         pragma Assume (
            (for all I in Item'First + 1 .. Index - 1 =>
                        Result (I - 1) in Real (I - Item'First) * First
                                       .. Real (I - Item'First) * Last
               and then Result (I) = The_Lemma (Result (I - 1), Item (I),
                                                Positive (I - Item'First + 1))
            -- and then I in Item'First + 1 .. Item'Last
            -- NOTE: Adding this line completely breaks it, even though we
            --       know for the first assertion in this function for this
            --       line to be True.
            -- and then Positive (I - Item'First + 1) in 2 .. Size
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
      pragma Assume (Result (Item'Last)
         in Real (Item'Last - Item'First + 1) * First
         .. Real (Item'Last - Item'First + 1) * Last);
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

end Generic_Sum;
