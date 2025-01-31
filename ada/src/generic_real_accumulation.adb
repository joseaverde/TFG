package body Generic_Real_Accumulation is

   function Low_Bounds (Item : in Input_Array) return Output_Array is
      Result : Output_Array (Item'Range)
   begin
      for I

-- function Partial_Accumulate (Item : in Input_Array) return Output_Array is
--    Low    : Output_Array (Item'Range) := [others => 0.0];
--    High   : Output_Array (Item'Range) := [others => 0.0];
--    Result : Output_Array (Item'Range) := [others => 0.0];
-- begin
--    Result (Item'First) := Output_Real (Item (Item'First));
--    Low (Item'First) := First;
--    High (Item'First) := Last;
--    for Index in Item'First + 1 .. Item'Last loop

--       -- Head equal
--       pragma Loop_Invariant (
--          Result (Item'First) = Output_Real (Item (Item'First)));
--       pragma Loop_Invariant (Low (Item'First) = First);
--       pragma Loop_Invariant (High (Item'First) = Last);

--       -- Tail equal.
--       pragma Loop_Invariant (
--          (for all I in Item'First + 1 .. Index - 1 =>
--             Result (I) = Result (I - 1) + Output_Real (Item (I))));
--       pragma Loop_Invariant (
--          (for all I in Item'First + 1 .. Index - 1 =>
--             Low (I) = Low (I - 1) + First));
--       pragma Loop_Invariant (
--          (for all I in Item'First + 1 .. Index - 1 =>
--             High (I) = High (I - 1) + Last));

--       -- Lower bounds
--       -- Result within bounds
--       pragma Loop_Invariant (
--          (for all I in Item'First .. Index - 1 =>
--             Result (I) >= Low (I)));
--       pragma Loop_Invariant (
--          (for all I in Item'First .. Index - 1 =>
--             Result (I) <= High (I)));

--       Result (Index) := Result (Index - 1) + Output_Real (Item (Index));
--       Low (Index) := Low (Index - 1) + First;
--       High (Index) := High (Index - 1) + Last;
--    end loop;
--    return Result;
-- end Partial_Accumulate;

end Generic_Real_Accumulation;
