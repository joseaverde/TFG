--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_accumuation.adb                       |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

function Detector.Signals.Generic_Accumulation (
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
end Detector.Signals.Generic_Accumulation;
