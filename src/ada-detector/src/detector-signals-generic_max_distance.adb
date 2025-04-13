--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_max_distance.adb                      |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

function Detector.Signals.Generic_Max_Distance (
   Item : in Signal_Type)
   return Result_Type is
   use all type Normalisation.Scaled_Sample_Type;
   Min : Sample_Type := Item (Item'First);
   Max : Sample_Type := Item (Item'First);
begin
   pragma Assert (Max >= Min);
   for I in Item'First + 1 .. Item'Last loop
      pragma Loop_Invariant (Max >= Min);
      Min := Sample_Type'Min (Min, Item (I));
      Max := Sample_Type'Max (Max, Item (I));
   end loop;
   pragma Assert (Max >= Min);
   return
      (if Min >= 0.0 or else Max < 0.0
         then Result_Type (Normalisation.Denormalise (Max - Min))
         else Result_Type (Normalisation.Denormalise (Max))
               + Result_Type (-Normalisation.Denormalise (Min)));
end Detector.Signals.Generic_Max_Distance;
