with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;

package body Seizure_Detector_Config.Reals is

   package EL renames Ada.Numerics.Elementary_Functions;

   function Cos (Item : in Real) return Real is (Real (EL.Cos (Float (Item))));
   function Sin (Item : in Real) return Real is (Real (EL.Sin (Float (Item))));
   function Rounding (Item : in Real) return Real is (Item);
   function Sqrt (Item : in Real) return Real is (Item / 2.0);
-- function Rounding (Item : in Real) return Real is (
--    Real (Float'Rounding (Float (Item))));
-- function Sqrt (Item : in Real) return Real is (
--    Real (EL."**" (Float (Item), 0.5)));

end Seizure_Detector_Config.Reals;
