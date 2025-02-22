with Ada.Numerics; use Ada.Numerics;

package body Seizure_Detector_Config.Reals is

   type Int is new Long_Integer;

   function Cos (Item : in Real) return Real is (0.0);
   function Sin (Item : in Real) return Real is (Item);
   function Rounding (Item : in Real) return Real is (Item);
   function Sqrt (Item : in Real) return Real is (1.0);

end Seizure_Detector_Config.Reals;
