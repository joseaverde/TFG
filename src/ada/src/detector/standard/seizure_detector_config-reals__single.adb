with Ada.Numerics.Generic_Elementary_Functions;

package body Seizure_Detector_Config.Reals with SPARK_Mode => On is

   package EF is new Ada.Numerics.Generic_Elementary_Functions (Real);

   function Cos (Item : in Real) return Real is (EF.Cos (Item));
   function Sin (Item : in Real) return Real is (EF.Sin (Item));
   function Sqrt (Item : in Real) return Real is (EF.Sqrt (Item));

end Seizure_Detector_Config.Reals;
