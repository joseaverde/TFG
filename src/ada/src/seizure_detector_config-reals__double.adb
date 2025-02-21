with Ada.Numerics.Generic_Elementary_Functions;

package body Seizure_Detector_Config.Reals with SPARK_Mode => On is

   package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Real);

   function Cos (Item : in Real) return Real is (
      Elementary_Functions.Cos (Item));
   function Sin (Item : in Real) return Real is (
      Elementary_Functions.Sin (Item));
   function Sqrt (Item : in Real) return Real is (
      Elementary_Functions.Sqrt (Item));

end Seizure_Detector_Config.Reals;
