package Seizure_Detector_Config.Reals with Pure, SPARK_Mode => On is

   type Real is new Float;

   function Cos (Item : in Real) return Real with
      Post => Cos'Result in -1.0 .. 1.0;
   function Sin (Item : in Real) return Real with
      Post => Sin'Result in -1.0 .. 1.0;
   function Sqrt (Item : in Real) return Real;
   function Rounding (Item : in Real) return Real
      renames Real'Rounding;
   function Squared (Item : in Real) return Real is (Item ** 2);

end Seizure_Detector_Config.Reals;
