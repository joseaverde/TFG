package Seizure_Detector_Config.Reals with Pure, SPARK_Mode => On is

   subtype Real is Long_Float;

   function Cos (Item : in Real) return Real with
      Post => Cos'Result in -1.0 .. 1.0;
   function Sin (Item : in Real) return Real with
      Post => Sin'Result in -1.0 .. 1.0;
   function Rounding (Item : in Real) return Real
      renames Real'Rounding;
   function Sqrt (Item : in Real) return Real;

end Seizure_Detector_Config.Reals;
