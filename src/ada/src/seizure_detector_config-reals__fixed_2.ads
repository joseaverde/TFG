package Seizure_Detector_Config.Fixed_2 with Pure, SPARK_Mode => On is

   subtype Real is Float;

   function Cos (Item : in Real) return Real with
      Post => Cos'Result in -1.0 .. 1.0;
   function Sin (Item : in Real) return Real with
      Post => Sin'Result in -1.0 .. 1.0;
   function Rounding (Item : in Real) return Real
      renames Real'Rounding;

end Seizure_Detector_Config.Fixed_2;
