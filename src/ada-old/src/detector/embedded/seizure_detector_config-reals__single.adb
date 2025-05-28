package body Seizure_Detector_Config.Reals with SPARK_Mode => On is

   function c_cosf (x : in Real) return Real with
      Import => True, Convention => C, External_Name => "cosf";
   function c_sinf (x : in Real) return Real with
      Import => True, Convention => C, External_Name => "sinf";
   function c_powf (x, y : in Real) return Real with
      Import => True, Convention => C, External_Name => "powf";

   function Cos (Item : in Real) return Real is (c_cosf (Item));
   function Sin (Item : in Real) return Real is (c_sinf (Item));
   function Sqrt (Item : in Real) return Real is (c_powf (Item, 0.5));

end Seizure_Detector_Config.Reals;
