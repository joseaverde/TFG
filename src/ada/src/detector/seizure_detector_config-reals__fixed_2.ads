package Seizure_Detector_Config.Reals with Pure, SPARK_Mode => On is

   Bits  : constant := 64;
   Right : constant := 24;
   Left  : constant := Bits - Right - 1;
   Small : constant := 2.0 ** (-Right);

   type Real is delta Small range -2.0 ** Left .. 2.0 ** Left - Small with
      Size => Bits;

   function Cos (Item : in Real) return Real with
      Post => Cos'Result in -1.0 .. 1.0;
   function Sin (Item : in Real) return Real with
      Post => Sin'Result in -1.0 .. 1.0;
   function Rounding (Item : in Real) return Real;
   function Sqrt (Item : in Real) return Real;
   function Squared (Item : in Real) return Real is (Item * Item);
   overriding function "-" (Right : in Real) return Real is (0.0 - Right);

end Seizure_Detector_Config.Reals;
