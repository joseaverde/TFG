package Detector.Module.Fixed_32_p_8 with Pure is

   Fixed_Delta : constant := 2.0 ** (-8);
   Fixed_Bound : constant := 2.0 ** (32 -8 - 1);

   type Fixed is
      delta Fixed_Delta
      range -Fixed_Bound .. Fixed_Bound - Fixed_Delta with
      Size => 32;

end Detector.Module.Fixed_32_p_8;
