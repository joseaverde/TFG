package Reals with Pure, SPARK_Mode => On is

   Real_Sqrt_Max     : constant := 1_000.0;
   Real_Max          : constant := Real_Sqrt_Max ** 2;
   Real_Min          : constant := -Real_Max;
   Fractional_Digits : constant := 8;
   Epsilon           : constant := 2.0 ** (-Fractional_Digits);

   type Real is delta Epsilon range Real_Min .. Real_Max;

-- function Sqrt (Item : in Real) return Real with
--    Pre  => Item >= 0.0,
--    Post => Sqrt'Result in 0.0 .. Real_Sqrt_Max and then
--            abs (Item - Sqrt'Result ** 2) <= Epsilon;

end Reals;
