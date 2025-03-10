with Detector.Details.Variance;

package Detector.Details.Square_Root with SPARK_Mode => On is

   use type Variance.Real;

   Sqrt_Result_Bits     : constant := Variance.Real_Bits;
   Sqrt_Result_Mantissa : constant := Variance.Real_Mantissa * 2;
   Sqrt_Result_Delta    : constant := 2.0 ** (-Sqrt_Result_Mantissa);

   type Sqrt_Result is
      delta Sqrt_Result_Delta
      range -2.0 ** (Sqrt_Result_Bits - Sqrt_Result_Mantissa - 1)
         .. 2.0 ** (Sqrt_Result_Bits - Sqrt_Result_Mantissa - 1)
            - Sqrt_Result_Delta with
      Size => Sqrt_Result_Bits;

   function Sqrt (
      Item : in Variance.Real)
      return Sqrt_Result with
      Global => null,
      Pre      => Item in 0.0 .. Variance.Max_Sq,
      Post     => Sqrt'Result * Sqrt'Result <= Item
         and then Sqrt'Result in 0.0 .. Sqrt_Result (Variance.Max_Abs);

end Detector.Details.Square_Root;
