--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-saturating_arithmetic.ads                    |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package Detector.Numerics.Saturating_Arithmetic with Pure, SPARK_Mode is

   generic
      type Left_Fixed_Type is delta <>;
      type Right_Fixed_Type is delta <>;
      type Result_Fixed_Type is delta <>;
   function Generic_Saturated_Multiplication (
      Left  : in Left_Fixed_Type;
      Right : in Right_Fixed_Type)
      return Result_Fixed_Type with
      Pre      => Left_Fixed_Type'Delta <= 1.0
         and then Right_Fixed_Type'Delta <= 1.0
         and then Result_Fixed_Type'Delta <= 1.0,
      Inline => True,
      Global => null;

end Detector.Numerics.Saturating_Arithmetic;
