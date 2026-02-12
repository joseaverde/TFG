--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-complex_types_operations.ads                 |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Numerics.Generic_Complex_Types;

package Detector.Numerics.Complex_Types_Operations with Pure, SPARK_Mode is

   generic
      with package Left_Complex is new Generic_Complex_Types (<>);
      with package Right_Complex is new Generic_Complex_Types (<>);
      with package Result_Complex is new Generic_Complex_Types (<>);
      use type Result_Complex.Fixed_Type;
   function Multiply (
      Left  : in Left_Complex.Complex;
      Right : in Right_Complex.Complex)
      return Result_Complex.Complex with
      Global => null,
      Inline => True;

   generic
      with package Input_Complex is new Generic_Complex_Types (<>);
      type Result_Type is delta <>;
   function Norm_Squared (
      Item : in Input_Complex.Complex)
      return Result_Type with
      Global => null,
      Inline => True;

end Detector.Numerics.Complex_Types_Operations;
