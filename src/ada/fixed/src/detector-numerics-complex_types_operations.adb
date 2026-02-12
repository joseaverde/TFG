--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-complex_types_operations.adb                 |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Numerics.Complex_Types_Operations with SPARK_Mode => Off is

   function Multiply (
      Left  : in Left_Complex.Complex;
      Right : in Right_Complex.Complex)
      return Result_Complex.Complex is
   begin
      return (
         Re => Left.Re * Right.Re - Left.Im * Right.Im,
         Im => Left.Re * Right.Im + Left.Im * Right.Re);
   end Multiply;

   function Norm_Squared (
      Item : in Input_Complex.Complex)
      return Result_Type is (
      Item.Re * Item.Re + Item.Im * Item.Im);

end Detector.Numerics.Complex_Types_Operations;
