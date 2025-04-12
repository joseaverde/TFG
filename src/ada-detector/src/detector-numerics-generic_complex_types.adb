--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-generic_complex_types.adb                    |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Numerics.Generic_Complex_Types with SPARK_Mode is

   function "*" (Left : in Complex; Right : in Integer) return Complex is (
      Re => Left.Re * Right,
      Im => Left.Im * Right);

   function "/" (Left : in Complex; Right : in Integer) return Complex is (
      Re => Left.Re / Right,
      Im => Left.Im / Right);

   function "*" (Left, Right : in Complex) return Complex is (
      Re => Left.Re * Right.Re - Left.Im * Right.Im,
      Im => Left.Re * Right.Im + Left.Im * Right.Re);

   function "+" (Left, Right : in Complex) return Complex is (
      Re => Left.Re + Right.Re,
      Im => Left.Im + Right.Im);

   function "-" (Left, Right : in Complex) return Complex is (
      Re => Left.Re - Right.Re,
      Im => Left.Im - Right.Im);

   function Norm_Squared (Item : in Complex) return Fixed_Type is (
      Item.Re * Item.Re + Item.Im * Item.Im);

end Detector.Numerics.Generic_Complex_Types;
