--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-generic_complex_types.ads                    |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

generic
   type Fixed_Type is delta <>;
package Detector.Numerics.Generic_Complex_Types with Pure, SPARK_Mode is

   type Complex is record
      Re : Fixed_Type;
      Im : Fixed_Type;
   end record;

   function "*" (Left : in Complex; Right : in Integer) return Complex with
      Global   => null,
      Inline   => True,
      Post     => "*"'Result.Re = Left.Re * Right
         and then "*"'Result.Im = Left.Im * Right;

   function "/" (Left : in Complex; Right : in Integer) return Complex with
      Global   => null,
      Inline   => True,
      Pre      => Right /= 0,
      Post     => "/"'Result.Re = Left.Re / Right
         and then "/"'Result.Im = Left.Im / Right;

   function "*" (Left, Right : in Complex) return Complex with
      Global   => null,
      Inline   => True,
      Post     => "*"'Result.Re = Left.Re * Right.Re - Left.Im * Right.Im
         and then "*"'Result.Im = Left.Re * Right.Im + Left.Im * Right.Re;

   function "+" (Left, Right : in Complex) return Complex with
      Global   => null,
      Inline   => True,
      Post     => "+"'Result.Re = Left.Re + Right.Re
         and then "+"'Result.Im = Left.Im + Right.Im;

   function "-" (Left, Right : in Complex) return Complex with
      Global   => null,
      Inline   => True,
      Post     => "-"'Result.Re = Left.Re - Right.Re
         and then "-"'Result.Im = Left.Im - Right.Im;

   function Norm_Squared (Item : in Complex) return Fixed_Type with
      Global => null,
      Inline => True,
      Post   => Norm_Squared'Result = Item.Re * Item.Re + Item.Im * Item.Im;

end Detector.Numerics.Generic_Complex_Types;
