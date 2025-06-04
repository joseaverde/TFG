--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-elementary_functions.ads                     |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package Detector.Numerics.Elementary_Functions with Pure, SPARK_Mode is

   -- This package is similar to `Ada.Numerics.Elementary_Functions'. It
   -- differs in that it isn't generic, it isn't complete (it only contains the
   -- functions that are needed for this project), each function has its own
   -- input and output types and there may be generic functions.
   --
   -- This functions are only approximations to the real functions. And they
   -- may have an error. So certain properties may not hold such as:
   --
   --    sin² x + cos² x = 1
   --
   -- would not be true, as there are fluctuations. All this functions are
   -- SPARK-proven and can be used as they are.

   -->> Trigonometric Functions <<--

   -- Trigonometric functions use a turn based implementation:
   -- https://en.wikipedia.org/wiki/Sine_and_cosine#Turns_based_implementations
   -- The idea is instead of multiplying by π, which may incur into accuracy
   -- problemas. We compute the sine or the cosine of a fraction of the circle.
   -- Which means:
   --
   --    Sinpi (X) = Sin (X * π)
   --    Cospi (X) = Cos (X * π) = Sinpi (0.5 - X)
   --
   -- That way we can be sure that:
   --
   --    Sin (0) = 0
   --    Cos (0) = 1
   --    Sin (π) = 1
   --    Cos (π) = 0

   Trigonometric_Input_Bits          : constant := Bits;
   Trigonometric_Input_Whole_Bits    : constant := 2;
   Trigonometric_Input_Fraction_Bits : constant :=
      Bits - Trigonometric_Input_Whole_Bits - 1;
   Trigonometric_Input_Delta         : constant :=
      2.0 ** (-Trigonometric_Input_Fraction_Bits);
   type Trigonometric_Input_Type is
      delta Trigonometric_Input_Delta
      range 0.0 .. 2.0 - Trigonometric_Input_Delta with
   Size => Trigonometric_Input_Bits;
   -- The input for the trigonometric functions `Sinpi' and `Cospi' is a number
   -- in the set [0, 2). As both functions implictly multiply the given value
   -- by π. If the user wants to compute the sine or cosine of a bigger value.
   -- They must first module the input value.

   Trigonometric_Output_Bits          : constant := Bits;
   Trigonometric_Output_Whole_Bits    : constant := 2;
   Trigonometric_Output_Fraction_Bits : constant :=
      Bits - Trigonometric_Output_Whole_Bits - 1;
   Trigonometric_Output_Delta         : constant :=
      2.0 ** (-Trigonometric_Output_Fraction_Bits);
   type Trigonometric_Output_Type is
      delta Trigonometric_Output_Delta
      range -1.0 .. 1.0 with
      Size => Trigonometric_Output_Bits;
   -- The output of the sine and cosine functions are always in range [-1, 1].
   -- This type allows to return that ratio with as much precision as possible.

   function Sinpi (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type with
      Inline => True,
      Global => null;
   -- This function computes the sine of the value multiplied by pi (π):
   --
   --    Sinpi (Item) ~= Sin (Item * π)
   --
   -- This allows an easier implementation and ensures that:
   --
   --    Sin (0) = 0 = Sin (π)
   --    Sin (π/2) = 1
   --    Sin (3π/2) = -1

   function Cospi (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type with
      Inline => True,
      Global => null;
   -- This function computes the cosine of the value multiplied by pi (π):
   --
   --    Cospi (Item) ~= Cos (Item * π)

   -->> Square Root <<--

   Max_Bits : constant := 64;
   generic
      type Fixed_Type is delta <>;
   function Fixed_Sqrt (Item : in Fixed_Type) return Fixed_Type'Base with
      Global   => null,
      Pre      => Fixed_Type'Size <= Max_Bits and then Item >= 0.0,
      Post     => Fixed_Sqrt'Result >= 0.0
         and then (if Item <= 1.0 then Fixed_Sqrt'Result <= 1.0
                     else Fixed_Sqrt'Result >= 1.0);

end Detector.Numerics.Elementary_Functions;
