package Detector.Numerics.Elementary_Functions with Pure, SPARK_Mode is

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

   function Sinpi (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type with
      Inline => True,
      Global => null;

   function Cospi (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type with
      Inline => True,
      Global => null;

   -->> Square Root <<--

   generic
      type Fixed_Type is delta <>;
   function Sqrt (Item : in Fixed_Type) return Fixed_Type'Base with
      Global => null,
      Pre    => Item >= 0.0,
      Post   => Sqrt'Result >= 0.0 and then Sqrt'Result <= Item;

end Detector.Numerics.Elementary_Functions;
