package Detector.Lemmas.Uniformly_Complex with SPARK_Mode is

   -- This package's purpose is to provide a proof for certain properties
   -- around uniform complex numbers. When I refer to `uniform' I mean is a
   -- number in the closed set (-1, 1). We are going to use them for the Fast
   -- Fourier Transform Algorithm.

   Real_Whole_Bits    : constant := 4;
   Real_Fraction_Bits : constant := Bits - Real_Whole_Bits - 1;
   Real_Delta         : constant := 2.0 ** (-Real_Fraction_Bits);

   type Real is
      delta Real_Delta
      range -2.0 ** Real_Whole_Bits .. 2.0 ** Real_Whole_Bits - Real_Delta with
      Size => Bits;

   subtype Uniform_Real is Real range -1.0 .. 1.0;
-- subtype Uniform_Real is Real range -1.0 + Real'Delta .. 1.0 - Real'Delta;
   subtype Positive_Uniform_Real is Uniform_Real
      range 0.0 ..  Uniform_Real'Last;

   -- We declare a complex type.

   type Complex is record
      Re : Real;
      Im : Real;
   end record;

   -- The square root is not defined as such for rational numbers, because the
   -- result is irrational and we can't fit all the bits it has. Therefore when
   -- I talk about `Norm2' I mean the norm squared. And to simplify proofs I'm
   -- going to use the notation:
   --
   --    |C| = C.Re * C.Re + C.Im * C.Im
   --
   -- We only care when the `Norm' is Uniform. If the value is greater than 1
   -- then it is just garbage.

   subtype Complex_Result is Complex with
      Dynamic_Predicate => Complex_Result.Re in -2.0 .. 2.0
                  and then Complex_Result.Im in -2.0 .. 2.0;

   function Norm2 (Item : in Complex_Result) return Real is (
      Item.Re * Item.Re + Item.Im * Item.Im) with
      Global => null,
      Post   => Norm2'Result in 0.0 .. 8.0;

   subtype Uniform_Complex is Complex_Result with
      Dynamic_Predicate => Norm2 (Uniform_Complex) in 0.0 .. 1.0;

   -- We then need to prove a new property which is that if:
   --
   --    |C| <= 1 => C.Re and C.Im are Uniform
   --
   -- Which can easily be proved by contradiction. And the theorem prover is
   -- able to prove automagically with --level=2.

   procedure Lemma_If_Complex_Is_Uniform_Components_Are_Uniform (
      Item : in Uniform_Complex) with
      Ghost  => True,
      Global => null,
      Post   => Item.Re in Uniform_Real and then Item.Im in Uniform_Real;

   -- Let's define the product between two complex numbers as such:
   --
   --    A * B = (A.Re + A.Im * I) + (B.Re + B.Im) * I
   --          = A.Re * B.Re + A.Re * B.Im * I
   --          + A.Im * B.Re * I + A.Im * B.Im * I * I
   --          = (A.Re * B.Re - A.Im * B.Im) + (A.Re * B.Im + A.Im * B.Re) * I
   --    (A * B).Re = A.Re * B.Re - A.Im * B.Im
   --    (A * B).Im = A.Re * B.Im + A.Im * B.Re
   --
   -- The problem is that for the theorem prover A.Re * B.Re and A.Im * B.Im
   -- will be in range -1.0 .. 1.0.

   function "*" (Left, Right : in Uniform_Complex) return Complex_Result with
      Post     => "*"'Result.Re = Left.Re * Right.Re - Left.Im * Right.Im
         and then "*"'Result.Im = Left.Re * Right.Im + Left.Im * Right.Re,
      Global   => null;

   -- Then we want to prove that the Norm2 of the product equals the product
   -- of the norms.

   procedure Lemma_Distributive_Property (
      A    : in Uniform_Real;
      B, C : in Uniform_Real) with
      Ghost    => True,
      Global   => null,
      Pre      => B + C in -1.0 .. 1.0,
      Post     => Real (A * (B + C)) = Real (A * B) + Real (A * C)
         and then Real (A * (B + C)) in -1.0 .. 1.0
         and then Real (A * B) + Real (A * C) in -1.0 .. 1.0;

   procedure Lemma_Commutative_Property (
      Left  : in Uniform_Real;
      Right : in Uniform_Real) with
      Ghost  => True,
      Global => null,
      Post   => Real (Left * Right) = Real (Right * Left);

   procedure Lemma_Distributive_Property (
      A, B : in Uniform_Real;
      C, D : in Uniform_Real) with
      Ghost    => True,
      Global   => null,
      Pre      => A + B in -1.0 .. 1.0 and then C + D in -1.0 .. 1.0,
      Post     => Real (A + B) * Real (C + D)
                  = Real (A * C) + (A * D) + (B * C) + (B * D);

-- procedure Lemma_Norm2_Of_Product_Equals_Product_Of_Norm2 (
--    Left  : in Uniform_Complex;
--    Right : in Uniform_Complex) with
--    Ghost  => True,
--    Global => null,
--    Post   => Norm2 (Left) * Norm2 (Right) = Norm2 (Left * Right);

end Detector.Lemmas.Uniformly_Complex;
