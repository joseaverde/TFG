package body Detector.Lemmas.Uniformly_Complex with SPARK_Mode is

   procedure Lemma_If_Complex_Is_Uniform_Components_Are_Uniform (
      Item : in Uniform_Complex) is
   begin
      null;
   end Lemma_If_Complex_Is_Uniform_Components_Are_Uniform;

   function "*" (Left, Right : in Uniform_Complex) return Complex_Result is
      Result : Complex;
   begin
      Lemma_If_Complex_Is_Uniform_Components_Are_Uniform (Left);
      Lemma_If_Complex_Is_Uniform_Components_Are_Uniform (Right);
      Result.Re := Left.Re * Right.Re - Left.Im * Right.Im;
      Result.Im := Left.Re * Right.Im + Left.Im * Right.Re;
      return Result;
   end "*";

   procedure Lemma_Commutative_Property (
      Left  : in Uniform_Real;
      Right : in Uniform_Real) is
   begin
      pragma Assume (Real (Left * Right) = Real (Right * Left));
   end Lemma_Commutative_Property;

   procedure Lemma_Distributive_Property (
      A    : in Uniform_Real;
      B, C : in Uniform_Real) is
   begin
      pragma Assume (Real (A * (B + C)) = Real (A * B) + Real (A * C));
   end Lemma_Distributive_Property;

   procedure Lemma_Distributive_Property (
      A, B : in Uniform_Real;
      C, D : in Uniform_Real) is
   begin
      Lemma_Distributive_Property (A + B, C, D);
      pragma Assert (
         Real (A + B) * Real (C + D)
         = Real (Real (A + B) * C) + (Real (A + B) * D));
      Lemma_Commutative_Property (C, A + B);
      Lemma_Commutative_Property (D, A + B);
      pragma Assert (Real (C * Real (A + B)) = Real (A + B) * C);
      pragma Assert (Real (D * Real (A + B)) = Real (A + B) * D);
      Lemma_Distributive_Property (C, A, B);
      Lemma_Distributive_Property (D, A, B);
      pragma Assert (C * Real (A + B) = Real (C * A) + Real (C * B));
      pragma Assert (D * Real (A + B) = Real (D * A) + Real (D * B));
      pragma Assert (
         Real (Real (A + B) * C) + (Real (A + B) * D)
         = Real (C * Real (A + B)) + Real (D * Real (A + B)));
      pragma Assert (
         Real (C * Real (A + B)) + Real (D * Real (A + B))
         = Real (C * A) + Real (C * B) + Real (D * A) + Real (D * B));
      pragma Assert (
         Real (Real (A + B) * C) + (Real (A + B) * D)
         = Real (C * A) + Real (C * B) + Real (D * A) + Real (D * B));
      pragma Assert (
         Real (A + B) * Real (C + D)
         = Real (C * A) + Real (C * B) + Real (D * A) + Real (D * B));
      Lemma_Commutative_Property (C, A);
      Lemma_Commutative_Property (C, B);
      Lemma_Commutative_Property (D, A);
      Lemma_Commutative_Property (D, B);
      pragma Assert (
         Real (C * A) + Real (C * B) + Real (D * A) + Real (D * B)
         = Real (A * C) + Real (B * C) + Real (A * D) + Real (B * D));
   end Lemma_Distributive_Property;

-- procedure Lemma_Norm2_Of_Product_Equals_Product_Of_Norm2 (
--    Left  : in Uniform_Complex;
--    Right : in Uniform_Complex) is
--    Result : constant Complex_Result := Left * Right;
-- begin
--    -- Norm of product
--    pragma Assert (Result.Re = Left.Re * Right.Re - Left.Im * Right.Im);
--    pragma Assert (Result.Im = Left.Re * Right.Im + Left.Im * Right.Re);
--    pragma Assert (
--       Norm2 (Result) = Result.Re * Result.Re + Result.Im * Result.Im);
--    pragma Assert (
--       Norm2 (Result)
--          = (Real (Left.Re * Right.Re) - Left.Im * Right.Im)
--          * (Real (Left.Re * Right.Re) - Left.Im * Right.Im)
--          + (Real (Left.Re * Right.Im) + Left.Im * Right.Re)
--          * (Real (Left.Re * Right.Im) + Left.Im * Right.Re));
--    pragma Assert (Real (Left.Re * Right.Re) in 0.0 .. 1.0);
--    pragma Assert (Real (Left.Im * Right.Im) in 0.0 .. 1.0);
--    Lemma_Distributive_Property (
--       Left.Re * Right.Re, -Left.Im * Right.Im,
--       Left.Re * Right.Im, Left.Im * Right.Re);
--    -- TODO: Prove that (A + B) * (A - B) = A * A - B * B
--    -- NOTE: Use Lemma_Distributive_Property (4) and make B negative

--    -- Product of norms
--    pragma Assert (Norm2 (Left) = Left.Re * Left.Re + Left.Im * Left.Im);
--    pragma Assert (
--       Norm2 (Right) = Right.Re * Right.Re + Right.Im * Right.Im);
--    pragma Assert (Norm2 (Left) in 0.0 .. 1.0);
--    pragma Assert (Norm2 (Right) in 0.0 .. 1.0);
--    pragma Assert (Real (Norm2 (Left) * Norm2 (Right)) in 0.0 .. 1.0);
--    pragma Assert (
--       Real (Norm2 (Left) * Norm2 (Right))
--          = (Real (Left.Re * Left.Re) + Left.Im * Left.Im)
--          * (Real (Right.Re * Right.Re) + Right.Im * Right.Im));
--    Lemma_Distributive_Property (
--       Left.Re * Left.Re, Left.Im * Left.Im,
--       Right.Re * Right.Re, Right.Im * Right.Im);
--    pragma Assert (
--       Real (Norm2 (Left) * Norm2 (Right))
--          = Real (Left.Re * Left.Re) * Real (Right.Re * Right.Re)
--          + Real (Left.Re * Left.Re) * Real (Right.Im * Right.Im)
--          + Real (Left.Im * Left.Im) * Real (Right.Re * Right.Re)
--          + Real (Left.Im * Left.Im) * Real (Right.Im * Right.Im));

--    -- Join solutions

--    pragma Assume (
--       (Real (Left.Re * Left.Re) + Left.Im * Left.Im)
--       * (Real (Right.Re * Right.Re) + Right.Im * Right.Im)
--          = Real ((Real (Left.Re * Right.Re) - Left.Im * Right.Im)
--          * (Real (Left.Re * Right.Re) - Left.Im * Right.Im))
--          + (Real (Left.Re * Right.Im) + Left.Im * Right.Re)
--          * (Real (Left.Re * Right.Im) + Left.Im * Right.Re));
--    pragma Assert (Norm2 (Left) * Norm2 (Right) = Norm2 (Result));
-- end Lemma_Norm2_Of_Product_Equals_Product_Of_Norm2;

end Detector.Lemmas.Uniformly_Complex;
