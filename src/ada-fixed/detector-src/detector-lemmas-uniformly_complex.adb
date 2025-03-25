package body Detector.Lemmas.Uniformly_Complex with SPARK_Mode is

   procedure Lemma_If_Complex_Is_Uniform_Components_Are_Uniform (
      Item : in Uniform_Complex) is
   begin
      null;
   end Lemma_If_Complex_Is_Uniform_Components_Are_Uniform;

   procedure Lemma_Multiplication_Of_Uniforms_Is_Uniform (
      Left  : in Uniform_Real;
      Right : in Uniform_Real) is
   begin
      null;
   end Lemma_Multiplication_Of_Uniforms_Is_Uniform;

   function "*" (Left, Right : in Uniform_Complex) return Complex_Result is
      Result : Complex;
   begin
      Lemma_If_Complex_Is_Uniform_Components_Are_Uniform (Left);
      Lemma_If_Complex_Is_Uniform_Components_Are_Uniform (Right);
      Result.Re := Left.Re * Right.Re - Left.Im * Right.Im;
      Result.Im := Left.Re * Right.Im + Left.Im * Right.Re;
      return Result;
   end "*";

   procedure Lemma_Distributive_Property (
      A    : in Double_Uniform_Real;
      B, C : in Uniform_Real) is
   begin
      -- FIXME: Find out why
      -- Proof: It appeared to me in a dream
      pragma Assume (Real (A * (B + C)) = Real (A * B) + Real (A * C));
   end Lemma_Distributive_Property;

   procedure Lemma_Commutative_Property (
      Left  : in Double_Uniform_Real;
      Right : in Double_Uniform_Real) is
   begin
      -- FIXME: Find out why => because yes
      pragma Assume (Real (Left * Right) = Real (Right * Left));
   end Lemma_Commutative_Property;

   procedure Lemma_Adding_Itself_Doubles_It (
      Item : in Uniform_Real) is
   begin
      null;
   end Lemma_Adding_Itself_Doubles_It;

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

   procedure Lemma_Square_Of_Negation_Equals_Its_Square_Without_Negation (
      Item : in Uniform_Real) is
   begin
      pragma Assume (Uniform_Real ((-Item) * (-Item))
                     = Uniform_Real (Item * Item));
   end Lemma_Square_Of_Negation_Equals_Its_Square_Without_Negation;

   procedure Lemma_Norm2_Of_Product_Equals_Product_Of_Norm2 (
      Left  : in Uniform_Complex;
      Right : in Uniform_Complex) is
      Result : constant Complex_Result := Left * Right;
   begin
      -->> Norm of product <<--

      -- A * B = (A.Re + A.Im * I) + (B.Re + B.Im) * I
      --       = A.Re * B.Re + A.Re * B.Im * I
      --       + A.Im * B.Re * I + A.Im * B.Im * I * I
      --       = (A.Re * B.Re - A.Im * B.Im) + (A.Re * B.Im + A.Im * B.Re) * I
      -- (A * B).Re = A.Re * B.Re - A.Im * B.Im
      -- (A * B).Im = A.Re * B.Im + A.Im * B.Re
      pragma Assert (Result.Re = Left.Re * Right.Re - Left.Im * Right.Im);
      pragma Assert (Result.Im = Left.Re * Right.Im + Left.Im * Right.Re);
      pragma Assert (
         Norm2 (Result) = Result.Re * Result.Re + Result.Im * Result.Im);
      pragma Assert (
         Norm2 (Result)
            = (Real (Left.Re * Right.Re) - Left.Im * Right.Im)
            * (Real (Left.Re * Right.Re) - Left.Im * Right.Im)
            + (Real (Left.Re * Right.Im) + Left.Im * Right.Re)
            * (Real (Left.Re * Right.Im) + Left.Im * Right.Re));
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (Left.Re, Right.Re);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (Left.Im, Right.Im);
      pragma Assert (Real (Left.Re * Right.Re) in Uniform_Real);
      pragma Assert (Real (Left.Im * Right.Im) in Uniform_Real);

      -- (A.Re * B.Re - A.Im * B.Im)²
      --    = (A.Re * B.Re)² + (A.Im * B.Im)²
      --    - 2 * (A.Re * B.Re * A.Im * B.Im)
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (Left.Re, Right.Im);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (Left.Im, Right.Re);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (
         Left.Re * Right.Re, Left.Re * Right.Re);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (
         Left.Im * Right.Im, Left.Im * Right.Im);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (
         Left.Re * Right.Re, -Left.Im * Right.Im);
      pragma Assert (
         Real (Left.Re * Right.Re)
         * Real (Left.Re * Right.Re) in Uniform_Real);
      Lemma_Square_Of_Negation_Equals_Its_Square_Without_Negation (
         -Left.Im * Right.Im);
      pragma Assert (
         Real (Real'(-Left.Im * Right.Im)
         * Real'(-Left.Im * Right.Im))
         = Real (Real (Left.Im * Right.Im) * Real (Left.Im * Right.Im)));
      pragma Assert (
         Real (Left.Im * Right.Im)
         * Real (Left.Im * Right.Im) in Uniform_Real);
      Lemma_Commutative_Property (Left.Re * Right.Re, -Left.Im * Right.Im);
      pragma Assert (
         Real (Left.Re * Right.Re)
         * Real'(-Left.Im * Right.Im) in Uniform_Real);
      Lemma_Distributive_Property (
         Left.Re * Right.Re, -Left.Im * Right.Im,
         Left.Re * Right.Re, -Left.Im * Right.Im);
      pragma Assert (
         Real (Real (Left.Re * Right.Re) * Real (Left.Re * Right.Re))
         + Real (Left.Im * Right.Im) * Real (Left.Im * Right.Im)
         in -2.0 .. 2.0);
      Lemma_Adding_Itself_Doubles_It (
         Real (Left.Re * Right.Re) * Real'(-Left.Im * Right.Im));
      pragma Assert (
         Real (2 * (Real (
            Real (Left.Re * Right.Re) * Real'(-Left.Im * Right.Im))))
         in -2.0 .. 2.0);
   -- pragma Assert (
   --    Real ((Real (Left.Re * Right.Re) - (Left.Im * Right.Im))
   --    * Real (Real (Left.Re * Right.Re) - Left.Im * Right.Im))
   --       = Real (Real (Left.Re * Right.Re) * Real (Left.Re * Right.Re))
   --       + Real (Real (-Left.Im) * Right.Im) * Real (Real (-Left.Im) * Right.Im)
   --       + Real (Left.Re * Right.Re) * Real (Real (-Left.Im) * Right.Im)
   --       + Real (Left.Re * Right.Re) * Real (Real (-Left.Im) * Right.Im));

      -- (A.Re * B.Im + A.Im * B.Re)²
      Lemma_Distributive_Property (
         Left.Re * Right.Im, Left.Im * Right.Re,
         Left.Re * Right.Im, Left.Im * Right.Re);
      -- TODO: Prove that (A + B) * (A - B) = A * A - B * B
      -- NOTE: Use Lemma_Distributive_Property (4) and make B negative

      -->> Product of norms <<--
      pragma Assert (Norm2 (Left) = Left.Re * Left.Re + Left.Im * Left.Im);
      pragma Assert (
         Norm2 (Right) = Right.Re * Right.Re + Right.Im * Right.Im);
      pragma Assert (Norm2 (Left) in Uniform_Real);
      pragma Assert (Norm2 (Right) in Uniform_Real);
      pragma Assert (Real (Norm2 (Left) * Norm2 (Right)) in Uniform_Real);
      pragma Assert (
         Real (Norm2 (Left) * Norm2 (Right))
            = (Real (Left.Re * Left.Re) + Left.Im * Left.Im)
            * (Real (Right.Re * Right.Re) + Right.Im * Right.Im));
      Lemma_Distributive_Property (
         Left.Re * Left.Re, Left.Im * Left.Im,
         Right.Re * Right.Re, Right.Im * Right.Im);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (
            Left.Re * Left.Re, Right.Re * Right.Re);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (
            Left.Re * Left.Re, Right.Im * Right.Im);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (
            Left.Im * Left.Im, Right.Re * Right.Re);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (
            Left.Im * Left.Im, Right.Im * Right.Im);
      pragma Assert (
         Real (Norm2 (Left) * Norm2 (Right))
            = Real (Left.Re * Left.Re) * Real (Right.Re * Right.Re)
            + Real (Left.Re * Left.Re) * Real (Right.Im * Right.Im)
            + Real (Left.Im * Left.Im) * Real (Right.Re * Right.Re)
            + Real (Left.Im * Left.Im) * Real (Right.Im * Right.Im));

      -- Join solutions

      pragma Assume (
         (Real (Left.Re * Left.Re) + Left.Im * Left.Im)
         * (Real (Right.Re * Right.Re) + Right.Im * Right.Im)
            = Real ((Real (Left.Re * Right.Re) - Left.Im * Right.Im)
            * (Real (Left.Re * Right.Re) - Left.Im * Right.Im))
            + (Real (Left.Re * Right.Im) + Left.Im * Right.Re)
            * (Real (Left.Re * Right.Im) + Left.Im * Right.Re));
      pragma Assert (Norm2 (Left) * Norm2 (Right) = Norm2 (Result));
   end Lemma_Norm2_Of_Product_Equals_Product_Of_Norm2;

end Detector.Lemmas.Uniformly_Complex;
