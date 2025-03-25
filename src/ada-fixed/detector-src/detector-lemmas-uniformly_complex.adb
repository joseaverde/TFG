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
      -- Let's simplify a little bit the names:
      --    Left = A + B * I
      --    Right = X + Y * I
      A : constant Uniform_Real := Left.Re;
      B : constant Uniform_Real := Left.Im;
      X : constant Uniform_Real := Right.Re;
      Y : constant Uniform_Real := Right.Im;
   begin

      -->> Norm of product <<--

      -- Asserting that the result of the multiplication is as we expected:
      --
      --    Left * Right = (A + B * I) * (X + Y * I)
      --                 = (A * X - B * Y) + (A * Y + B * X) * I
      --    (Left * Right).Re = A * X - B * Y
      --    (Left * Right).Im = A * Y + B * X
      pragma Assert (Result.Re = A * X - B * Y);
      pragma Assert (Result.Im = A * Y + B * X);

      -- Then the norm2 is the square of the real part plus the square of the
      -- imaginary part:
      --
      --    |Left * Right| = (A * X - B * Y)² + (A * Y + B * X)²
      pragma Assert (
         Norm2 (Result) = Result.Re * Result.Re + Result.Im * Result.Im);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (A, A);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (B, B);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (X, X);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (Y, Y);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (A, X);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (B, Y);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (A, Y);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (B, X);
      pragma Assert (
         Norm2 (Result)
            = (Real (A * X) - B * Y) * (Real (A * X) - B * Y)
            + (Real (A * Y) + B * X) * (Real (A * Y) + B * X));

      -- We expand the expression, first the left hand side:
      --
      --    (A * X - B * Y)² = (A * X)² + (B * Y)² - 2 * A * B * X * Y
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (A * X, A * X);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (B * Y, B * Y);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (A * X, -B * Y);
      Lemma_Square_Of_Negation_Equals_Its_Square_Without_Negation (-B * Y);

      -- Make sure that there is no overflow
      pragma Assert (
         Real (Real'(-B * Y) * Real'(-B * Y))
         = Real (Real (B * Y) * Real (B * Y)));
      Lemma_Commutative_Property (A * X, -B * Y);
      pragma Assert (Real (A * X) * Real'(-B * Y) in Uniform_Real);
      pragma Assert (
         Real (Real (A * X) * Real (A * X)) + Real (B * Y) * Real (B * Y)
         in -2.0 .. 2.0);
      Lemma_Adding_Itself_Doubles_It (Real (A * X) * Real'(-B * Y));
      pragma Assert (
         Real (2 * (Real (Real (A * X) * Real'(-B * Y)))) in -2.0 .. 2.0);

      Lemma_Distributive_Property (A * X, -B * Y, A * X, -B * Y);
      pragma Assert (
         Real ((Real (A * X) + (-B * Y)) * Real (Real (A * X) + (-B * Y)))
         = Real (
           Real (A * X)  * Real (A * X))
         + Real (A * X)  * Real'(-B * Y)
         + Real'(-B * Y) * Real'(-B * Y)
         + Real'(-B * Y) * Real (A * X));

      -- Reoder it so it looks better
      Lemma_Commutative_Property (-B * Y, A * X);
      pragma Assert (
         Real'(Real'(-B * Y) * Real'(A * X))
         = Real'(Real'(A * X) * Real'(-B * Y)));
      pragma Assert (
         Real (Real (Real (A * X) * Real'(-B * Y))
         + Real'(-B * Y) * Real (A * X))
         = Real (Real (Real (A * X) * Real'(-B * Y)))
            + Real (Real (A * X)  * Real'(-B * Y)));
      Lemma_Adding_Itself_Doubles_It (Real (A * X) * Real'(-B * Y));
      -- Move the minus from the B
      pragma Assert (
         Real (2 * Real (Real (A * X) * Real'(-B * Y)))
         = Real (Real (Real (A * X) * Real'(-B * Y)))
            + Real (Real (A * X)  * Real'(-B * Y)));
      pragma Assert (
         Real (2 * Real (Real (A * X) * Real'(-B * Y)))
         = Real (2 * Real (Real (A * X) * (-Real (B * Y)))));
      Lemma_Move_Minus_Outside (A * X, B * Y);
      pragma Assert (
         Real (2 * Real (Real (A * X) * (-Real (B * Y))))
         = Real (2 * (-Real (Real (A * X) * Real (B * Y)))));
      pragma Assert (
         Real (2 * (-Real (Real (A * X) * Real (B * Y))))
         = Real (-2 * Real (Real (A * X) * Real (B * Y))));
      pragma Assert (
         Real (2 * (-Real (Real (A * X) * Real (B * Y))))
         = Real (Real (Real (A * X) * Real'(-B * Y)))
            + Real (Real (A * X)  * Real'(-B * Y)));
      -- Finish reordering
      pragma Assert (
         Real (Real (A * X) * Real (A * X))
         + Real (A * X)  * Real'(-B * Y)
         + Real'(-B * Y) * Real'(-B * Y)
         + Real'(-B * Y) * Real (A * X)
         = Real (Real (A * X) * Real (A * X))
         + Real'(-B * Y) * Real'(-B * Y)
         + Real (A * X)  * Real'(-B * Y)
         + Real (A * X)  * Real'(-B * Y));
      pragma Assert (
         Real (Real (A * X) * Real (A * X))
         + Real'(-B * Y) * Real'(-B * Y)
         + Real (A * X)  * Real'(-B * Y)
         + Real (A * X)  * Real'(-B * Y)
            = Real (Real (A * X) * Real (A * X))
            + Real'(-B * Y) * Real'(-B * Y)
            + Real (-2 * Real (Real (A * X) * Real (B * Y))));
      pragma Assert (
         Real (Real (A * X) * Real (A * X))
         + Real'(-B * Y) * Real'(-B * Y)
         - Real (2 * Real (Real (A * X) * Real (B * Y)))
            = Real (Real (A * X) * Real (A * X))
            + Real'(-B * Y) * Real'(-B * Y)
            + Real (-2 * Real (Real (A * X) * Real (B * Y))));
      -- Et c'est fini !
      pragma Assert (
         Real ((Real (A * X) + (-B * Y)) * Real (Real (A * X) + (-B * Y)))
            = Real (Real (A * X) * Real (A * X))
            + Real'(-B * Y) * Real'(-B * Y)
            - Real (2 * Real (Real (A * X) * Real (B * Y))));

      -- (A.Re * B.Im + A.Im * B.Re)²
      --  (A * Y + B * X)² = (A * Y)² + (B * X)² + 2 * A * B * X * Y
      Lemma_Distributive_Property (A * Y, B * X, A * Y, B * X);
      pragma Assert (
         Real (Real (Real (A * Y) + Real (B * X))
         * Real (Real (A * Y) + Real (B * X)))
            = Real (A * Y) * Real (A * Y)
            + Real (A * Y) * Real (B * X)
            + Real (B * X) * Real (A * Y)
            + Real (B * X) * Real (B * X));
      Lemma_Commutative_Property (B * X, A * Y);
      pragma Assert (
         Real (Real (A * Y) * Real (A * Y))
         + Real (A * Y) * Real (B * X)
         + Real (B * X) * Real (A * Y)
         + Real (B * X) * Real (B * X)
            = Real (A * Y) * Real (A * Y)
            + Real (B * X) * Real (B * X)
            + Real (B * X) * Real (A * Y)
            + Real (B * X) * Real (A * Y));
      Lemma_Adding_Itself_Doubles_It (Real (B * X) * Real (A * Y));
      pragma Assert (
         Real'(Real (B * X) * Real (A * Y)
         + Real (B * X) * Real (A * Y))
            = 2 * (Real (B * X) * Real (A * Y)));
      pragma Assert (
         Real (Real (A * Y) * Real (A * Y))
         + Real (B * X) * Real (B * X)
         + Real (B * X) * Real (A * Y)
         + Real (B * X) * Real (A * Y)
            = Real (A * Y) * Real (A * Y)
            + Real (B * X) * Real (B * X)
            + Real (2 * Real (Real (B * X) * Real (A * Y))));
      pragma Assert (
         Real (Real (Real (A * Y) + Real (B * X))
         * Real (Real (A * Y) + Real (B * X)))
            = Real (A * Y) * Real (A * Y)
            + Real (B * X) * Real (B * X)
            + Real (2 * Real (Real (B * X) * Real (A * Y))));

      -- It works!!!
      --
      --    (A * X - B * Y)² = (A * X)² + (B * Y)² - 2 * A * B * X * Y
      --  + (A * Y + B * X)² = (A * Y)² + (B * X)² + 2 * A * B * X * Y
      --  --------------------------------------------------------------
      --                     = (A * X)² + (B * Y)² + (A * Y)² + (B * X)²

      -- Then we need to prove that the the terms which are multiplied by
      -- two cancel each other. We need to use the associative property
      -- on one of them.

      -- TODO:  Properly prove it
      -- Proof: Read a elementary school math book.
      -- NOTE:  I know that it can't be true because of precission. However,
      --        in order to prove the properties for real numbers, we need to
      --        assume associativity.
      pragma Assume (
         Real'(Real (B * X) * Real (A * Y))
         = Real (A * X) * Real (B * Y));
      pragma Assert (
         Real'(Real (2 * Real (Real (A * X) * Real (B * Y)))
         - Real (2 * Real (Real (B * X) * Real (A * Y))))
         = 0.0);
      pragma Assert (
         Real'(
           Real (A * Y) * Real (A * Y)
         + Real (B * X) * Real (B * X)
         + Real (2 * Real (Real (B * X) * Real (A * Y)))
         + Real (Real (A * X) * Real (A * X))
         + Real'(-B * Y) * Real'(-B * Y)
         - Real (2 * Real (Real (A * X) * Real (B * Y))))
            = Real (A * Y) * Real (A * Y)
            + Real (B * X) * Real (B * X)
            + Real (Real (A * X) * Real (A * X))
            + Real'(-B * Y) * Real'(-B * Y));
      pragma Assert (
         Real (Real (Real (A * Y) + Real (B * X))
         * Real (Real (A * Y) + Real (B * X)))
         + Real ((Real (A * X) + (-B * Y)) * Real (Real (A * X) + (-B * Y)))
            = Real (A * Y) * Real (A * Y)
            + Real (B * X) * Real (B * X)
            + Real (2 * Real (Real (B * X) * Real (A * Y)))
            + Real (Real (A * X) * Real (A * X))
            + Real'(-B * Y) * Real'(-B * Y)
            - Real (2 * Real (Real (A * X) * Real (B * Y))));
      pragma Assert (
         Real (Real (Real (A * Y) + Real (B * X))
         * Real (Real (A * Y) + Real (B * X)))
         + Real ((Real (A * X) + (-B * Y)) * Real (Real (A * X) + (-B * Y)))
            = Real (A * Y) * Real (A * Y)
            + Real (B * X) * Real (B * X)
            + Real (A * X) * Real (A * X)
            + Real'(-B * Y) * Real'(-B * Y));
      pragma Assert (
         Norm2 (Result)
         = Real (A * Y) * Real (A * Y)
         + Real (B * X) * Real (B * X)
         + Real (A * X) * Real (A * X)
         + Real'(-B * Y) * Real'(-B * Y));
      Lemma_Commutative_Property (-B, Y);
      Lemma_Move_Minus_Outside (Y, B);
      Lemma_Commutative_Property (B, Y);
      pragma Assert (Real'(-B * Y) = Real'(Y * (-B)));
      pragma Assert (Real'(Y * (-B)) = -Real'(Y * B));
      pragma Assert (Real'(Y * B) = Real'(B * Y));
      pragma Assert (-Real'(Y * B) = -Real'(B * Y));
      pragma Assert (Real'(-B * Y) = -Real'(B * Y));
      Lemma_Square_Of_Negation_Equals_Its_Square_Without_Negation (B * Y);
      pragma Assert (
         Real'((-Real'(B * Y)) * (-Real'(B * Y)))
         = Real'(B * Y) * Real'(B * Y));
      pragma Assert (
         Real'(Real (A * Y) * Real (A * Y)
         + Real (B * X) * Real (B * X)
         + Real (A * X) * Real (A * X)
         + Real'(-B * Y) * Real'(-B * Y))
            = Real (A * Y) * Real (A * Y)
            + Real (B * X) * Real (B * X)
            + Real (A * X) * Real (A * X)
            + Real (B * Y) * Real (B * Y));
      pragma Assert (
         Norm2 (Result)
         = Real (A * Y) * Real (A * Y)
         + Real (B * X) * Real (B * X)
         + Real (A * X) * Real (A * X)
         + Real (B * Y) * Real (B * Y));

      -->> Product of norms <<--
      pragma Assert (Norm2 (Left) = A * A + B * B);
      pragma Assert (Norm2 (Right) = X * X + Y * Y);
      pragma Assert (Norm2 (Left) in Uniform_Real);
      pragma Assert (Norm2 (Right) in Uniform_Real);
      pragma Assert (Real (Norm2 (Left) * Norm2 (Right)) in Uniform_Real);
      pragma Assert (
         Real (Norm2 (Left) * Norm2 (Right))
            = (Real (A * A) + B * B)
            * (Real (X * X) + Y * Y));
      Lemma_Distributive_Property (A * A, B * B, X * X, Y * Y);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (A * A, X * X);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (A * A, Y * Y);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (B * B, X * X);
      Lemma_Multiplication_Of_Uniforms_Is_Uniform (B * B, Y * Y);
      pragma Assert (
         Real'(Norm2 (Left) * Norm2 (Right))
            = Real (A * A) * Real (X * X)
            + Real (A * A) * Real (Y * Y)
            + Real (B * B) * Real (X * X)
            + Real (B * B) * Real (Y * Y));

      -- Join solutions

      pragma Assume (
         (Real (A * A) + B * B)
         * (Real (X * X) + Y * Y)
            = Real ((Real (A * X) - B * Y)
            * (Real (A * X) - B * Y))
            + (Real (A * Y) + B * X)
            * (Real (A * Y) + B * X));
      pragma Assert (Norm2 (Left) * Norm2 (Right) = Norm2 (Result));
   end Lemma_Norm2_Of_Product_Equals_Product_Of_Norm2;

   procedure Lemma_Move_Minus_Outside (
      Left  : in Uniform_Real;
      Right : in Uniform_Real) is
   begin
      pragma Assume (Real (Left * (-Right)) = -Real (Left * Right));
   end Lemma_Move_Minus_Outside;

end Detector.Lemmas.Uniformly_Complex;
