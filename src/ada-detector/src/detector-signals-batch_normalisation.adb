with Detector.Signals.Lemmas;
with Detector.Signals.Mean;
with Detector.Signals.Quarter_Variance;
with Detector.Numerics.Elementary_Functions;
with Ada.Numerics.Elementary_Functions;

package body Detector.Signals.Batch_Normalisation with SPARK_Mode is

   Even_Bits          : constant := Big_Sample_Bits;
   Even_Whole_Bits    : constant := 1;
   Even_Fraction_Bits : constant := Even_Bits - Even_Whole_Bits - 1;
   Even_Delta         : constant := 2.0 ** (-Even_Fraction_Bits);
   type Even_Fraction_Sample is
      delta Even_Delta
      range -2.0 ** Even_Whole_Bits .. 2.0 ** Even_Whole_Bits - Even_Delta with
   Size => Even_Bits;
   -- We need to create a type with an even number of bits for the fractional
   -- part. So that the square root algorithm doesn't have precission problems
   -- and the square root of the denominator is trivial:
   --
   --    √(2^(2x)) = 2^x
   --
   -- We are going to use this type for the denominator of the batch
   -- normalisation.

   function Sqrt is
      new Detector.Numerics.Elementary_Functions.Fixed_Sqrt (
      Fixed_Type => Even_Fraction_Sample);
   -- function Sqrt (Item : in Even_Fraction_Sample) return Even_Fraction_Sample
   --    is (Even_Fraction_Sample (
   --          Ada.Numerics.Elementary_Functions."**" (Float (Item), 0.5)));

   Bound : constant Normalised_Sample := Normalised_Sample'Last / 2;

   procedure Lemma_Negative_Safe_Division (
      Left  : in Sample_Type;
      Right : in Even_Fraction_Sample) with
      Ghost    => True,
      Global   => null,
      Pre      => Left < 0.0 and then Right > 0.0
         and then Right >= Left / (-Bound),
      Post     => Normalised_Sample'(Left / Right) in -Bound .. 0.0;

   procedure Lemma_Negative_Safe_Division (
      Left  : in Sample_Type;
      Right : in Even_Fraction_Sample) is
      -- Thanks to Álvaro and Lucía for helping me debug this Lemma :)
      L : constant Sample_Type := -Left;
   begin
      -- Knowledge
      pragma Assert (Left < 0.0);               -- As per precondition
      pragma Assert (Right > 0.0);              -- As per precondition
      pragma Assert ((-Bound) < 0.0);           -- As per precondition
      -- Thougths
      pragma Assert (Right >= L / Bound);       -- By negatig the variable
      pragma Assert (Bound >= L / Right);       -- we can assist the prover!
      -- Postcondition
      pragma Assert (Left / Right >= -Bound);   -- As per postcondition
   end Lemma_Negative_Safe_Division;

   function Scale (
      X : in Sample_Type;
      σ : in Even_Fraction_Sample)
      return Normalised_Sample with
      Global => null,
      Inline => True,
      Pre    => σ > 0.0,
      Post   => Scale'Result in -Bound .. Bound;
   -- This function computes: X / σ. If it overflows it returns Bound. If it
   -- underflows it returns -Bound. According to Wikipedia and to my artificial
   -- vision teacher: the Batch Normalisation puts the more common numbers
   -- around 0.0. If there are any weird numbers their are fram from 0.

   function Scale (
      X : in Sample_Type;
      σ : in Even_Fraction_Sample)
      return Normalised_Sample is
      Result : Normalised_Sample;
   begin

      -- Next we have to divide by `σ'. `σ' is known to be a value lower than
      -- 1.0. Which implies the result is going to be greater than the
      -- numerator.
      --
      --    σ ∈ (0, 1] -> x / σ >= x, ∀x
      --
      -- We know the following things:
      --
      --    σ ∈ (0, 1]
      --    x ∈ (-1, 1)
      --    Normalised_Sample = [First, Last], First < Last
      --
      -- As we need to multiply then by 2. We will work with the following
      -- assumptions (there is an assert on the spec that proves it):
      --
      --    First = -Last
      --    Last = 2^k, k ∈ |N
      --    B = Last / 2
      --
      -- We have to make sure:
      --
      --    x / σ ∈ [-B, B]
      --
      -- That way the quotient can be represented in the target type.

      if X = 0.0 then

         -- If x = 0.0 then
         --    x / σ = 0 ∈ [-B, B]  [Assert -B < 0.0 < B]
         --                         [Conditions are met :)]
         Result := X / σ;
         pragma Assert (Result = 0.0);
         pragma Assert (Result in -Bound .. Bound);

      elsif X > 0.0 and then σ >= X / Bound then

         -- If x > 0.0 then
         --    x / σ > 0.0          [σ > 0.0 /\ X > 0.0]
         --    x / σ <= B           [As per postcondition]
         -- -> x <= B σ             [Multiplying by σ both sides, σ > 0.0]
         -- -> x / B <= σ           [Dividing by B both sides, B > 0.0]
         --
         -- Then if:
         --    x > 0.0 /\ x / B <= σ -> x / σ <= B

         -- NOTE: We don't need the lemma with --level=2, the prover is able to
         --       prove it without help.
         -- Lemma_Positive_Safe_Division (X, σ);
         Result := X / σ;
         pragma Assert (Result in 0.0 .. Bound);

      elsif X < 0.0 and then σ >= X / (-Bound) then

         -- If x < 0.0 then
         --    x / σ < 0.0          [σ > 0.0 /\ x < 0.0]
         --    x / σ >= -B          [As per postcondition]
         -- -> x >= -B * σ          [Multiplying by σ both sides, σ > 0.0]
         -- -> x / -B <= σ          [Divide by -B both sides, -B < 0.0]
         --
         -- Then if:
         --    x < 0.0 /\ x / -B <= σ -> x / σ >= -B

         Lemma_Negative_Safe_Division (X, σ);
         Result := X / σ;
         pragma Assert (Result in -Bound .. 0.0);

      else

         Result := (if X < 0.0 then -Bound else Bound);
         pragma Assert (Result in -Bound .. Bound);

      end if;

      pragma Assert (Result in -Bound .. Bound);
      return Result;

   end Scale;

   procedure Normalise (
      Input  : in     Signal_Type;
      Output :    out Normalised_Signal) is

      μ : constant Sample_Type := Mean (Input);

      -- The variable `σ2' contains a quarter of the variance. And it is a
      -- positive number between 0.0 and 1.0. We want to compute its square
      -- root for the denominator.
      --
      --    √(σ²/4) = σ/2
      --
      -- According to the algorithm, we need to add a small `ε' value so that
      -- we don't divide by zero. It is as simple as check whether the value
      -- is zero. And then assigning its δ.
      --
      -- The value of σ²/4 is in range [0, 1). Therefore the value of its
      -- square root: σ/2 is in range [0, 1) too. If we multiply by 2. We are
      -- in range [0, 2), which is the same as the `Even_Fraction_Sample' type.

      σ2 : constant Even_Fraction_Sample :=
         Even_Fraction_Sample (Quarter_Variance (Input));
      σ  : constant Even_Fraction_Sample := (
         declare
            σ : constant Even_Fraction_Sample := Sqrt (σ2);
         begin
            (if    σ = 0.0  then Even_Fraction_Sample'Delta
             elsif σ >= 1.0 then Even_Fraction_Sample'Last
             else                2 * σ));

   begin
      Output := [others => 0.0];
      pragma Assert (σ > 0.0);
      for I in Count_Type range 0 .. Output'Length - 1 loop
         pragma Loop_Invariant (σ > 0.0);
         -- Input (I) ∈ (-1, 1)
         -- μ        ∈ (-1, 1)
         -- Dev      ∈ (0, 2)
         -- ---
         -- Input (I) - μ ∈ (-2, 2)
         -- Input (I) / 2 - μ / 2 ∈ (-1, 1)
         Lemmas.Lemma_Half_Halves (Input (Input'First + I));
         Lemmas.Lemma_Half_Halves (μ);
         Output (Output'First + I) :=
            2 * Scale (Input (Input'First + I) / 2 - μ / 2, σ);
      end loop;
   end Normalise;

end Detector.Signals.Batch_Normalisation;
