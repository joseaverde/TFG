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
      pragma Assert (σ > 0.0);
      pragma Assert (σ < 0.0);
      for I in Count_Type range 0 .. Output'Length - 1 loop
         pragma Loop_Invariant (σ > 0.0);
         -- Input (I) ∈ (-1, 1)
         -- μ        ∈ (-1, 1)
         -- Dev      ∈ (0, 2)
         -- ---
         -- Input (I) - μ ∈ (-2, 2)
         Output (Output'First + I) :=
            (Input (Input'First + I) / 2 - μ / 2) / σ;
         Output (Output'First + I) := @ * 2;
      end loop;
   end Normalise;

end Detector.Signals.Batch_Normalisation;
