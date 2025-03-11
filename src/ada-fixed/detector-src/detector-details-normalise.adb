with Detector.Details.Mean;
package body Detector.Details.Normalise with SPARK_Mode => On is

   -- Dividing by a number lower than 1.0 implies multiplication.
   --  * The exponent for Dev is the square of the exponent for Real.
   --  * The exponent for Real is the square of the exponent for Sample.
   --  * And that must fit in Normalised_Sample with certain exponent.
   -- If we divide by Dev a number that is between [First - Last, Last - First]
   -- we find three different cases (assuming Normalised_Sample range is a
   -- subset of [First - Last, Last - First] range):
   --
   --  * If Dev < Min_Dev where Min_Dev < 1.0 then the division increases the
   --    value. As it is less than one, so therefore we must truncate it.
   --  * If Dev < Max_Dev where Max_Dev > Min_Dev then the division doesn't
   --    fall in the output range and we must truncate it.
   --  * Otherwise the division yields a correct value.
   --
   -- In order to determine wether the division falls into range we know that:
   --
   --  * Dev > 0.0 (as per assertion)
   --  * Item (I) - μ ∈ [First - Last, Last - First]
   --  * Normalised_Sample = [NF, NL]
   --  * (Item (I) - μ) / Dev shall ∈ [NF, NL]
   --
   -- Let's call:
   --
   --    Value := Item (I) - μ
   --    Value / Dev shall ∈ [NF, NL]
   --
   -- Assume:
   --
   --    NF < 0.0
   --    NL > 0.0
   --
   -- Therefore:
   --
   -- If Value = 0.0 then
   --    Value = 0 ∈ [NF, NL] [Assert NF < 0.0 < NL]
   --    Conditions are met! :)
   --
   -- If Value > 0.0 then
   --    Value / Dev > 0.0    [Dev > 0.0 and Value > 0.0]
   --    Value / Dev <= NL    [As per postcondition]
   --    Dev >= Value / NL    [Multiply by Dev, divide by NL, NL, Dev > 0.0]
   --
   -- If Value < 0.0 then
   --    Value / Dev < 0.0    [Dev > 0.0 and Value < 0.0]
   --    Value / Dev >= NF    [As per postcondition]
   --    Value >= NF * Dev    [Multiply by Dev both sides]
   --    Dev >= Value / NF    [Divide by NF < 0.0]
   --
   -- Now we know the values of Dev that do not overflow.

   -->> Normalise <<--

   procedure Lemma_Positive_Safe_Division (
      Left  : in Real;
      Right : in Sqrt_Result) is
   begin
      pragma Assert (Normalised_Sample'(Left / Right) <= NL);
   end Lemma_Positive_Safe_Division;

   procedure Lemma_Negative_Safe_Division (
      Left  : in Real;
      Right : in Sqrt_Result) is
      -- Thanks to Álvaro and Lucía for helping me debug this Lemma :)
   begin
      pragma Assert (Left < 0.0);
      pragma Assert (Right > 0.0);
      pragma Assert (NF < 0.0);
      pragma Assert (Right * NF <= Left);
   end Lemma_Negative_Safe_Division;

   function Normalise (
      Item : in Sample_Epoch)
      return Normalised_Epoch is
      μ      : constant Sample_Type := Mean.Mean (Item);
      μ2     : constant Real := Real (μ) * Real (μ);
      Sum2   : constant Real := Variance.Variance (Item);
      Dev    : Sqrt_Result;
      Result : Normalised_Epoch := [others => Normalised_Sample'Last];
      Value  : Real;
   begin
      if Sum2 < μ2 then
         -- This is a very interesting case, because usually:
         --
         --    Σ x_i² >= (Σ x_i)², if ∀ x_i : |x_i| >= 1.0
         --
         -- However for |x_i| < 1.0, such as:
         --
         --    x_1 = x_2 = 0.1
         --
         -- We get that
         --
         --    0.1² + 0.1² = 0.02 < (0.1 + 0.1)² = 0.04
         --
         -- TODO: Signalise it
         return Result;
      end if;
      pragma Assert (Sum2 - μ2 >= 0.0);
      Dev := Sqrt (Sum2 - μ2);
      pragma Assert (Dev >= 0.0);
      if Dev = 0.0 then
         -- We cannot divide by 0
         -- TODO: Signalise it
         return Result;
      else
         pragma Assert (Dev > 0.0);
         for I in Result'Range loop
            -- Item (I) ∈ [First, Last]
            -- μ        ∈ [First, Last]
            -- Dev      ∈ (0.0, Max (abs First, abs Last))
            -- ---
            -- Item (I) - μ ∈ [First - Last, Last - First]
            Value := Real (Item (I)) - Real (μ);
            pragma Assert (
               Value in Real (Sample_Type'First) - Real (Sample_Type'Last)
                     .. Real (Sample_Type'Last) - Real (Sample_Type'First));
            if Value = 0.0 then
               Result (I) := 0.0;
            elsif Value > 0.0 and then Dev >= Value / NL then
               Lemma_Positive_Safe_Division (Value, Dev);
               Result (I) := Value / Dev;
            elsif Value < 0.0 and then Dev >= Value / NF then
               -- TODO: Properly prove this shit using the other lemma.
               Lemma_Negative_Safe_Division (Value, Dev);
               Result (I) := Value / Dev;
            else
               -- TODO: Signalise it
               Result (I) := (if Value > 0.0 then Normalised_Sample'Last
                                 else Normalised_Sample'First);
            end if;
         end loop;
         return Result;
      end if;
   end Normalise;

end Detector.Details.Normalise;
