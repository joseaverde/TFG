with Ada.Numerics.Elementary_Functions;
with Detector.Details.Mean;

package body Detector.Details.Dynamic_Time_Warping with SPARK_Mode => On is

   -->> Sum_Squares <<--

   function Acc_Sum_Squares (
      Item : in Real_Epoch)
      return Real_Epoch is
      Result : Real_Epoch := [others => 0.0];
   begin
      Result (Item'First) := Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I - 1) >= 0.0 and then Item (I) >= 0.0 and then
               Result (I - 1) in 0.0 .. Real (I - 1) * Max_Sq and then
               Result (I) = Result (I - 1) + Item (I) and then
               Result (I) in 0.0 .. Real (I) * Max_Sq));
         Result (Index) := Result (Index - 1) + Item (Index);
      end loop;
      return Result;
   end Acc_Sum_Squares;

   function Map_Square (
      Item : in Sample_Epoch)
      return Real_Epoch is
      Result : Real_Epoch := [others => 0.0];
   begin
      for Index in Item'Range loop
         Result (Index) := Real (Item (Index)) * Real (Item (Index));
         pragma Loop_Invariant (
            (for all I in Item'First .. Index =>
               Result (I) = Real (Item (I)) * Real (Item (I))));
      end loop;
      return Result;
   end Map_Square;

   function Sum_Squares (
      Item : in Sample_Epoch)
      return Real is
      Result : Real := 0.0;
      State  : constant Real_Epoch := Map_Square (Item) with Ghost;
   begin
      for I in Item'Range loop
         Result := Result + Real (Item (I)) * Real (Item (I));
         pragma Loop_Invariant (Result = Acc_Sum_Squares (State) (I));
      end loop;
      return Result;
   end Sum_Squares;

   -->> Square Root <<--

   function Sqrt (
      Item : in Real)
      return Real is
      pragma SPARK_Mode (Off);
      -- TODO: REPLACE IT WITH A NORMAL ALGORITHM
      use Ada.Numerics.Elementary_Functions;
      Result : constant Float := Float (Item) ** 0.5;
   begin
      return Real (Result);
   end Sqrt;

   -->> Normalise <<--

   function Normalise (
      Item : in Sample_Epoch)
      return Normalised_Array is
      μ      : constant Sample_Type := Mean.Mean (Item);
      μ2     : constant Real := Real (μ) * Real (μ);
      Sum2   : constant Real := Sum_Squares (Item) / Epoch_Size;
      Dev    : Real;
      Result : Normalised_Array := [others => Sample_Type'Last];
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
      Dev := Sum2 - μ2;
      pragma Assert (Dev >= 0.0);
      Dev := Sqrt (Dev);
      pragma Assert (Dev >= 0.0);
      if Dev = 0.0 then
         -- We cannot divide by 0
         -- TODO: Signalise it
         return Result;
      else
         pragma Assert (Dev > 0.0);
         pragma Assert (Dev <= Max_Sq);
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
            if Dev = 1.0 then
               -- (Item (I) - μ) / 1.0 ∈ [First - Last, Last - First]
               Result (I) := Value / Dev;
            elsif Dev < 1.0 then
               -- Worst case, it may overflow when Dev = Delta
               -- (Item (I) - μ) / Delta ∈ ...
               Result (I) := Value / Dev;
            else
               pragma Assert (Dev > 1.0);
               -- Best case, it is very small when Dev = Max_Abs
               -- (Item (I) - μ) / Max_Abs ∈ ...
               Result (I) := Value / Dev;
            end if;
         end loop;
         return Result;
      end if;
   end Normalise;

end Detector.Details.Dynamic_Time_Warping;
