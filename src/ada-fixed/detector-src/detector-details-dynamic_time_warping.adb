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
      return Normalised_Epoch is
      μ      : constant Sample_Type := Mean.Mean (Item);
      μ2     : constant Real := Real (μ) * Real (μ);
      Sum2   : constant Real := Sum_Squares (Item) / Epoch_Size;
      Dev    : Real;
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
            Result (I) := Value / Dev;
         end loop;
         return Result;
      end if;
   end Normalise;

   function Saturated_Addition (
      Left  : in Normalised_Sample;
      Right : in Normalised_Sample)
      return Normalised_Sample is (
      (if Left > Normalised_Sample'Last - Right
         then Normalised_Sample'Last
         else Left + Right));

   function Saturated_Square (
      Item : in Normalised_Sample)
      return Normalised_Sample is
   begin
      if Item = Normalised_Sample'First then
         return Normalised_Sample'Last;
      elsif abs Item <= Sqrt_Last then
         return Item * Item;
      else
         return Normalised_Sample'Last;
      end if;
   end Saturated_Square;

   function Saturated_Distance (
      Left  : in Normalised_Sample;
      Right : in Normalised_Sample)
      return Normalised_Sample is (
      Saturated_Addition (Saturated_Square (Left), Saturated_Square (Right)));

   function Single_Dynamic_Time_Warping (
      Signal  : in Normalised_Epoch;
      Pattern : in Normalised_Epoch;
      Max     : in Feature_Type)
      return Feature_Type is
      pragma Unreferenced (Max);
      Diag_Cost : constant := 1;
      Band_Size : constant := 2 * Warping_Window + 3;
      Infinity  : constant Normalised_Sample := Normalised_Sample'Last;
      subtype Positive_Normalised_Sample is Normalised_Sample
         range 0.0 .. Normalised_Sample'Last;
      type Band_Pair is
         array (
            Boolean,
            Count_Type range 0 .. Band_Size - 1)
         of Positive_Normalised_Sample with
         Default_Component_Value => Infinity;
      Dist    : Normalised_Sample;
      Index   : Count_Type;
      Bands   : Band_Pair;
      Cost    : Boolean := False;
      Prev    : Boolean := True;
      X, Y, Z : Normalised_Sample;
      F, L    : Count_Type;
   begin
      for Row in Count_Type range 1 .. Epoch_Size loop
         Index := Count_Type'Max (1, Warping_Window - Row + 2);
         F := Count_Type'Max (1, Row - Warping_Window);
         L := Count_Type'Min (Epoch_Size, Row + Warping_Window);

         pragma Assert (Index in 1 .. Warping_Window + 1);
         pragma Assert (F in 1 .. Epoch_Size - Warping_Window);
         pragma Assert (L in Warping_Window + 1 .. Epoch_Size);

         -- Ranges
         pragma Assert (Epoch_Size - Warping_Window > Warping_Window + 1);
         if Row < Warping_Window + 1 then
            pragma Assert (Index = Warping_Window - Row + 2);
            pragma Assert (F = 1);
            pragma Assert (L = Row + Warping_Window);
            pragma Assert (L >= F);
            pragma Assert (L - F = Row + Warping_Window - 1);
            pragma Assert (L - F in Warping_Window .. 2 * Warping_Window - 1);
            pragma Assert (Index in 2 .. Warping_Window + 1);
            pragma Assert (Index + L - F = 2 * Warping_Window + 1);
         elsif Row = Warping_Window + 1 then
            pragma Assert (Index = 1);
            pragma Assert (F = 1);
            pragma Assert (L = Row + Warping_Window);
            pragma Assert (L >= F);
            pragma Assert (L - F = Row + Warping_Window - 1);
            pragma Assert (L - F = 2 * Warping_Window);
            pragma Assert (Index + L - F = 2 * Warping_Window + 1);
         elsif Row < Epoch_Size - Warping_Window then
            pragma Assert (Index = 1);
            pragma Assert (F = Row - Warping_Window);
            pragma Assert (L = Row + Warping_Window);
            pragma Assert (L >= F);
            pragma Assert (L - F = Warping_Window + Warping_Window);
            pragma Assert (L - F in 2 * Warping_Window .. 2 * Warping_Window);
            pragma Assert (Index + L - F = 2 * Warping_Window + 1);
         else
            pragma Assert (Row in Epoch_Size - Warping_Window .. Epoch_Size);
            pragma Assert (Index = 1);
            pragma Assert (F = Row - Warping_Window);
            pragma Assert (L = Epoch_Size);
            pragma Assert (L >= F);
            pragma Assert (L - F = Epoch_Size - Row + Warping_Window);
            pragma Assert (L - F in Warping_Window .. 2 * Warping_Window);
            pragma Assert (Index + L - F in Warping_Window + 1
                                         .. 2 * Warping_Window + 1);
         end if;
         pragma Assert (L >= F);
         pragma Assert (L - F in Warping_Window .. 2 * Warping_Window);
         pragma Assert (Index + L - F in Warping_Window + 1
                                      .. 2 * Warping_Window + 1);
         pragma Assert (Index in 1 .. Warping_Window + 1);
         pragma Assert (2 * Warping_Window + 1 = Band_Size - 2);

         for Col in F .. L loop
            Dist := Saturated_Distance (Signal (Row), Pattern (Col));
            if Row = 1 and Col = 1 then
               Bands (Cost, Index) := Dist;
            else
               Y := Saturated_Addition (Bands (Cost, Index - 1), Dist);
               X := Saturated_Addition (Bands (Prev, Index + 1), Dist);
               Z := Saturated_Addition (Bands (Prev, Index), Diag_Cost * Dist);
               Bands (Cost, Index) :=
                  Normalised_Sample'Min (Normalised_Sample'Min (X, Y), Z);
            end if;
            Index := Index + 1;
            pragma Loop_Invariant (Index = Index'Loop_Entry + Col - F + 1);
         end loop;
         Prev := not Prev;
         Cost := not Cost;
      end loop;
      return Feature_Type (Bands (Prev, Index - 1));
   end Single_Dynamic_Time_Warping;

end Detector.Details.Dynamic_Time_Warping;
