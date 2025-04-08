function Detector.Signals.Generic_Dynamic_Time_Warping (
   Left           : in Batch_Normalisation.Normalised_Signal;
   Right          : in Batch_Normalisation.Normalised_Signal;
   Warping_Window : in Positive_Count_Type)
   return Result_Type is

   subtype Small_Real is Batch_Normalisation.Normalised_Sample;
   subtype Real is Batch_Normalisation.Base_Normalised_Sample
      range 0.0 .. Batch_Normalisation.Base_Normalised_Sample'Last;
   use all type Real;

   function Distance (Left, Right : in Small_Real) return Real with
      Global => null;
   function Saturated_Addition (Left, Right : in Real) return Real with
      Global => null;

   Diag_Cost : constant := 1;
   Band_Size : constant Positive_Count_Type := 2 * Warping_Window + 3;
   Infinity  : constant Real := Real'Last;

   type Band_Pair is
      array (Boolean, Count_Type range 0 .. Band_Size - 1) of Real with
      Default_Component_Value => Infinity;

   Dist    : Real;
   Index   : Count_Type := 1;
   Bands   : Band_Pair;
   Cost    : Boolean := False;
   Prev    : Boolean := True;
   X, Y, Z : Real;
   F, L    : Count_Type;

   function Distance (Left, Right : in Small_Real) return Real is (
      (Left - Right) * (Left - Right));
   function Saturated_Addition (Left, Right : in Real) return Real is (
      (if Left > Real'Last - Right then Real'Last else Left + Right));
   Size : constant Positive_Count_Type := Left'Length;

begin

   pragma Assert (Size >= 1);
   for Row in Count_Type range 1 .. Size loop
      Index := Count_Type'Max (1, Warping_Window - Row + 2);
      F := Count_Type'Max (1, Row - Warping_Window);
      L := Count_Type'Min (Size, Row + Warping_Window);

      pragma Assert (Index in 1 .. Warping_Window + 1);
      pragma Assert (F in 1 .. Size - Warping_Window);
      pragma Assert (L in Warping_Window + 1 .. Size);

      -- Ranges
      pragma Assert (Size - Warping_Window > Warping_Window + 1);
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
      elsif Row < Size - Warping_Window then
         pragma Assert (Index = 1);
         pragma Assert (F = Row - Warping_Window);
         pragma Assert (L = Row + Warping_Window);
         pragma Assert (L >= F);
         pragma Assert (L - F = Warping_Window + Warping_Window);
         pragma Assert (L - F in 2 * Warping_Window .. 2 * Warping_Window);
         pragma Assert (Index + L - F = 2 * Warping_Window + 1);
      else
         pragma Assert (Row in Size - Warping_Window .. Size);
         pragma Assert (Index = 1);
         pragma Assert (F = Row - Warping_Window);
         pragma Assert (L = Size);
         pragma Assert (L >= F);
         pragma Assert (L - F = Size - Row + Warping_Window);
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
         -- TODO: A good optimisation is to make the square not saturating
         --       because if we add certain precondition to the normalised
         --       sample. We can make sure that most of the values will be
         --       near 0.0. Therefore there won't be an overflow (mostly).
         --       For values that are far outside the range, we can just
         --       ignore them.
         Dist := Distance (Left (Row - 1 + Left'First),
                           Right (Col - 1 + Right'First));
         if Row = 1 and Col = 1 then
            Bands (Cost, Index) := Dist;
         else
            Y := Saturated_Addition (Bands (Cost, Index - 1), Dist);
            X := Saturated_Addition (Bands (Prev, Index + 1), Dist);
            Z := Saturated_Addition (Bands (Prev, Index), Diag_Cost * Dist);
            Bands (Cost, Index) := Real'Min (Real'Min (X, Y), Z);
         end if;
         Index := Index + 1;
         pragma Loop_Invariant (Index = Index'Loop_Entry + Col - F + 1);
      end loop;

      Prev := not Prev;
      Cost := not Cost;

   end loop;

   return Result_Type (Bands (Prev, Index - 1));

end Detector.Signals.Generic_Dynamic_Time_Warping;
