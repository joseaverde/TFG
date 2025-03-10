package body Detector.Details.Dynamic_Time_Warping with SPARK_Mode => On is

   function Saturated_Addition (
      Left  : in Base_Normalised_Sample;
      Right : in Base_Normalised_Sample)
      return Base_Normalised_Sample is (
      (if Left > Base_Normalised_Sample'Last - Right
         then Base_Normalised_Sample'Last
         else Left + Right));

   function Saturated_Square (
      Item : in Normalised_Sample)
      return Base_Normalised_Sample is
   begin
      return Item * Item;
   end Saturated_Square;

   function Saturated_Distance (
      Left  : in Normalised_Sample;
      Right : in Normalised_Sample)
      return Base_Normalised_Sample is (
      (Left - Right) * (Left - Right));

   function Single_Dynamic_Time_Warping (
      Signal  : in Normalised_Epoch;
      Pattern : in Normalised_Epoch;
      Max     : in Feature_Type)
      return Feature_Type is
      pragma Unreferenced (Max);
      Diag_Cost : constant := 1;
      Band_Size : constant := 2 * Warping_Window + 3;
      Infinity  : constant Base_Normalised_Sample :=
         Base_Normalised_Sample'Last;
      subtype Positive_Normalised_Sample is Base_Normalised_Sample
         range 0.0 .. Base_Normalised_Sample'Last;
      type Band_Pair is
         array (
            Boolean,
            Count_Type range 0 .. Band_Size - 1)
         of Positive_Normalised_Sample with
         Default_Component_Value => Infinity;
      Dist    : Base_Normalised_Sample;
      Index   : Count_Type;
      Bands   : Band_Pair;
      Cost    : Boolean := False;
      Prev    : Boolean := True;
      X, Y, Z : Base_Normalised_Sample;
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
            -- TODO: A good optimisation is to make the square not saturating
            --       because if we add certain precondition to the normalised
            --       sample. We can make sure that most of the values will be
            --       near 0.0. Therefore there won't be an overflow (mostly).
            --       For values that are far outside the range, we can just
            --       ignore them.
            Dist := Saturated_Distance (Signal (Row), Pattern (Col));
            if Row = 1 and Col = 1 then
               Bands (Cost, Index) := Dist;
            else
               Y := Saturated_Addition (Bands (Cost, Index - 1), Dist);
               X := Saturated_Addition (Bands (Prev, Index + 1), Dist);
               Z := Saturated_Addition (Bands (Prev, Index), Diag_Cost * Dist);
               Bands (Cost, Index) := Base_Normalised_Sample'Min (
                  Base_Normalised_Sample'Min (X, Y), Z);
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
