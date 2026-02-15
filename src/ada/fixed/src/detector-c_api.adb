--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-c_api.adb                                             |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals;

package body Detector.C_API with
   SPARK_Mode,
   Refined_State => (State => Batch)
is

   package Batches renames Default_Detector.Batches;

   Batch : Batches.Batch_Type :=
      Batches.Make_Batch (
         PSD_1    => (Feature_Type'First, Feature_Type'Last),
         PSD_2    => (Feature_Type'First, Feature_Type'Last),
         PSD_3    => (Feature_Type'First, Feature_Type'Last),
         Energy   => (Feature_Type'First, Feature_Type'Last),
         Max_Dist => (Feature_Type'First, Feature_Type'Last),
         DTW      => (0.0, 0.0),
         Patterns =>
            [for I in 1 .. Pattern_Count =>
               [for J in Batches.Epoch_Type'Range =>
                  0.0]]);

   function Batch_Pattern_Count return Count_Type is (
      Batches.Get_Pattern_Count (Batch));

   -- Sample conversion functions --

   function To_Sample (Item : in double) return Sample_Type is
      pragma SPARK_Mode (Off);
   begin
      if Item > double (Sample_Type'Last) then
         return Sample_Type'Last;
      elsif Item < double (Sample_Type'First) then
         return Sample_Type'First;
      else
         return Sample_Type (Item);
      end if;
   end To_Sample;

   function To_Sample (Item : in int) return Sample_Type is
   begin
      if Item > int (Sample_Type'Last) then
         return Sample_Type'Last;
      elsif Item < int (Sample_Type'First) then
         return Sample_Type'First;
      else
         return Sample_Type (Item);
      end if;
   end To_Sample;

   function To_Double (Item : in Sample_Type) return double is
      Numerator : constant Int := Int (
         Fixed_Integer'(Item / Sample_Type'(Sample_Type'Delta)));
   begin
      return double (Numerator) / Sample_Type'Delta;
   end To_Double;

   -- Feature conversion functions --

   function To_Feature (Item : in double) return Feature_Type is
      pragma SPARK_Mode (Off);
   begin
      if Item > double (Feature_Type'Last) then
         return Feature_Type'Last;
      elsif Item < double (Feature_Type'First) then
         return Feature_Type'First;
      else
         return Feature_Type (Item);
      end if;
   end To_Feature;

-- function To_Feature (Item : in double) return Feature_Type is
--    Numerator : double;
--    Value     : Feature_Type'Base;
-- begin
--    -- Feature_Type cannot be negative.
--    if Item < 0.0 then
--       return 0.0;
--    end if;
--    -- Check it isn't too big, so that we can divide the delta.
--    if Item not in 0.0 .. 1.0E+80 then
--       return (if Item > 0.0 then Feature_Type'Last else Feature_Type'First);
--    end if;
--    Numerator := Item / Feature_Type'Delta;
--    -- Finally check the numerator
--    if Numerator > double (Long_Int'Last) then
--       return (if Item > 0.0 then Feature_Type'Last else Feature_Type'First);
--    end if;
--    pragma Assert (Numerator <= double (Long_Int'Last));
--    pragma Assert (Long_Int (double (Long_Int'Last)) = Long_Int'Last);
--    Lemma_Exact_Double_Conversion_Comparison (Long_Int'Last, Numerator);
--    pragma Assert (Long_Int (Numerator) <= Long_Int'Last);
--    Value := Fixed_Long_Integer (Long_Int (Numerator))
--             / Feature_Type'(Feature_Type'Delta);
--    if Value not in Feature_Type then
--       return (if Item > 0.0 then Feature_Type'Last else Feature_Type'First);
--    else
--       return Value;
--    end if;

   function To_Double (Item : in Feature_Type) return double is
      Numerator : constant Long_Int := Long_Int (
         Fixed_Long_Integer'(Item / Feature_Type'(Feature_Type'Delta)));
   begin
      return double (Numerator) / Feature_Type'Delta;
   end To_Double;

   -- Batch configuration functions --

   procedure Set_Max_Distance (Min, Max : in Feature_Type) is
   begin
      Batches.Set_Max_Dist (Batch, (Min, Max));
   end Set_Max_Distance;

   procedure Set_Energy (Min, Max : in Feature_Type) is
   begin
      Batches.Set_Energy (Batch, (Min, Max));
   end Set_Energy;

   procedure Set_PSD_1 (Min, Max : in Feature_Type) is
   begin
      Batches.Set_PSD_1 (Batch, (Min, Max));
   end Set_PSD_1;

   procedure Set_PSD_2 (Min, Max : in Feature_Type) is
   begin
      Batches.Set_PSD_2 (Batch, (Min, Max));
   end Set_PSD_2;

   procedure Set_PSD_3 (Min, Max : in Feature_Type) is
   begin
      Batches.Set_PSD_3 (Batch, (Min, Max));
   end Set_PSD_3;

   procedure Set_d_max_c (Value : in Feature_Type) is
   begin
      Batches.Set_DTW_Dist (Batch, (0.0, Value));
   end Set_d_max_c;

   procedure Set_Pattern (Pattern : in Epoch_Type; Index : in int) is
      Signal : Detector.Signals.Signal_Type (Pattern.Value'Range);
      Normal : Batches.Pattern_Type;
   begin
      pragma Assert (Index in 0 .. Pattern_Count - 1);
      pragma Assert (Batches.Get_Pattern_Count (Batch) = Pattern_Count);
      Batches.Normalise (Pattern.Value, Signal);
      Batches.Normalise (Signal, Normal);
      Batches.Set_Pattern (Batch, Count_Type (Index + 1), Normal);
   end Set_Pattern;

   procedure Reset is
   begin
      Batches.Reset (Batch);
   end Reset;

   -- Detection functions

   procedure Is_Seizure (Item : in Epoch_Type; Result : out C_bool) is
      Temp : Boolean;
   begin
      Batches.Is_Seizure (Batch, Item.Value, Temp);
      Result := C_bool (Temp);
   end Is_Seizure;

   -- Feature functions

   function Max_Distance (Item : in Epoch_Type) return Feature_Type is
      Signal : Detector.Signals.Signal_Type (Item.Value'Range);
   begin
      Batches.Normalise (Item.Value, Signal);
      return Batches.Max_Distance (Signal);
   end Max_Distance;

   function Energy (Item : in Epoch_Type) return Feature_Type is
      Signal : Detector.Signals.Signal_Type (Item.Value'Range);
   begin
      Batches.Normalise (Item.Value, Signal);
      return Batches.Energy (Signal);
   end Energy;

   function DTW (Item : in Epoch_Type; Index : in int) return Feature_Type is
      Signal : Detector.Signals.Signal_Type (Item.Value'Range);
      Normal : Batches.Pattern_Type;
   begin
      pragma Assert (Index in 0 .. Pattern_Count - 1);
      pragma Assert (Batches.Get_Pattern_Count (Batch) = Pattern_Count);
      Batches.Normalise (Item.Value, Signal);
      Batches.Normalise (Signal, Normal);
      return Batches.Dynamic_Time_Warping (
               Left  => Normal,
               Right => Batches.Get_Pattern (Batch, Count_Type (Index + 1)),
               Warping_Window => Default_Warping_Window);
   end DTW;

   procedure PSDs (
      Item : in Epoch_Type; PSD_1, PSD_2, PSD_3 : out Feature_Type) is
      Signal : Detector.Signals.Signal_Type (Item.Value'Range);
   begin
      Batches.Normalise (Item.Value, Signal);
      Batches.Power_Spectral_Densities (Signal, PSD_1, PSD_2, PSD_3);
   end PSDs;

end Detector.C_API;
