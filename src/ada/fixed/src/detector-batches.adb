--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-batches.adb                                           |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Batches with SPARK_Mode is

   procedure Is_Seizure (
      Batch  : in out Batch_Type;
      Epoch  : in     Epoch_Type;
      Result :    out Boolean) is
      Signal : Signals.Signal_Type (Epoch'Range);
      Normal : Pattern_Type;
      PSDs   : Feature_Array (1 .. 3) := [others => 0.0];
   begin
      Result := False;
      Normalise (Epoch, Signal);
      -- Max_Distance and Energy
      if Is_In (Max_Distance (Signal), Batch.Max_Dist)
         and then Is_In (Energy (Signal), Batch.Energy)
      then
         Power_Spectral_Densities (Signal, PSDs (1), PSDs (2), PSDs (3));
         if Is_In (PSDs (1), Batch.PSD_1)
            and then Is_In (PSDs (2), Batch.PSD_2)
            and then Is_In (PSDs (3), Batch.PSD_3)
         then
            -- DTW
            Normalise (Signal, Normal);
            Result :=
               (for some I in 1 .. Batch.Count =>
                  Dynamic_Time_Warping (Normal, Batch.Patterns (I),
                                             Warping_Window) / d_th
                  < Batch.d_max_c.High);
         end if;
      end if;

      -- Update lookback information
      if Result /= Batch.Was_Seizure then
         Batch.Streak := 0;
         Batch.Was_Seizure := Result;
      else
         if Batch.Streak < Batch.Lookback then
            Batch.Streak := Batch.Streak + 1;
            Result := False;
         end if;
      end if;

   end Is_Seizure;

   procedure Get_Features (
      Batch    : in     Batch_Type;
      Epoch    : in     Epoch_Type;
      PSD_1    :    out Feature_Type;
      PSD_2    :    out Feature_Type;
      PSD_3    :    out Feature_Type;
      Energy   :    out Feature_Type;
      Max_Dist :    out Feature_Type;
      DTW_Dist :    out Feature_Type) is
      Signal : Signals.Signal_Type (Epoch'Range);
      Normal : Pattern_Type;
   begin
      Normalise (Epoch, Signal);
      Normalise (Signal, Normal);
      Max_Dist := Batches.Max_Distance (Signal);
      Energy := Batches.Energy (Signal);
      Power_Spectral_Densities (Signal, PSD_1, PSD_2, PSD_3);
      DTW_Dist := Feature_Type'Last;
      for I in 1 .. Batch.Count loop
         DTW_Dist := Feature_Type'Min (@,
            Dynamic_Time_Warping (Normal, Batch.Patterns (I), Warping_Window));
      end loop;
   end Get_Features;

-- function Is_Seizure (
--    Item  : in Sample_Epoch;
--    Batch : in Batch_Type)
--    return Boolean is
--    PSD_1, PSD_2, PSD_3 : Feature_Type;
-- begin
--    if not Within (Max_Distance (Item), Batch.Max_Dist)
--       or else not Within (Energy (Item), Batch.Energy)
--    then
--       return False;
--    end if;
--    Power_Spectral_Density (Item, Feature_Type (Stride_Size),
--       PSD_1, PSD_2, PSD_3);
--    if not Within (PSD_1, Batch.PSD_1)
--       or else not Within (PSD_2, Batch.PSD_2)
--       or else not Within (PSD_3, Batch.PSD_3)
--    then
--       return False;
--    end if;

--    return (declare
--       Epoch : constant Normalised_Epoch := Normalise (Item);
--    begin
--       (for some I in 1 .. Batch.Count =>
--          Dynamic_Time_Warping (Epoch, Batch.Patterns (I), Batch.d_max_c)
--          < Batch.d_max_c));
-- end Is_Seizure;

   procedure Normalise_Epochs (
      Input  : in     Epoch_Array;
      Output :    out Pattern_Array) is
      Temp : Detector.Signals.Signal_Type (Pattern_Type'Range);
   begin
      Output := [others => [others => 0.0]];
      for I in Count_Type range 0 .. Input'Length - 1 loop
         Normalise (Input (Input'First + I), Temp);
         Normalise (Temp, Output (Output'First + I));
      end loop;
   end Normalise_Epochs;

   procedure Normalise (
      Input  : in     Epoch_Type;
      Output :    out Detector.Signals.Signal_Type) is
   begin
      Output := [others => 0.0];
      for I in Count_Type range 0 .. Input'Length - 1 loop
         Output (Output'First + I) :=
            Normalisation.Normalise (Input (Input'First + I));
      end loop;
   end Normalise;

   procedure Normalise (
      Input  : in     Detector.Signals.Signal_Type;
      Output :    out Pattern_Type) is
   begin
      Signals.Batch_Normalisation.Normalise (Input, Output);
   end Normalise;

   function Make_Batch (
      PSD_1, PSD_2, PSD_3, Max_Dist, Energy, DTW : in Span_Type;
      Patterns                                   : in Epoch_Array)
      return Batch_Type is
   begin
      return Batch : Batch_Type (Patterns'Length) do
         Batch.PSD_1    := PSD_1;
         Batch.PSD_2    := PSD_2;
         Batch.PSD_3    := PSD_3;
         Batch.Max_Dist := Max_Dist;
         Batch.Energy   := Energy;
         Batch.d_max_c  := DTW;
         Normalise_Epochs (Patterns, Batch.Patterns);
      end return;
   end Make_Batch;

   procedure Power_Spectral_Densities (
      Signal : in     Signals.Signal_Type;
      PSD_1  :    out Feature_Type;
      PSD_2  :    out Feature_Type;
      PSD_3  :    out Feature_Type) is
      pragma SPARK_Mode (Off);
      -- FIXME: Set it to SPARK_Mode => On
      Size   : constant Positive_Count_Type := Welch_Window_Size;
      Pxx    : Signals.Signal_Type (1 .. Size / 2 + 1);
      Fq_Res : constant Signals.Sample_Type :=
         Fixed_Integer (Stride_Size) / Fixed_Integer (Size);
      Scale  : Welch_Rescaling_Factor_Type;
      First  : Count_Type;
      Last   : Count_Type;

      Denormalise : constant Positive := Normalisation.Factor ** 2;

      function Rescale (
         Item : in Feature_Type)
         return Feature_Type is (
         Feature_Type'(Item * Scale) * Denormalise);
   begin
      Welch (Signal, Pxx, Stride_Size, Size, Size / 2, Scale);

      First := Count_Type (Feature_Type (2.0)  / Fq_Res);
      Last  := Count_Type (Feature_Type (12.0) / Fq_Res);
      PSD_1 := Rescale (Simpson (Pxx (First .. Last), Fq_Res));

      First := Count_Type (Feature_Type (12.0)  / Fq_Res);
      Last  := Count_Type (Feature_Type (18.0) / Fq_Res);
      PSD_2 := Rescale (Simpson (Pxx (First .. Last), Fq_Res));

      First := Count_Type (Feature_Type (18.0)  / Fq_Res);
      Last  := Count_Type (Feature_Type (35.0) / Fq_Res);
      PSD_3 := Rescale (Simpson (Pxx (First .. Last), Fq_Res));
   end Power_Spectral_Densities;

   -->> Setters & Getters <<--

   procedure Reset (Batch : in out Batch_Type) is
   begin
      Batch.Was_Seizure := False;
      Batch.Streak := 0;
   end Reset;

   procedure Set_PSD_1 (Batch : in out Batch_Type; Item : in Span_Type) is
   begin
      Batch.PSD_1 := Item;
   end Set_PSD_1;

   procedure Set_PSD_2 (Batch : in out Batch_Type; Item : in Span_Type) is
   begin
      Batch.PSD_2 := Item;
   end Set_PSD_2;

   procedure Set_PSD_3 (Batch : in out Batch_Type; Item : in Span_Type) is
   begin
      Batch.PSD_3 := Item;
   end Set_PSD_3;

   procedure Set_Energy (Batch : in out Batch_Type; Item : in Span_Type) is
   begin
      Batch.Energy := Item;
   end Set_Energy;

   procedure Set_Max_Dist (Batch : in out Batch_Type; Item : in Span_Type) is
   begin
      Batch.Max_Dist := Item;
   end Set_Max_Dist;

   procedure Set_DTW_Dist (Batch : in out Batch_Type; Item : in Span_Type) is
   begin
      Batch.d_max_c := Item;
   end Set_DTW_Dist;

   procedure Set_Pattern (
      Batch : in out Batch_Type;
      Index : in     Count_Type;
      Value : in     Pattern_Type) is
   begin
      Batch.Patterns (Index) := Value;
   end Set_Pattern;

end Detector.Batches;
