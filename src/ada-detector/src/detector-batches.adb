--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-batches.adb                                           |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals.Windows;
with Detector.Signals.Generic_Simpson;
with Detector.Signals.Generic_Welch;

package body Detector.Batches with SPARK_Mode is

   procedure Is_Seizure (
      Batch  : in out Batch_Type;
      Epoch  : in     Epoch_Type;
      Result :    out Boolean) is
      Signal : Signals.Signal_Type (Epoch'Range);
      Normal : Pattern_Type;
      PSDs   : Feature_Array (1 .. 3);
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
                  Is_In (Dynamic_Time_Warping (Normal, Batch.Patterns (I),
                                             Warping_Window),
                        Batch.d_max_c));
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
      procedure Hann_Welch is
         new Detector.Signals.Generic_Welch (Detector.Signals.Windows.Hann);
      function Simpson is
         new Detector.Signals.Generic_Simpson (Feature_Type);
      Size   : constant Positive_Count_Type := Welch_Window_Size;
      Pxx    : Signals.Signal_Type (1 .. Size / 2 + 1);
      Period : constant Signals.Sample_Type :=
         1.0 / Fixed_Integer (Stride_Size);
      Fq_Res : constant Signals.Sample_Type :=
         Fixed_Integer (Stride_Size) / Fixed_Integer (Size);
      First  : Count_Type;
      Last   : Count_Type;
   begin
      Hann_Welch (Signal, Pxx, Period, Size, Size / 2);

      First := Count_Type (Feature_Type (2.0)  / Fq_Res);
      Last  := Count_Type (Feature_Type (12.0) / Fq_Res);
      PSD_1 := Simpson (Signal (First .. Last), Fq_Res);

      First := Count_Type (Feature_Type (12.0)  / Fq_Res);
      Last  := Count_Type (Feature_Type (18.0) / Fq_Res);
      PSD_2 := Simpson (Signal (First .. Last), Fq_Res);

      First := Count_Type (Feature_Type (18.0)  / Fq_Res);
      Last  := Count_Type (Feature_Type (35.0) / Fq_Res);
      PSD_3 := Simpson (Signal (First .. Last), Fq_Res);
   end Power_Spectral_Densities;

end Detector.Batches;
