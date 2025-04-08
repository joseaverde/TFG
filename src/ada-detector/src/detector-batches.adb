package body Detector.Batches with SPARK_Mode is

   pragma Warnings (Off, "aspect Unreferenced specified for ""Batch""");
   procedure Is_Seizure (
      Batch  : in out Batch_Type;
      Result :    out Boolean) is
   begin
      Result := False;
   end Is_Seizure;
   pragma Warnings (On, "aspect Unreferenced specified for ""Batch""");

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
      for I in Count_Type range 0 .. Input'Length - 1 loop
         Normalise (Input (Input'First + I), Temp);
         Normalise (Temp, Output (Output'First + I));
      end loop;
   end Normalise_Epochs;

   procedure Normalise (
      Input  : in     Epoch_Type;
      Output :    out Detector.Signals.Signal_Type) is
   begin
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

end Detector.Batches;
