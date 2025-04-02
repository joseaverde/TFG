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

   function Normalise (Item : in Epoch_Type)
      return Detector.Signals.Signal_Type is
      Result : Signals.Signal_Type (Item'Range);
   begin
      for I in Result'Range loop
         Result (I) := Normalisation.Normalise (Item (I));
      end loop;
      return Result;
   end Normalise;

end Detector.Batches;
