procedure Detector.Batches.Runner (
   Batch : in out Batch_Type) with
   SPARK_Mode => On
is
   Epoch : Epoch_Type := [others => 0.0];
   Is_It : Boolean;
begin
   loop
      Epoch (Epoch'First .. Epoch'Last - Stride_Size) :=
         Epoch (Epoch'First + Stride_Size .. Epoch'Last);
      Read (Epoch (Epoch'Last - Stride_Size + 1 .. Epoch'Last));
      Is_Seizure (Batch, Epoch, Is_It);
      if Is_It then
         Notify_Seizure;
      else
         Notify_Nothing;
      end if;
   end loop;
end Detector.Batches.Runner;
