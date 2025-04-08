generic
   with procedure Read (Item : out Stride_Type);
   with procedure Notify_Seizure;
   with procedure Notify_Nothing;
procedure Detector.Batches.Runner (
   Batch : in out Batch_Type) with
   No_Return  => True,
   Pure       => True,
   SPARK_Mode => On;
