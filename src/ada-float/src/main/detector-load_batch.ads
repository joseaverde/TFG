with Ada.Text_IO;

procedure Detector.Load_Batch (
   Result :    out Batch_Type;
   Valid  : in out Boolean) with
   SPARK_Mode => On,
   Relaxed_Initialization => Result,
   Post   => Result'Initialized = Valid,
   Global => (In_Out => Ada.Text_IO.File_System);
