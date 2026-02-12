--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-batches-runner.adb                                    |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

generic
   with procedure Read (Item : out Stride_Type);
   with procedure Notify_Seizure;
   with procedure Notify_Nothing;
procedure Detector.Batches.Runner (
   Batch : in out Batch_Type) with
   No_Return, Preelaborate, SPARK_Mode;
