with Ada.Unchecked_Deallocation;
with Detector.Algorithms, Detector.Load_Batch, Safe_IO;
with Seizure_Detector_Config;

procedure Seizure_Detector_FTests is
   use all type Detector.Count_Type;
   type Sample_Array_Access is access Detector.Sample_Array;
   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Detector.Sample_Array,
      Name   => Sample_Array_Access);
   procedure Get_Count is
      new Safe_IO.Generic_Get (
      Object_Type => Detector.Count_Type,
      From_String => Detector.Count_Type'Value);
   procedure Get_Sample is
      new Safe_IO.Generic_Get (
      Object_Type => Detector.Sample,
      From_String => Detector.Sample'Value);
   Batch  : Detector.Batch_Type;
   Valid  : Boolean := True;
   Signal : Sample_Array_Access;
   Count  : Detector.Count_Type;
   Index  : Detector.Count_Type;
   Epoch  : Detector.Sample_Array (1 .. Detector.Epoch_Size);
begin

   Detector.Load_Batch (Batch, Valid);
   if not Valid then
      Safe_IO.Put_Line ("Invalid batch file!");
      return;
   end if;

   Get_Count (Count, Valid);
   if not Valid then
      Safe_IO.Put_Line ("Invalid signal file!");
      return;
   end if;
   Signal := new Detector.Sample_Array (1 .. Count);
   for I in 1 .. Count loop
      Get_Sample (Signal (I), Valid);
   end loop;

   -- Identification
   Safe_IO.Put ("Ada ftests");
   Safe_IO.Put (Seizure_Detector_Config.Samples_Per_Stride'Image);
   Safe_IO.Put (Seizure_Detector_Config.Strides_Per_Epoch'Image);
   Safe_IO.Put (" ");
   Safe_IO.Put (Seizure_Detector_Config.Real_Type'Image);
   case Seizure_Detector_Config.Real_Type is
      when Seizure_Detector_Config.Fixed_2 =>
         null;
      when Seizure_Detector_Config.Fixed_10 =>
         null;
      when others =>
         null;
   end case;
   Safe_IO.New_Line;

   Index := Signal.all'First;
   while Index <= Count - Detector.Epoch_Size loop
      Epoch := Signal (Index .. Index + Detector.Epoch_Size - 1);
      Safe_IO.Put (Detector.Algorithms.Is_Seizure (Epoch, Batch)'Image);
      Safe_IO.Put (" ");
      Safe_IO.Put (Detector.Algorithms.Power_Spectral_Density (
         Signal             => Epoch,
         Sampling_Frequency => Detector.Algorithms.PSD_Sampling_Frequency,
         Low                => Detector.Algorithms.PSD_1_Bounds.Low,
         High               => Detector.Algorithms.PSD_1_Bounds.High)'Image);
      Safe_IO.Put (" ");
      Safe_IO.Put (Detector.Algorithms.Power_Spectral_Density (
         Signal             => Epoch,
         Sampling_Frequency => Detector.Algorithms.PSD_Sampling_Frequency,
         Low                => Detector.Algorithms.PSD_2_Bounds.Low,
         High               => Detector.Algorithms.PSD_2_Bounds.High)'Image);
      Safe_IO.Put (" ");
      Safe_IO.Put (Detector.Algorithms.Power_Spectral_Density (
         Signal             => Epoch,
         Sampling_Frequency => Detector.Algorithms.PSD_Sampling_Frequency,
         Low                => Detector.Algorithms.PSD_3_Bounds.Low,
         High               => Detector.Algorithms.PSD_3_Bounds.High)'Image);
      Safe_IO.Put (" ");
      Safe_IO.Put (Detector.Algorithms.Energy (Epoch)'Image);
      Safe_IO.Put (" ");
      Safe_IO.Put (Detector.Algorithms.Max_Distance (Epoch)'Image);
      for I in 1 .. Batch.Count loop
         Safe_IO.Put (" ");
         Safe_IO.Put (Detector.Algorithms.Dynamic_Time_Warping (
            Epoch, Batch.Patterns (I), Batch.d_max_c)'Image);
      end loop;
      Safe_IO.New_Line;
      Index := Index + Detector.Stride_Size;
   end loop;

   Free (Signal);

end Seizure_Detector_FTests;
