with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Detector; use Detector;
with Emul;

procedure Seizure_Detector_FTests with SPARK_Mode => Off is
   type Sample_Array_Access is access Sample_Array;
   package Sample_IO is new Fixed_IO (Sample_Type);
   package Count_IO is new Integer_IO (Count_Type);
   package Feature_IO is new Fixed_IO (Feature_Type);
   use Count_IO, Sample_IO, Feature_IO;
   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Sample_Array,
      Name   => Sample_Array_Access);
   Batch  : Batch_Type;
   Signal : Sample_Array_Access;
   Count  : Count_Type;
   Index  : Count_Type;
   Epoch  : Sample_Epoch;
   Normal : Normalised_Epoch;
begin

   Get (Count);
   Batch := Batch_Type'(Count => Pattern_Index (Count), others => <>);
   Get (Batch.PSD_1.Low);    Get (Batch.PSD_1.High);
   Get (Batch.PSD_2.Low);    Get (Batch.PSD_2.High);
   Get (Batch.PSD_3.Low);    Get (Batch.PSD_3.High);
   Get (Batch.Energy.Low);   Get (Batch.Energy.High);
   Get (Batch.Max_Dist.Low); Get (Batch.Max_Dist.High);
   Get (Batch.d_max_c);
   for P in 1 .. Pattern_Index (Count) loop
      for I in Epoch'Range loop
         Get (Epoch (I));
      end loop;
      Batch.Patterns (P) := Detector.Normalise (Epoch);
   end loop;

   Get (Count);
   Signal := new Sample_Array (1 .. Count);
   for I in 1 .. Count loop
      Get (Signal (I));
   end loop;

   -- Identification
   Put ("Ada ftests");
   Put (Count_Type'Image (Stride_Size));
   Put (Count_Type'Image (Strides_Per_Epoch));
   Put (" ");
   Put ("fixed2");
   New_Line;

   Index := Signal.all'First;
   while Index <= Count - Epoch_Size + 1 loop
      Epoch := Signal (Index .. Index + Epoch_Size - 1);
      Put (Is_Seizure (Epoch, Batch)'Image);
   -- Put (" ");
   -- Put (Power_Spectral_Density (
   --    Signal             => Epoch,
   --    Sampling_Frequency => PSD_Sampling_Frequency,
   --    Low                => PSD_1_Bounds.Low,
   --    High               => PSD_1_Bounds.High)'Image);
   -- Put (" ");
   -- Put (Power_Spectral_Density (
   --    Signal             => Epoch,
   --    Sampling_Frequency => PSD_Sampling_Frequency,
   --    Low                => PSD_2_Bounds.Low,
   --    High               => PSD_2_Bounds.High)'Image);
   -- Put (" ");
   -- Put (Power_Spectral_Density (
   --    Signal             => Epoch,
   --    Sampling_Frequency => PSD_Sampling_Frequency,
   --    Low                => PSD_3_Bounds.Low,
   --    High               => PSD_3_Bounds.High)'Image);
   -- Put (" ");
      Put (Energy (Epoch)'Image);
      Put (" ");
      Put (Max_Distance (Epoch)'Image);
      Normal := Normalise (Epoch);
      for I in 1 .. Batch.Count loop
         Put (" ");
         Put (Dynamic_Time_Warping (
            Normal, Batch.Patterns (I), Batch.d_max_c)'Image);
      end loop;
      New_Line;
      Index := Index + Stride_Size;
   end loop;

   Free (Signal);

end Seizure_Detector_FTests;
