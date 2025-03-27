with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Detector; use Detector;
with Detector.Temp; use Detector.Temp;
with Emul;
with Detector.Batches;

procedure Seizure_Detector_FTests_New with SPARK_Mode => Off is

   package My_Batches is new Detector.Batches (
      Sample_Type        => Detector.Temp.Sample_Type,
      Feature_Type       => Detector.Temp.Feature_Type,
      Samples_Per_Stride => Detector.Temp.Stride_Size,
      Strides_Per_Epoch  => Detector.Temp.Strides_Per_Epoch);

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
   PSD_1  : Feature_Type;
   PSD_2  : Feature_Type;
   PSD_3  : Feature_Type;
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
      Batch.Patterns (P) := Detector.Temp.Normalise (Epoch);
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
      Power_Spectral_Density (Epoch, Feature_Type (Stride_Size),
                              PSD_1, PSD_2, PSD_3);
      Put (" ");
      Put (PSD_1'Image);
      Put (" ");
      Put (PSD_2'Image);
      Put (" ");
      Put (PSD_3'Image);
      Put (" ");
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

end Seizure_Detector_FTests_New;
