with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Detector, Detector.Signals, Default_Detector;
with Detector.Signals.Mean;
with Detector.Signals.Quarter_Variance;

procedure Seizure_Detector_FTests with SPARK_Mode => Off is

   use Detector, Default_Detector, Default_Detector.Batches;
   package Sample_IO is new Fixed_IO (Sample_Type);
   package Count_IO is new Integer_IO (Count_Type);
   package Feature_IO is new Fixed_IO (Feature_Type);
   use Count_IO, Feature_IO, Sample_IO;

   type Sample_Array_Access is access Sample_Array;
   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Sample_Array,
      Name   => Sample_Array_Access);

   Pat_Count : Count_Type;
   Epochs    : Epoch_Array (Pattern_Count);
   Patterns  : Pattern_Array (Pattern_Count);
   PSD_1, PSD_2, PSD_3, Energy, Max_Dist, DTW : Span_Type;
begin

   Get (Pat_Count);
   Get (PSD_1.Low);     Get (PSD_1.High);
   Get (PSD_2.Low);     Get (PSD_2.High);
   Get (PSD_3.Low);     Get (PSD_3.High);
   Get (Energy.Low);    Get (Energy.High);
   Get (Max_Dist.Low);  Get (Max_Dist.High);
   DTW.Low := 0.0;      Get (DTW.High);
   for P in 1 .. Pat_Count loop
      for I in Epoch_Type'Range loop
         Get (Epochs (P) (I));
      end loop;
   end loop;

   Normalise_Epochs (Epochs, Patterns);

   declare
      Count  : Count_Type;
      Signal : Sample_Array_Access;
      Batch  : Batch_Type :=
         Make_Batch (PSD_1, PSD_2, PSD_3, Max_Dist, Energy, DTW,
                     Epochs (1 .. Pat_Count));
      Index  : Count_Type;
      Is_It  : Boolean;
      Data   : Epoch_Type;
      Epoch  : Signals.Signal_Type (1 .. Default_Detector.Epoch_Size);
      Normal : Pattern_Type;
      PSD    : Batches.Feature_Array (1 .. 3);
   begin

      Get (Count);
      Signal := new Sample_Array (1 .. Count);
      Put_Line (Count'Image);
      for I in 1.. Count loop
         Get (Signal (I));
      end loop;

      Put ("Ada ftests");
      Put (" "); Put (Default_Detector.Stride_Size, 1);
      Put (" "); Put (Strides_Per_Epoch, 1);
      Put_Line (" Binary Fixed Point");
      Index := Signal.all'First;
      while Index <= Count - Epoch'Length + 1 loop
         Data := Signal (Index .. Index + Epoch'Length - 1);
         Epoch := [for I in Epoch'Range =>
                     Normalisation.Normalise (Data (I))];
         Is_Seizure (Batch, Data, Is_It);
         Put (Is_It'Image);
         Batches.Power_Spectral_Densities (Epoch, PSD (1), PSD (2), PSD (3));
         Put (" "); Put (PSD (1), 1);
         Put (" "); Put (PSD (2), 1);
         Put (" "); Put (PSD (3), 1);
         Put (" "); Put (Batches.Energy (Epoch), 1);
         Put (" "); Put (Batches.Max_Distance (Epoch), 1);

         Normalise (Epoch, Normal);
         for I in 1 .. Pat_Count loop
            Put (" ");
            Put (Batches.Dynamic_Time_Warping (Normal, Patterns (I), 16), 1);
         end loop;

         New_Line;

         Index := Index + Default_Detector.Stride_Size;
      end loop;

      Free (Signal);

   end;

end Seizure_Detector_FTests;
