with Ada.Text_IO;
with Ada.Real_Time;
with Default_Detector;
with Detector;
with Detector.Batches.Validator;
with Detector.Containers.Vectors;

procedure Seizure_Detector_Validator with SPARK_Mode => On is

   package Unsafe is
      procedure Get (Item : out Default_Detector.Sample_Type);
      procedure Get (Item : out Default_Detector.Feature_Type);
      procedure Get (Item : out Detector.Count_Type);
      procedure Get (Item : out Default_Detector.Batches.Span_Type);
   end Unsafe;

   package body Unsafe with SPARK_Mode => Off is
      use Ada.Text_IO;
      package Sample_IO is new Fixed_IO (Default_Detector.Sample_Type);
      package Feature_IO is new Fixed_IO (Default_Detector.Feature_Type);
      package Count_IO is new Integer_IO (Detector.Count_Type);
      procedure Get (Item : out Default_Detector.Sample_Type) is
      begin
         Sample_IO.Get (Item);
      end Get;
      procedure Get (Item : out Default_Detector.Feature_Type) is
      begin
         Feature_IO.Get (Item);
      end Get;
      procedure Get (Item : out Detector.Count_Type) is
      begin
         Count_IO.Get (Item);
      end Get;
      procedure Get (Item : out Default_Detector.Batches.Span_Type) is
      begin
         Get (Item.Low);
         Get (Item.High);
      end Get;
   end Unsafe;

   subtype Raw_Sample is Default_Detector.Sample_Type;
   use all type Raw_Sample;
   package Vectors is new Detector.Containers.Vectors (Raw_Sample);
   package Validator is new Default_Detector.Batches.Validator (Cores => 8);

   use Ada.Text_IO, Default_Detector, Detector, Unsafe;
   package B renames Default_Detector.Batches;
   Pat_Count, Signal_Length                   : Count_Type;
   PSD_1, PSD_2, PSD_3, Energy, Max_Dist, DTW : B.Span_Type;
   Patterns : B.Epoch_Array (B.Pattern_Count);
   package Feature_IO is new Fixed_IO (Default_Detector.Feature_Type);

begin

   Get (Pat_Count);
   Get (PSD_1); Get (PSD_2); Get (PSD_3); Get (Energy); Get (Max_Dist);
   DTW.Low := 0.0; Get (DTW.High);
   for P in 1 .. Pat_Count loop
      for I in B.Epoch_Type'Range loop
         Get (Patterns (P) (I));
      end loop;
   end loop;
   Get (Signal_Length);

   Put_Line ("----- Batch -----");
   Put ("Patterns:"); Put (Pat_Count'Image); New_Line;
   Put ("PSD_1    in ["); Feature_IO.Put (PSD_1.Low, 1); Put (", ");
   Feature_IO.Put (PSD_1.High, 1); Put_Line (")");
   Put ("PSD_2    in ["); Feature_IO.Put (PSD_2.Low, 1); Put (", ");
   Feature_IO.Put (PSD_2.High, 1); Put_Line (")");
   Put ("PSD_3    in ["); Feature_IO.Put (PSD_3.Low, 1); Put (", ");
   Feature_IO.Put (PSD_3.High, 1); Put_Line (")");
   Put ("Energy   in ["); Feature_IO.Put (Energy.Low, 1); Put (", ");
   Feature_IO.Put (Energy.High, 1); Put_Line (")");
   Put ("Max_Dist in ["); Feature_IO.Put (Max_Dist.Low, 1); Put (", ");
   Feature_IO.Put (Max_Dist.High, 1); Put_Line (")");
   Put ("DTW      in ["); Feature_IO.Put (DTW.Low, 1); Put (", ");
   Feature_IO.Put (DTW.High, 1); Put_Line (")");
   New_Line;

   Validate_Signal : declare
      use Ada.Real_Time, Vectors;
      Batch    : constant B.Batch_Type := B.Make_Batch (
         PSD_1, PSD_2, PSD_3, Max_Dist, Energy, DTW,
         Patterns (1 .. Pat_Count));
      Epoch_Count : constant Count_Type :=
         (Signal_Length - Epoch_Size) / Stride_Size;
      Signal   : Vector := Create (Signal_Length, 0.0);
      Temp     : Nullable_Vector;
      Value    : Sample_Type;
      Metrics  : Validator.Quality_Metrics;
      Seizures : constant Validator.Span_Array := [
         (10196 + 1, 10236),
         (12267 + 1, 12294),
         (52132 + 1, 52172),
         (55015 + 1, 55066),
         (62920 + 1, 63010),
         (71390 + 1, 71483),
         (90925 + 1, 91026)];

      package Score_IO is new Fixed_IO (Validator.Score_Type);
      package Count_IO is new Integer_IO (Validator.Metric_Count_Type);
      package Duration_IO is new Fixed_IO (Duration);
      Start, Stop : Time;
      use Score_IO, Count_IO, Duration_IO;
   begin
      for I in 1 .. Signal_Length loop
         Get (Value);
         Append (Signal, Value);
      end loop;

      Put_Line ("----- Seizures -----");
      for Seizure of Seizures loop
         Put (Seizure.First); Put (" .. "); Put (Seizure.Last);
         New_Line;
      end loop;
      New_Line;

      Start := Clock;
      Validator.Validate (
         Signal   => B.Sample_Array (Signal.Data (1 .. Signal.Last)),
         Batch    => Batch,
         Seizures => Seizures,
         Quality  => Metrics);
      Stop := Clock;

      Put_Line ("----- Quality metrics -----");
      Set_Col (12 + 1 * Validator.Metric_Count_Type'Width - 4); Put ("True");
      Set_Col (12 + 2 * Validator.Metric_Count_Type'Width - 5); Put ("False");
      New_Line;

      Put ("Positives: "); Set_Col (12);
         Put (Metrics.True_Positives); Put (Metrics.False_Positives);
      New_Line;

      Put ("Negatives: "); Set_Col (12);
         Put (Metrics.True_Negatives); Put (Metrics.False_Negatives);
      New_Line;

      Put ("Precision   = "); Put (Validator.Precision (Metrics));   New_Line;
      Put ("Sensitivity = "); Put (Validator.Sensitivity (Metrics)); New_Line;
      Put ("F1 Score    = "); Put (Validator.F1_Score (Metrics));    New_Line;
      Put ("Elapsed "); Put (To_Duration (Stop - Start), 1); Put_Line (" s");
      Put (Duration'(Duration (Epoch_Count) / To_Duration (Stop - Start)), 1);
      Put (" epochs/s");

      Temp := Signal;
      Free (Temp);
   end Validate_Signal;

end Seizure_Detector_Validator;
