--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-batches-validator__single_threaded.adb                |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Batches.Validator with SPARK_Mode is

   procedure Validate (
      Signal   : in     Sample_Array;
      Batch    : in     Batch_Type;
      Seizures : in     Span_Array;
      Quality  :    out Quality_Metrics) is

      Count        : constant Count_Type := (Signal'Length - Epoch_Size)
                                            / Stride_Size;
      Detections   : array (Positive_Count_Type range 1 .. Count) of Boolean;
      Copy         : Batch_Type := Batches.Copy (Batch);
      Index, Last  : Positive_Count_Type := Signal'First;
      Span         : Span_Type;
      Non_Seizures : Span_Array (Seizures'First .. Seizures'Last + 1);

   begin
      Quality := (others => 0);
      pragma Assert (Count * Stride_Size <= Signal'Length - Epoch_Size);

      -- Fill the detections array.

      for I in Detections'Range loop
         pragma Loop_Invariant (Index = Signal'First + (I - 1) * Stride_Size);
         Is_Seizure (Copy, Signal (Index .. Index + Epoch_Size - 1),
                     Detections (I));

      -- if Detections (I) then
      --    declare
      --       PSD_1    : Feature_Type;
      --       PSD_2    : Feature_Type;
      --       PSD_3    : Feature_Type;
      --       Energy   : Feature_Type;
      --       Max_Dist : Feature_Type;
      --       DTW_Dist : Feature_Type;
      --    begin
      --       Get_Features (Copy, Signal (Index .. Index + Epoch_Size - 1),
      --                     PSD_1, PSD_2, PSD_3, Energy, Max_Dist, DTW_Dist);
      --       Debug_IO.Put_Line ("Dump at" & I'Image);
      --       Debug_IO.Put ("Seizure? = ");
      --       Debug_IO.Put_Line (Detections (I)'Image);
      --       Debug_IO.Put ("PSD_1    ="); Debug_IO.Put_Line (PSD_1'Image);
      --       Debug_IO.Put ("PSD_2    ="); Debug_IO.Put_Line (PSD_2'Image);
      --       Debug_IO.Put ("PSD_3    ="); Debug_IO.Put_Line (PSD_3'Image);
      --       Debug_IO.Put ("Energy   ="); Debug_IO.Put_Line (Energy'Image);
      --       Debug_IO.Put ("Max_Dist ="); Debug_IO.Put_Line (Max_Dist'Image);
      --       Debug_IO.Put ("DTW      ="); Debug_IO.Put_Line (DTW_Dist'Image);
      --       Debug_IO.New_Line;
      --    end;
      -- end if;

         Index := Index + Stride_Size;
      end loop;

      -- Check first seizure regions for detections.

      for I in Seizures'Range loop
         pragma Loop_Invariant (Quality.True_Positives in 0 .. I - 1);
         pragma Loop_Invariant (Quality.False_Negatives in 0 .. I - 1);
         Span := Seizures (I);
         Span.First := Count_Type'Max (1, @ - Exclusion);
         Span.Last := Count_Type'Min (Count, @ + Exclusion);
         if (for some J in Span.First .. Span.Last => Detections (J)) then
            Quality.True_Positives  := @ + 1;
         else
            Quality.False_Negatives := @ + 1;
         end if;
      end loop;

      -- Finally check the non-seizure regions.

      Non_Seizures (Non_Seizures'First).First := Detections'First;
      for I in Seizures'Range loop
         Non_Seizures (I).Last := Seizures (I).First - 1;
         Non_Seizures (I + 1).First := Seizures (I).Last + 1;
      end loop;
      Non_Seizures (Non_Seizures'Last).Last := Count;

      for I in Non_Seizures'Range loop
         Span := Non_Seizures (I);
         Span.First := Count_Type'Min (Count, @ + Exclusion);
         Span.Last := Count_Type'Max (1, @ - Exclusion);
         Index := Span.First;
         while Index <= Span.Last loop
            pragma Loop_Invariant (Quality.True_Negatives
                                    in 0 .. (Index - Span.First) / Chunk_Size);
            pragma Loop_Invariant (Quality.False_Positives
                                    in 0 .. (Index - Span.First) / Chunk_Size);
            Last := Count_Type'Min (Span.Last, Index + Chunk_Size);
            if (for some J in Index .. Last => Detections (J)) then
               Quality.False_Positives := @ + 1;
            else
               Quality.True_Negatives  := @ + 1;
            end if;
            Index := Index + Chunk_Size;
         end loop;
      end loop;

   end Validate;

end Detector.Batches.Validator;
