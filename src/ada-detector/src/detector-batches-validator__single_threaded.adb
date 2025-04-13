--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
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
      Count   : constant Count_Type := (Signal'Length - Epoch_Size)
                                       / Stride_Size;
      Copy    : Batch_Type          := Batches.Copy (Batch);
      Seizure : Positive_Count_Type := Seizures'First;
      Index   : Positive_Count_Type := Signal'First;
      Result  : Boolean;
      Expect  : Boolean;
   begin
      Quality := (others => 0);
      pragma Assert (Count * Stride_Size <= Signal'Length - Epoch_Size);
      for I in 1 .. Count loop
         pragma Loop_Invariant (Seizure in Seizures'Range);
         pragma Loop_Invariant (Index = Signal'First + (I - 1) * Stride_Size);
         pragma Loop_Invariant (Quality.True_Positives in 0 .. I - 1);
         pragma Loop_Invariant (Quality.True_Negatives in 0 .. I - 1);
         pragma Loop_Invariant (Quality.False_Positives in 0 .. I - 1);
         pragma Loop_Invariant (Quality.False_Negatives in 0 .. I - 1);
         -- Is it a seizure?
         if I < Seizures (Seizure).First then
            Expect := False;
         elsif I <= Seizures (Seizure).Last then
            Expect := True;
         else
            Expect := False;
            if Seizure < Seizures'Last then
               Seizure := Seizure + 1;
            end if;
         end if;

         -- Validate it
         Is_Seizure (Copy, Signal (Index .. Index + Epoch_Size - 1), Result);
         if Result then
            if Expect then
               Quality.True_Positives := @ + 1;
            else
               Quality.False_Positives := @ + 1;
            end if;
         else
            if Expect then
               Quality.False_Negatives := @ + 1;
            else
               Quality.True_Negatives := @ + 1;
            end if;
         end if;

         Index := Index + Stride_Size;
      end loop;
   end Validate;

end Detector.Batches.Validator;
