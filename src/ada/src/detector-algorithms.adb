with Ada.Numerics;

package body Detector.Algorithms with SPARK_Mode => On is

   Hann_Window : constant Sample_Array := [
      for I in Count_Type range 1 .. Epoch_Size =>
         0.5 - 0.5 * Reals.Cos (2.0 * Ada.Numerics.Pi * Real (I - 1) /
                                 Real (Epoch_Size - 1))];
   Correction_Factor : constant Real :=
      [for C of Hann_Window => C ** 2]'Reduce ("+", 0.0);

   function Simpson (
      Signal : in Sample_Array;
      dx     : in Sample)
      return Real is
      Result : Real := 0.0;
   begin
      for I in Signal'First .. Signal'Last - 2 loop
         Result := Signal (I) + 4.0 * Signal (I + 1) + Signal (I + 2);
      end loop;
      Result := Result * dx / 3.0;
      if Signal'Length > 2 and then Signal'Length mod 2 = 0 then
         Result := Result + dx * (
                   5.0 * Signal (Signal'Last)
                 + 8.0 * Signal (Signal'Last - 1)
                 -       Signal (Signal'Last - 2)) / 12.0;
      end if;
      return Result;
   end Simpson;

-- procedure FFT (
--    Input  : in     Real_Array;
--    Output :    out Complex_Array;
--    Length : in     Positive_Count_Type;
--    Stride : in     Positive_Count_Type) is
-- begin
--    if Length = 1 then
--       Output (Output'First) = Input (Input'First);
--    elsif Length mod 2 = 0 then
--       FFT (
--          Input  => Input,
--          Output => Output (Output'First .. Output'First + Length / 2 - 1),
--          Length => Length / 2,
--          Stride => 2 * Stride);
--       FFT (
--          Input  => Input (Input'First + Stride - 1 .. Input'Last),
--          Output => Output (Output'First + Length / 2 .. Output'Last),
--          Length => Length / 2,
--          Stride => 2 * Stride);
--       for K in 0 .. Length / 2 - 1 loop
--          declare
--             -- p = output[k]
--             -- q = exp(-2πk/N) * output[k + N/2]
--             --   = [cos(-2πk/N) + i sin(-2πk/N)] * output[k + N/2]
--             -- output[k] = p + q
--             -- output[k + N/2] = p - q
--             q_s : constant Real := Sin

--       end loop;

   function Mean (
      Signal : in Sample_Array)
      return Sample is
      Result : Real := Signal (Signal'First);
   begin
      for I in Signal'First + 1 .. Signal'Last loop
         Result := Result + Signal (I);
      end loop;
      return Result / Real (Signal'Length);
   end Mean;

   function Energy (
      Signal : in Sample_Array)
      return Real is
      Mean   : constant Sample := Algorithms.Mean (Signal);
      Result : Real := 0.0;
   begin
      for I in Signal'Range loop
         Result := Result + (Signal (I) - Mean) ** 2;
      end loop;
      return Result / Real (Signal'Length);
   end Energy;

   function Max_Distance (
      Signal : in Sample_Array)
      return Real is
      Min : Sample := Signal (Signal'First);
      Max : Sample := Signal (Signal'First);
   begin
      pragma Assert (Min <= Max);
      for I in Signal'First + 1 .. Signal'Last loop
         Min := Sample'Min (Min, Signal (I));
         Max := Sample'Max (Max, Signal (I));
         pragma Loop_Invariant (Min <= Max);
      end loop;
      return Real (Max) - Real (Min);
   end Max_Distance;

   function Power_Spectral_Density (
      Signal             : in Sample_Array;
      Sampling_Frequency : in Sample;
      Low                : in Sample;
      High               : in Sample)
      return Real is
   begin
      return Simpson (Signal, 0.0);
   end Power_Spectral_Density;

   function Dynamic_Time_Warping (
      Signal  : in Sample_Array;
      Pattern : in Sample_Array;
      Maximum : in Real)
      return Real is (0.0);

   function Is_Seizure (
      Signal : in Sample_Array;
      Batch  : in Batch_Type)
      return Boolean is (
               Within (Energy (Signal), Batch.Energy)
      and then Within (Max_Distance (Signal), Batch.Max_Dist)
      and then Within (Power_Spectral_Density (
                           Signal             => Signal,
                           Sampling_Frequency => PSD_Sampling_Frequency,
                           Low                => PSD_1_Bounds.Low,
                           High               => PSD_1_Bounds.High),
                       Batch.PSD_1)
      and then Within (Power_Spectral_Density (
                           Signal             => Signal,
                           Sampling_Frequency => PSD_Sampling_Frequency,
                           Low                => PSD_2_Bounds.Low,
                           High               => PSD_2_Bounds.High),
                       Batch.PSD_2)
      and then Within (Power_Spectral_Density (
                           Signal             => Signal,
                           Sampling_Frequency => PSD_Sampling_Frequency,
                           Low                => PSD_3_Bounds.Low,
                           High               => PSD_3_Bounds.High),
                       Batch.PSD_3)
      and then (for all Pattern of Batch.Patterns =>
                  Dynamic_Time_Warping (Signal, Pattern, Batch.d_max_c)
                     <= Batch.d_max_c));

end Detector.Algorithms;
