with Ada.Numerics;
with Safe_IO;

package body Detector.Algorithms with SPARK_Mode => On is

   -- TODO: Make it a parameter
   Welch_Window_Size : constant := 512;

   use Reals;

   Hann_Window : constant Sample_Array := [
      for I in Count_Type range 1 .. Welch_Window_Size =>
         0.5 - 0.5 * Cos (2.0 * Ada.Numerics.Pi * Real (I - 1) /
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

   function Omega (K, N : Count_Type) return Real is (
      -2.0 * Ada.Numerics.Pi * Real (K) / Real (N));

   function Exponent_Product (
      Factor : in Complex;
      K, N   : in Count_Type)
      return Complex is (
      Re => Factor.Re * Cos (Omega (K, N)) - Factor.Im * Sin (Omega (K, N)),
      Im => Factor.Re * Sin (Omega (K, N)) + Factor.Im * Cos (Omega (K, N)));

   function "+" (Left, Right : in Complex) return Complex is (
      Re => Left.Re + Right.Re,
      Im => Left.Im + Right.Im);

   function "-" (Left, Right : in Complex) return Complex is (
      Re => Left.Re - Right.Re,
      Im => Left.Im - Right.Im);

   procedure FFT (
      Input  : in     Sample_Array;
      Output :    out Complex_Array;
      Size   : in     Count_Type;
      Stride : in     Positive_Count_Type) is
      Half : constant Count_Type := Size / 2;
   begin
      if Size = 1 then
         Output (Output'First) := (Re => Input (Input'First), Im => 0.0);
      elsif Size mod 2 = 0 then
         FFT (
            Input  => Input (Input'First .. Input'Last),
            Output => Output (Output'First .. Output'First + Half - 1),
            Size   => Half,
            Stride => Stride * 2);
         FFT (
            Input  => Input (Input'First + Stride .. Input'Last),
            Output => Output (Output'First + Half .. Output'Last),
            Size   => Half,
            Stride => Stride * 2);
         for K in 0 .. Half - 1 loop
            declare
               Fst : constant Count_Type := Output'First + K;
               Snd : constant Count_Type := Output'First + K + Half;
               P   : constant Complex := Output (Fst);
               Q   : constant Complex := Exponent_Product (Output (Snd),
                                                           K, Size);
            begin
               Output (Fst) := P + Q;
               Output (Snd) := P - Q;
            end;
         end loop;
      else
         for K in 0 .. Size - 1 loop
            declare
               Result : Complex := (0.0, 0.0);
            begin
               for N in 0 .. Size - 1 loop
                  Result.Re := @ + Input (Input'First + N * Stride) *
                                 Cos (Omega (K * N, Size));
                  Result.Im := @ + Input (Input'First + N * Stride) *
                                 Sin (Omega (K * N, Size));
               end loop;
               Output (Output'First + K) := Result;
            end;
         end loop;
      end if;
   end FFT;

   procedure FFT (
      Input  : in     Sample_Array;
      Output :    out Complex_Array) is
   begin
      FFT (Input, Output, Input'Length, 1);
   end FFT;

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

-- procedure Welch (
--    Signal    : in Sample_Array;
--    Overlap   : in Positive_Count_Type;
--    Frequency : in Real) is
--    Pxx   : Real_Array (1 .. Epoch_Size / 2 + 1);
--    Steps : constant := (Signal'Length - Welch_WIndow_Size) / Overlap + 1;
--    Freq  : cosntant Real := Frequency / 2;
--    Output : Complex_Array (Input'Range);
-- begin
--    FFT (Input, Output);
--    for I

--   constexpr Channel operator()(Input_channel_of<Real> auto const & x, Real freq, Sample_count overlap) {
--      // Precondition((overlap > 0 && overlap < window_size_) && (std::ssize(x) >= window_size_));
--      Channel Pxx(window_size_ / 2 + 1);
--      Sample_count const steps  = (std::ssize(x) - window_size_) / overlap + 1;
--      freq                     /= 2;
--      auto const windows        = x | sliding_window_view(window_size_, window_size_ - overlap);
--      for (auto && win : windows) {
--        auto win_view = ranges::views::zip_with(std::multiplies<Real>{}, win, window_);
--        std::ranges::copy(win_view.begin(), win_view.end(), values_.begin());
--        Fftw::execute(plan_);
--        for (auto && [pxx_i, res_i] : ranges::views::zip(Pxx, result_)) {
--          pxx_i += norm_squared(res_i) / (normalisation_factor_ * freq);
--        }
--      }
--      Pxx |= ranges::actions::transform([steps](Real val) {
--        return val / steps;
--      });
--      return Pxx;
--    }



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
      and then (for some Pattern of Batch.Patterns =>
                  Dynamic_Time_Warping (Signal, Pattern, Batch.d_max_c)
                     <= Batch.d_max_c));

end Detector.Algorithms;
