with Ada.Numerics;

package body Detector.Algorithms with SPARK_Mode => On is

   use Reals;

   Hann_Window : constant Real_Array (1 .. Welch_Window_Size) := [
      for I in 1 .. Welch_Window_Size =>
         0.5 - 0.5 * Cos (2.0 * Ada.Numerics.Pi * Real (I - 1) /
                          Real (Welch_Window_Size - 1))];
   Normalisation_Factor : constant Real :=
      [for C of Hann_Window => C ** 2]'Reduce ("+", 0.0);
   Sin_Omegas : constant
            array (Count_Type range 0 .. Welch_Window_Size - 1) of Sample := [
      for K in Count_Type range 0 .. Welch_Window_Size - 1 =>
         Sin (-2.0 * Ada.Numerics.Pi * Real (K) / Real (Welch_Window_Size))];
   Cos_Omegas : constant
            array (Count_Type range 0 .. Welch_Window_Size - 1) of Sample := [
      for K in Count_Type range 0 .. Welch_Window_Size - 1 =>
         Cos (-2.0 * Ada.Numerics.Pi * Real (K) / Real (Welch_Window_Size))];

   function Simpson (
      Signal : in Real_Array;
      dx     : in Sample)
      return Real is
      Result : Real := 0.0;
      I      : Count_Type := Signal'First + 2;
   begin
      while I in Signal'Range loop
         Result := @ + Signal (I - 2) + 4.0 * Signal (I - 1) + Signal (I);
         I := I + 2;
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

   function Sin_Omega (K, N : Count_Type'Base) return Real is (
      Sin_Omegas ((K * (Welch_Window_Size / N)) mod Welch_Window_Size));
   function Cos_Omega (K, N : Count_Type'Base) return Real is (
      Cos_Omegas ((K * (Welch_Window_Size / N)) mod Welch_Window_Size));

   function Exponent_Product (
      Factor : in Complex;
      K, N   : in Count_Type)
      return Complex is (
      Re => Factor.Re * Cos_Omega (K, N) - Factor.Im * Sin_Omega (K, N),
      Im => Factor.Re * Sin_Omega (K, N) + Factor.Im * Cos_Omega (K, N));

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
                                 Cos_Omega (K * N, Size);
                  Result.Im := @ + Input (Input'First + N * Stride) *
                                 Sin_Omega (K * N, Size);
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

   function Norm_Squared (Item : in Complex) return Real is (
      Item.Re ** 2 + Item.Im ** 2);

   procedure Welch (
      Signal    : in     Sample_Array;
      Pxx       :    out Welch_Array;
      Overlap   : in     Positive_Count_Type;
      Frequency : in     Real) is
      Steps  : constant Real := Real (
         (Signal'Length - Welch_Window_Size) / Overlap + 1);
      Factor : constant Real := 2.0 / (Normalisation_Factor * Frequency);
      Input  : Sample_Array (1 .. Welch_Window_Size);
      Output : Complex_Array (1 .. Welch_Window_Size);
      Index  : Count_Type := Signal'First;
   begin
      Pxx := [others => 0.0];
      while Index <= Signal'Last - Welch_Window_Size + 1 loop
         Input := [for I in Input'Range =>
                     Hann_Window (Hann_Window'First + I - Input'First) *
                     Signal (I - Input'First + Index)];
         FFT (Input, Output);
         for I in Pxx'Range loop
            Pxx (I) := @ + Norm_Squared (Output (I)) * Factor;
         end loop;
         Index := Index + (Welch_Window_Size - Overlap);
      end loop;
      Pxx := [for I in Pxx'Range => Pxx (I) / Steps];
   end Welch;

   function Power_Spectral_Density (
      Signal             : in Sample_Array;
      Sampling_Frequency : in Sample;
      Low                : in Sample;
      High               : in Sample)
      return Real is
      Fq_Res : constant Real := Sampling_Frequency / Real (Welch_Window_Size);
      First  : Count_Type;
      Last   : Count_Type;
      Pxx    : Welch_Array;
   begin
      Welch (Signal, Pxx, Welch_Window_Overlap, Sampling_Frequency);
      First := Count_Type (Rounding (Low / Fq_Res)) + Signal'First;
      Last  := Count_Type (Rounding (High / Fq_Res)) + Signal'First - 1;
      return Simpson (Pxx (First .. Last), Fq_Res);
   end Power_Spectral_Density;

   function Power_Spectral_Density (
      Signal             : in Sample_Array;
      Sampling_Frequency : in Sample;
      Bounds             : in Span_Array)
      return Real_Array is
      Result : Real_Array (Bounds'Range);
      Fq_Res : constant Real := Sampling_Frequency / Real (Welch_Window_Size);
      First  : Count_Type;
      Last   : Count_Type;
      Pxx    : Welch_Array;
   begin
      Welch (Signal, Pxx, Welch_Window_Overlap, Sampling_Frequency);
      for I in Bounds'Range loop
         First := Count_Type (Rounding (Bounds (I).Low / Fq_Res));
         Last  := Count_Type (Rounding (Bounds (I).High / Fq_Res));
         Result (I) := Simpson (
            Pxx (First + Signal'First .. Last + Signal'First - 1), Fq_Res);
      end loop;
      return Result;
   end Power_Spectral_Density;

   function Distance (Left, Right : in Real) return Real is (
      (Left - Right) * (Left - Right));

   function Single_Dynamic_Time_Warping (
      Signal  : in Epoch_Array;
      Pattern : in Epoch_Array;
      Max     : in Real)
      return Real is
      Diag_Cost : constant := 1.0;
      Band_Size : constant := 2 * Warping_Window + 3;
      Infinity  : constant Real := Real'Last;
      function Saturated_Addition (Left, Right : in Real) return Real is (
         Left + Right);
      type Band_Pair is
         array (
            Boolean,
            Count_Type range 0 .. Band_Size - 1)
         of Real with
         Default_Component_Value => Infinity;
      Dist    : Real;
      Index   : Count_Type := 1;
      Bands   : Band_Pair;
      Cost    : Boolean := False;
      Prev    : Boolean := True;
      X, Y, Z : Real;
      F, L    : Count_Type;
   begin
      for Row in Count_Type range 1 .. Epoch_Size loop
         Index := Count_Type'Max (0, Warping_Window - Row + 1) + 1;
         F := Count_Type'Max (0, Row - 1 - Warping_Window) + 1;
         L := Count_Type'Min (Epoch_Size - 1, Row - 1 + Warping_Window) + 1;
         for Col in F .. L loop
            Dist := Distance (Signal (Row), Pattern (Col));
            if Row = 1 and Col = 1 then
               Bands (Cost, Index) := Dist;
            else
               Y := Saturated_Addition (Bands (Cost, Index - 1), Dist);
               X := Saturated_Addition (Bands (Prev, Index + 1), Dist);
               Z := Saturated_Addition (Bands (Prev, Index), Diag_Cost * Dist);
               Bands (Cost, Index) := Real'Min (Real'Min (X, Y), Z);
            end if;
            Index := Index + 1;
         end loop;
         Prev := not Prev;
         Cost := not Cost;
      end loop;
      return Bands (Prev, Index - 1);
   end Single_Dynamic_Time_Warping;

   function Sum_Squares (
      Item : in Sample_Array)
      return Real is
      Result : Real := 0.0;
   begin
      for I in Item'Range loop
         Result := @ + Item (I) * Item (I);
      end loop;
      return Result;
   end Sum_Squares;

   function Normalise (
      Item : in Sample_Array)
      return Epoch_Array is
      Mean : constant Real := Algorithms.Mean (Item);
      Sum2 : constant Real := Sum_Squares (Item);
      Inv_Dev : constant Real :=
         1.0 / Sqrt ((Sum2 / Real (Epoch_Size)) - Mean * Mean);
   begin
      return [for I in Count_Type range 1 .. Epoch_Size =>
                (Item (I - 1 + Item'First) - Mean) * Inv_Dev];
   end Normalise;

   function Dynamic_Time_Warping (
      Signal  : in Sample_Array;
      Pattern : in Sample_Array;
      Maximum : in Real)
      return Real is (
      Single_Dynamic_Time_Warping (
         Signal  => Normalise (Signal),
         Pattern => Normalise (Pattern),
         Max     => Maximum));

   function Is_Seizure (
      Signal : in Sample_Array;
      Batch  : in Batch_Type)
      return Boolean is (
               Within (Energy (Signal), Batch.Energy)
      and then Within (Max_Distance (Signal), Batch.Max_Dist)
      and then (
         declare
            PSDS : constant Real_Array := Power_Spectral_Density (
               Signal             => Signal,
               Sampling_Frequency => PSD_Sampling_Frequency,
               Bounds             => PSD_Bounds);
         begin       Within (PSDS (1), Batch.PSD_1)
            and then Within (PSDS (2), Batch.PSD_2)
            and then Within (PSDS (3), Batch.PSD_3)
            and then (
               for some Pattern of Batch.Patterns =>
                  Dynamic_Time_Warping (Signal, Pattern, Batch.d_max_c)
                  <= Batch.d_max_c)));

end Detector.Algorithms;
