with Ada.Text_IO;
with Detector.Signals.Batch_Normalisation;
with Detector.Batches.Runner;
with Detector.Signals.Fast_Fourier_Transform;
with Safe_Time;

package body Detector_C_Binding with
   SPARK_Mode,
   Refined_State => (State => V)
is

   V : int with Import, Convention => C,
     Volatile,
     Async_Writers    => True,
     Async_Readers    => False,
     Effective_Reads  => True,
     Effective_Writes => False;

   subtype Epoch_Signal is
      Detector.Signals.Signal_Type (1 ..  Default_Detector.Epoch_Size);

   procedure Read_Signal (Item : out Detector.Signals.Signal_Type) is
      use Default_Detector;
      Sample : int;
   begin
      for I in Item'Range loop
         Read_Sample (Sample);
         Item (I) := Batches.Normalisation.Normalise ((
            if Sample < int (Min_Sample) then Min_Sample
            elsif Sample > int (Max_Sample) then Max_Sample
            else Sample_Type (Sample)));
      end loop;
   end Read_Signal;

   procedure Max_Distance (Result : out Feature_Type) is
      Epoch : Epoch_Signal;
   begin
      Read_Signal (Epoch);
      Result := Default_Detector.Batches.Max_Distance (Epoch);
   end Max_Distance;

   procedure Energy (Result : out Feature_Type) is
      Epoch : Epoch_Signal;
   begin
      Read_Signal (Epoch);
      Result := Default_Detector.Batches.Energy (Epoch);
   end Energy;

   procedure Batch_Normalise (Result : out Feature_Type) is
      Epoch : Epoch_Signal;
   begin
      Read_Signal (Epoch);
      declare
         use Default_Detector.Batches;
         subtype Normalised_Signal is
            Detector.Signals.Batch_Normalisation.Normalised_Signal;
         Temp : Normalised_Signal (Epoch'Range);
      begin
         Normalise (Epoch, Temp);
         Result := Feature_Type (Temp (Temp'First))
                 * Feature_Type (Temp (Temp'Last));
      end;
   end Batch_Normalise;

   procedure Single_DTW (Result : out Feature_Type) is
      use Default_Detector.Batches, Detector.Signals.Batch_Normalisation;
      Left  : Epoch_Signal;
      Right : Epoch_Signal;
      N_L   : Normalised_Signal (Left'Range);
      N_R   : Normalised_Signal (Right'Range);
   begin
      Read_Signal (Left);
      Read_Signal (Right);
      Default_Detector.Batches.Normalise (Left, N_L);
      Default_Detector.Batches.Normalise (Right, N_R);
      Result := Dynamic_Time_Warping (N_L, N_R, 16);
   end Single_DTW;

   procedure Single_FFT (Result : out Feature_Type) is
      Size   : constant := Detector.Log_2 (Default_Detector.Welch_Size);
      use Detector.Signals;
      Input  : Signal_Type (1 .. Default_Detector.Welch_Size);
      Output : Complex_Signal (Input'Range);
      Scale  : Natural;
   begin
      Read_Signal (Input);
      Fast_Fourier_Transform (Input, Output, Size, Scale);
      Result := Feature_Type (2 ** Scale) * Output (Output'First).Re;
   end Single_FFT;

   procedure Seizure_Detector is
      use Ada.Text_IO, Safe_Time;

      Start : Time;
      Count : Natural := 0;

      procedure Notify_Nothing is
         Stop    : constant Time     := Clock;
         Elapsed : constant Duration := Stop - Start;
      begin
         Put ('.');
         Count := Count + 1;
         if Stop - Start >= 1.0 then
            New_Line;

            Put ("Elapsed:");
            Put (Float (Elapsed)'Image);
            Put (" s");
            New_Line;

            Put ("Ratio:  ");
            Put (Natural (Float (Count) / Float (Elapsed))'Image);
            Put (" epochs/second");
            New_Line;

            Start := Clock;
            Count := 0;
         end if;
      end Notify_Nothing;

      procedure Notify_Seizure is
      begin
         New_Line;
         Put_Line ("Seizure!");
         Notify_Nothing;
      end Notify_Seizure;

      procedure Read (Stride : out Default_Detector.Batches.Stride_Type) is
      begin
         Stride := [others => 0.0];
      end Read;

      Patterns : constant := 3;
      Batch    : Default_Detector.Batches.Batch_Type :=
         Default_Detector.Batches.Make_Batch (
            PSD_1    => (Feature_Type'First, Feature_Type'Last),
            PSD_2    => (Feature_Type'First, Feature_Type'Last),
            PSD_3    => (Feature_Type'First, Feature_Type'Last),
            Energy   => (Feature_Type'First, Feature_Type'Last),
            Max_Dist => (Feature_Type'First, Feature_Type'Last),
            DTW      => (0.0, 0.0),
            Patterns =>
               [for I in 1 .. Patterns =>
                  [for J in Default_Detector.Batches.Epoch_Type'Range =>
                     (Default_Detector.Sample_Type (I)
                        * Default_Detector.Sample_Type (J))]]);

      procedure Run is new Default_Detector.Batches.Runner (
         Read           => Read,
         Notify_Nothing => Notify_Nothing,
         Notify_Seizure => Notify_Seizure);

   begin
      Start := Clock;
      Run (Batch);
   end Seizure_Detector;

end Detector_C_Binding;
