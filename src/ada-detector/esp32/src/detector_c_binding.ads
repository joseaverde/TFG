with Detector, Detector.Signals, Default_Detector, Interfaces.C;
with Detector.Signals.Batch_Normalisation;
use Interfaces.C;

package Detector_C_Binding with
   SPARK_Mode,
   Abstract_State => (State with External => (
                        Async_Writers    => True,
                        Async_Readers    => False,
                        Effective_Reads  => True,
                        Effective_Writes => False)),
   Initializes => State
is

   subtype Feature_Type is Default_Detector.Feature_Type;
   subtype Epoch_Signal is
      Detector.Signals.Signal_Type (1 ..  Default_Detector.Epoch_Size);
   type Epoch_Access is access Epoch_Signal;
   subtype Batch_Normalised_Epoch is
      Detector.Signals.Batch_Normalisation.
         Normalised_Signal (Epoch_Signal'Range);
   type Batch_Normalised_Access is access Batch_Normalised_Epoch;

   -- Simple benchmarking functions --

   procedure Read_Sample (Sample : out Interfaces.C.int) with
      Global            => (In_Out => State),
      Import            => True,
      Convention        => C,
      External_Name     => "eeg_read_sample",
      Always_Terminates => True;

   procedure Max_Distance (
      Epoch  : in     Epoch_Access;
      Result :    out Feature_Type) with
      Global            => (In_Out => State),
      Export            => True,
      Convention        => C,
      External_Name     => "eeg_max_distance",
      Always_Terminates => True;

   procedure Mean (
      Epoch  : in     Epoch_Access;
      Result :    out Feature_Type) with
      Global            => (In_Out => State),
      Export            => True,
      Convention        => C,
      External_Name     => "eeg_mean",
      Always_Terminates => True;

   procedure Energy (
      Epoch  : in     Epoch_Access;
      Result :    out Feature_Type) with
      Global            => (In_Out => State),
      Export            => True,
      Convention        => C,
      External_Name     => "eeg_energy",
      Always_Terminates => True;

   procedure Batch_Normalise (Result : out Feature_Type) with
      Global            => (In_Out => State),
      Export            => True,
      Convention        => C,
      External_Name     => "old_eeg_batch_normalise",
      Always_Terminates => True;

   procedure Single_DTW (Result : out Feature_Type) with
      Global            => (In_Out => State),
      Export            => True,
      Convention        => C,
      External_Name     => "old_eeg_dtw",
      Always_Terminates => True;

   procedure Single_FFT (Result : out Feature_Type) with
      Global            => (In_Out => State),
      Export            => True,
      Convention        => C,
      External_Name     => "old_eeg_fft",
      Always_Terminates => True;

   procedure Seizure_Detector with
      Global            => (In_Out => State),
      Export            => True,
      Convention        => C,
      External_Name     => "seizure_detector",
      No_Return         => True;

private

   procedure Read_Signal (Item : out Detector.Signals.Signal_Type) with
      Global => (In_Out => State);

end Detector_C_Binding;
