with Detector, Detector.Signals, Default_Detector, Interfaces.C;
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

   procedure Read_Sample (Sample : out Interfaces.C.int) with
      Global            => (In_Out => State),
      Import            => True,
      Convention        => C,
      External_Name     => "eeg_read_sample",
      Always_Terminates => True;

   procedure Max_Distance (Result : out Feature_Type) with
      Global            => (In_Out => State),
      Export            => True,
      Convention        => C,
      External_Name     => "eeg_max_distance",
      Always_Terminates => True;

   procedure Energy (Result : out Feature_Type) with
      Global            => (In_Out => State),
      Export            => True,
      Convention        => C,
      External_Name     => "eeg_energy",
      Always_Terminates => True;

   procedure Batch_Normalise (Result : out Feature_Type) with
      Global            => (In_Out => State),
      Export            => True,
      Convention        => C,
      External_Name     => "eeg_batch_normalise",
      Always_Terminates => True;

   procedure Single_DTW (Result : out Feature_Type) with
      Global            => (In_Out => State),
      Export            => True,
      Convention        => C,
      External_Name     => "eeg_dtw",
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
