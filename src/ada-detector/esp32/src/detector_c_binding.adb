with Detector.Signals.Batch_Normalisation;
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
         for I in Temp'Range loop
            Temp (I) := 1.0;
         end loop;
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

end Detector_C_Binding;
