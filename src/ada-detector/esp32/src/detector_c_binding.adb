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

end Detector_C_Binding;
