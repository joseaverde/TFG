with Ada.Text_IO;
with Safe_Time;
with Detector.Batches.Runner;
with Default_Detector;
with Utility.IO;
procedure Seizure_Detector_Fixed with SPARK_Mode => On, No_Return is
   use Default_Detector, Default_Detector.Batches;
   Patterns : constant := 3;
   Batch    : Batch_Type := Make_Batch (
      PSD_1    => (Feature_Type'First, Feature_Type'Last),
      PSD_2    => (Feature_Type'First, Feature_Type'Last),
      PSD_3    => (Feature_Type'First, Feature_Type'Last),
      Energy   => (Feature_Type'First, Feature_Type'Last),
      Max_Dist => (Feature_Type'First, Feature_Type'Last),
      DTW      => (0.0, 0.0),
      Patterns => [for I in 1 .. Patterns =>
                     [for J in Epoch_Type'Range =>
                        (Sample_Type (I) * Sample_Type (J))]]);
   Start : Safe_Time.Time;
   Count : Natural := 0;
   Max   : constant := 2000;

   procedure Notify is
      use Safe_Time, Ada.Text_IO;
      procedure Put is new Utility.IO.Put_Float (Float);
      procedure Put is new Utility.IO.Put_Fixed (Duration);
      Stop : Time;
   begin
      if Count >= Max then
         Count := 0;
         Stop := Clock;

         Put ("Elapsed: ");
         Put (Stop - Start);
         Put ("s");
         New_Line;

         Put (Float (Max) / Float (Stop - Start));
         Put_Line (" epochs/second");
         Start := Clock;
      end if;
      Count := Count + 1;
   end Notify;

   procedure Notify_Seizure is
   begin
      Notify;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Seizure");
   end Notify_Seizure;

   procedure Notify_Nothing is
   begin
      Notify;
   end Notify_Nothing;

   procedure Read (Item : out Stride_Type) is
      use all type Detector.Count_Type;
   begin
      for I in Item'Range loop
         Item (I) := Sample_Type ((if I mod 2 = 0 then I else -I));
      end loop;
   end Read;

   procedure Run is new Batches.Runner (
      Read           => Read,
      Notify_Seizure => Notify_Seizure,
      Notify_Nothing => Notify_Nothing);

begin
   Start := Safe_Time.Clock;
   Run (Batch);
end Seizure_Detector_Fixed;
