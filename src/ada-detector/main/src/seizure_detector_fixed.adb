with Ada.Text_IO;
with Ada.Real_Time;
with Detector.Batches.Runner;
with Default_Detector;
with Ada.Calendar;
procedure Seizure_Detector_Fixed with SPARK_Mode => On, No_Return is
   use Ada.Real_Time, Default_Detector, Default_Detector.Batches;
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

   Notify_After : constant := 1_000;
   Start        : Time;
   Count        : Natural := 0;

   procedure Update_Clock is
      use Ada.Text_IO;
      package Duration_IO is new Fixed_IO (Duration);
      use Duration_IO;
      Stop    : Time;
      Elapsed : Duration;
   begin
      Count := Count + 1;
      if Count > Notify_After then
         Count := 0;
         Stop := Clock;
         Elapsed := To_Duration (Stop - Start);
         Put (Duration (Notify_After) / Elapsed, 1);
         Put_Line (" epochs/second");
         Start := Clock;
      end if;
   end Update_Clock;

   procedure Notify_Seizure is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Seizure");
      Update_Clock;
   end Notify_Seizure;

   procedure Notify_Nothing is
   begin
      Update_Clock;
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
   Start := Clock;
   Run (Batch);
end Seizure_Detector_Fixed;
