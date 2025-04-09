with Ada.Text_IO;
with Detector.Batches.Runner;
with Default_Detector;
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

   procedure Notify_Seizure is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Seizure");
   end Notify_Seizure;

   procedure Notify_Nothing is
   begin
      null;
      -- Ada.Text_IO.Put ('.');
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
   Run (Batch);
end Seizure_Detector_Fixed;
