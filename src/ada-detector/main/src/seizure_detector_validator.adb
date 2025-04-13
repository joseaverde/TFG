with Ada.Text_IO;
with Default_Detector;
with Detector.Signals;
with Detector.Containers.Vectors;

procedure Seizure_Detector_Validator with SPARK_Mode => On is

   subtype Raw_Sample is Default_Detector.Sample_Type;
   subtype Sample_Type is Detector.Signals.Sample_Type;
   package Sample_Vectors is
      new Detector.Containers.Vectors (
      Element_Type => Sample_Type,
      "="          => Detector.Signals."=");
   use Sample_Vectors;

   Signal : Vector := Create (1_000_000, 0.0);

begin

   declare
      Dummy : Nullable_Vector := Signal;
   begin
      Free (Dummy);
   end;

end Seizure_Detector_Validator;
