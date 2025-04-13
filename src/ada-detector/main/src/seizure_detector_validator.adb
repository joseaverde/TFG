with Default_Detector;
with Detector.Batches.Validator;
with Detector.Containers.Vectors;

procedure Seizure_Detector_Validator with SPARK_Mode => On is

   subtype Raw_Sample is Default_Detector.Sample_Type;
   use all type Raw_Sample;
   package Vectors is new Detector.Containers.Vectors (Raw_Sample);

   package Validator is new Default_Detector.Batches.Validator (Cores => 4);
begin
   null;
end Seizure_Detector_Validator;
