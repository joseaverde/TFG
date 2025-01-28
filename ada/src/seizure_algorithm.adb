with Seizure;
with Seizure.Detector;

procedure Seizure_Algorithm with SPARK_Mode => On is
   subtype Real is Float;
   package Real_Seizure is new Seizure (Real);
   package Detector is new Real_Seizure.Detector;
begin
   null;
end Seizure_Algorithm;
