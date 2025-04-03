function Detector.Signals.Quarter_Variance (
   Item : in Signal_Type)
   return Big_Sample_Type with
   Global => null,
   Pre  => Item'Length > 0,
   Post => Detector.Signals.Quarter_Variance'Result >= 0.0,
   Pure, SPARK_Mode;
