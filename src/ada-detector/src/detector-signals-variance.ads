function Detector.Signals.Variance (
   Item : in Signal_Type)
   return Big_Sample_Type with
   Global => null,
   Pre => Item'Length > 0,
   Pure, SPARK_Mode;
