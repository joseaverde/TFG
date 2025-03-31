function Detector.Signals.Mean (
   Item : in Signal_Type)
   return Sample_Type with
   Global => null,
   Pre    => Item'Length > 0,
   Pure, SPARK_Mode;
