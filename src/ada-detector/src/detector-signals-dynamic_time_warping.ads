function Detector.Signals.Dynamic_Time_Warping (
   Left  : in Variance_Scaled_Signal;
   Right : in Variance_Scaled_Signal) with
   Pre    => Left'Length = Right'Length,
   Global => null,
   Pure, SPARK_Mode;
