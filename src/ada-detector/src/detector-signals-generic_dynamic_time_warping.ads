with Detector.Signals.Batch_Normalisation;
generic
   type Result_Type is delta <>;
function Detector.Signals.Generic_Dynamic_Time_Warping (
   Left           : in Batch_Normalisation.Normalised_Signal;
   Right          : in Batch_Normalisation.Normalised_Signal;
   Warping_Window : in Positive_Count_Type)
   return Result_Type with
   Pre      => Left'Length = Right'Length
      and then Left'Length > 0
      and then Warping_Window < Left'Length,
   Global => null,
   Pure, SPARK_Mode;
