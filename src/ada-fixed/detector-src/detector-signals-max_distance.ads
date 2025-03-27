package Detector.Signals.Max_Distance with Pure, SPARK_Mode is

   -- Max distance is a function that returns the difference between the
   -- maximum and the minimum value of a signal.

   generic
      type Result_Type is delta <>;
      with package Normalisation is new Generic_Normalisation (<>);
   function Generic_Max_Distance (
      Item : in Signal_Type)
      return Result_Type with
      Global => null,
      Pre    => Item'Length > 0;

end Detector.Signals.Max_Distance;
