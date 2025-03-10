package Detector.Details.Dynamic_Time_Warping with SPARK_Mode => On is

   function Single_Dynamic_Time_Warping (
      Signal  : in Normalised_Epoch;
      Pattern : in Normalised_Epoch;
      Max     : in Feature_Type)
      return Feature_Type;

   function Saturated_Addition (
      Left  : in Base_Normalised_Sample;
      Right : in Base_Normalised_Sample)
      return Base_Normalised_Sample with
      Global   => null,
      Pre      => Left >= 0.0 and then Right >= 0.0,
      Post     => Saturated_Addition'Result >= Left
         and then Saturated_Addition'Result >= Right;

   function Saturated_Square (
      Item : in Normalised_Sample)
      return Base_Normalised_Sample with
      Global   => null,
      Post     => Saturated_Square'Result >= 0.0
         and then Saturated_Square'Result = Item * Item;

   function Saturated_Distance (
      Left  : in Normalised_Sample;
      Right : in Normalised_Sample)
      return Base_Normalised_Sample with
      Global => null,
      Post   => Saturated_Distance'Result >= 0.0;

end Detector.Details.Dynamic_Time_Warping;
