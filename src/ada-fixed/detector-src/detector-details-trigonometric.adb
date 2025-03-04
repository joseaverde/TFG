package body Detector.Details.Trigonometric with SPARK_Mode => On is

   function Cos (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type is (
      1.0);

   function Sin (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type is (
      Trigonometric_Output_Type (
         Trigonometric_Input_Type'Min (1.0,
         Trigonometric_Input_Type'Max (Item, -1.0))));

end Detector.Details.Trigonometric;
