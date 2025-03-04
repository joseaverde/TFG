package Detector.Details.Trigonometric with SPARK_Mode => On is

   π : constant := 3.14159_26535_89793_23846_26433_83279_50288_41971_69399;

   type Trigonometric_Input_Type is
      delta 2.0 ** (-Bits / 2)
      range -2.0 ** (Bits / 2 - 1)
         .. 2.0 ** (Bits / 2 - 1) - 2.0 ** (-Bits / 2) with
      Size =>Bits;

   type Trigonometric_Output_Type is
      delta 2.0 ** (-(Bits - 2))
      range -1.0 ..  1.0 with
      Size => Bits;

   function Cos (Item : in Trigonometric_Input_Type with Unreferenced)
      return Trigonometric_Output_Type;

   function Sin (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type;

end Detector.Details.Trigonometric;
