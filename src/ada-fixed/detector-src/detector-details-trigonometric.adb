with Ada.Numerics.Elementary_Functions;

package body Detector.Details.Trigonometric with SPARK_Mode => Off is

   package EL renames Ada.Numerics.Elementary_Functions;

   -- TODO: IMPLEMENT USING ANOTHER ALGORITHM

   function Cos (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type is
      Result : constant Float := EL.Cos (Float (Item));
   begin
      return (if Result > 1.0     then 1.0
              elsif Result < -1.0 then -1.0
              else Trigonometric_Output_Type (Result));
   end Cos;

   function Sin (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type is
      Result : constant Float := EL.Sin (Float (Item));
   begin
      return (if Result > 1.0     then 1.0
              elsif Result < -1.0 then -1.0
              else Trigonometric_Output_Type (Result));
   end Sin;

end Detector.Details.Trigonometric;
