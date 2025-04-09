generic
   type Fixed_Type is delta <>;
package Detector.Numerics.Generic_Complex_Types with Pure, SPARK_Mode is

   type Complex is record
      Re : Fixed_Type;
      Im : Fixed_Type;
   end record;

end Detector.Numerics.Generic_Complex_Types;
