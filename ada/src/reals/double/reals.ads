with Ada.Numerics.Generic_Elementary_Functions;
package Reals with Pure, SPARK_Mode => On is

   subtype Real is Long_Float;
   package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Real);

end Reals;
