with Ada.Numerics.Elementary_Functions;

package body Detector.Details.Square_Root with SPARK_Mode => On is

   -->> Square Root <<--

   function Sqrt (
      Item : in Variance.Real)
      return Sqrt_Result is
      pragma SPARK_Mode (Off);
      -- TODO: REPLACE IT WITH A NORMAL ALGORITHM
      use Ada.Numerics.Elementary_Functions;
      Result : constant Float := Float (Item) ** 0.5;
   begin
      return Sqrt_Result (Result);
   end Sqrt;

end Detector.Details.Square_Root;
