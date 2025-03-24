package body Detector.Details.Complex with SPARK_Mode => On is

   function Product (Left, Right : in Normal_Complex) return Normal_Complex is
      Result : Complex;
      Re, Im : Base_Real;
   begin
      -- Assert preconditions, we start from here.
      pragma Assert (Left.Re * Left.Re <= One - Left.Im * Left.Im);
      pragma Assert (Right.Re * Right.Re <= One - Right.Im * Right.Im);
      -- We want to prove that:
      -- |a*b| = |a|*|b| <= 1
      Re := Left.Re * Right.Re - Left.Im * Right.Im;
      Im := Left.Re * Right.Im + Left.Im * Right.Re;
      pragma Assert (Magnitude_Squared (Left) in 0.0 .. 1.0);
      pragma Assert (Magnitude_Squared (Right) in 0.0 .. 1.0);

      pragma Assume (Re in Real);
      pragma Assume (Im in Real);
      Result := (Re, Im);
      pragma Assume (Magnitude_Squared (Result) in 0.0 .. 1.0);
      pragma Assert (
         (if Magnitude_Squared (Result) in 0.0 .. 1.0
            then Result in Normal_Complex));
      return Result;
   end Product;

   function Magnitude_Squared (Item : in Complex) return Base_Real is
   begin
      return Item.Re * Item.Re + Item.Im * Item.Im;
   end Magnitude_Squared;

end Detector.Details.Complex;
