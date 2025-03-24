package Detector.Details.Complex with SPARK_Mode => On is

   Real_Mantissa : constant := Bits - 2;
   Real_Delta    : constant := 2.0 ** (-Real_Mantissa);
   type Base_Real is delta Real_Delta range -2.0 .. 2.0 - Real_Delta with
      Size => Bits;
   subtype Real is Base_Real range -1.0 .. 1.0;
   One : constant Real := 1.0;

   type Complex is record
      Re : Real;
      Im : Real;
   end record;

   function Is_Magnitude_Normal (Item : in Complex) return Boolean is (
      Item.Re * Item.Re <= One - Item.Im * Item.Im);

   subtype Normal_Complex is Complex with
      Dynamic_Predicate => Is_Magnitude_Normal (Normal_Complex);

   function Magnitude_Squared (Item : in Complex) return Base_Real with
      Pre      => Item /= (One, One) and then Item /= (-One, -One)
         and then Item /= (One, -One) and then Item /= (-One, One),
      Post     => (if Item in Normal_Complex
                     then Magnitude_Squared'Result in 0.0 .. 1.0
                     else Magnitude_Squared'Result >= 0.0)
         and then (if Magnitude_Squared'Result in 0.0 .. 1.0
                     then Item in Normal_Complex)
         and then (Magnitude_Squared'Result
                     = Item.Re * Item.Re + Item.Im * Item.Im);

   function Product (Left, Right : in Normal_Complex) return Normal_Complex;

end Detector.Details.Complex;
