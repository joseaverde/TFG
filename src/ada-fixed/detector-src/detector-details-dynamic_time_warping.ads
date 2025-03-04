package Detector.Details.Dynamic_Time_Warping with SPARK_Mode => On is

   -->> Sum_Squares <<--

   Real_Bits     : constant := 64;
   Real_Mantissa : constant := Sample_Mantissa * 2;
   Real_Delta    : constant := 2.0 ** (-Real_Mantissa);
   type Real is delta Real_Delta
      range -2.0 ** (Real_Bits - Real_Mantissa - 1)
         .. 2.0 ** (Real_Bits - Real_Mantissa - 1) - Real_Delta with
      Size => Real_Bits;
   type Real_Epoch is array (Sample_Epoch'Range) of Real;

   Max_Abs : constant Real :=
      Real'Max (abs Real (Sample_Type'First), abs Real (Sample_Type'Last));
   Max_Sq  : constant Real := Max_Abs * Max_Abs;

   function Acc_Sum_Squares (
      Item : in Real_Epoch)
      return Real_Epoch with
      Ghost    => True,
      Global   => null,
      Pre      => (for all X of Item => X in 0.0 .. Max_Sq),
      Post     => (Acc_Sum_Squares'Result (Item'First) = Item (Item'First))
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Sum_Squares'Result (I) =
                        Acc_Sum_Squares'Result (I - 1) + Item (I)
                     and then Acc_Sum_Squares'Result (I) >= 0.0)
         and then (for all I in Item'Range =>
                     Acc_Sum_Squares'Result (I) in 0.0 .. Real (I) * Max_Sq);

   function Map_Square (
      Item : in Sample_Epoch)
      return Real_Epoch with
      Ghost    => True,
      Global   => null,
      Post     => (for all I in Item'Range =>
                     Map_Square'Result (I) = Real (Item (I)) * Real (Item (I))
                     and then Map_Square'Result (I) in 0.0 .. Max_Sq);

   function Sum_Squares (
      Item : in Sample_Epoch)
      return Real with
      Global => null,
      Post   => Sum_Squares'Result <= Epoch_Size * Max_Sq;

   -->> Square Root <<--

   function Sqrt (
      Item : in Real)
      return Real with
      Global => null,
      Pre      => Item in 0.0 .. Max_Sq,
      Post     => Sqrt'Result * Sqrt'Result <= Item
         and then Sqrt'Result in 0.0 .. Max_Abs;

   -->> Normalise <<--

   type Normalised_Array is array (Sample_Epoch'Range) of Sample_Type;

   function Normalise(
      Item : in Sample_Epoch)
      return Normalised_Array;

end Detector.Details.Dynamic_Time_Warping;
