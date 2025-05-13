package Detector.Details.Energy with SPARK_Mode => On is

   -->> Energy <<--
   -- The Energy is computed as:
   --    Σ (Signal (I) - μ)² / Epoch_Size
   -- We now that
   --    μ ∈ First .. Last
   --    Signal (I) ∈ First .. Last, ∀ I
   -- Therefore:
   --    Signal (I) - μ ∈ First - Last .. Last - First
   -- And then
   --    (Signal (I) - μ)² ∈ 0 .. (Last - First)²
   -- Therefore:
   --    Σ (Signal (I) - μ)² ∈ 0 .. Epoch_Size (Last - First)²
   -- And then:
   --    Σ (Signal (I) - μ)² / Epoch_Size ∈ 0 .. (Last - First)²
   -- That gives us the upper limit for the energy computation.
   --

   Real_Bits     : constant := 64;
   Real_Mantissa : constant := Sample_Mantissa * 2;
   Real_Delta    : constant := 2.0 ** (-Real_Mantissa);
   type Real is delta Real_Delta
      range -2.0 ** (Real_Bits - Real_Mantissa - 1)
         .. 2.0 ** (Real_Bits - Real_Mantissa - 1) - Real_Delta with
      Size => Real_Bits;

   S_First : constant Real := Real (Sample_Type'First);
   S_Last  : constant Real := Real (Sample_Type'Last);
   F_Last  : constant Real := Real (Feature_Type'Last);
   Max_Sq  : constant Real := (S_Last - S_First) * (S_Last - S_First);

   type Real_Epoch is array (Sample_Epoch'Range) of Real;

   function Acc_Energy_Sum (
      Item : in Real_Epoch)
      return Real_Epoch with
      Ghost    => True,
      Global   => null,
      Pre      => (for all X of Item => X in 0.0 .. Max_Sq),
      Post     => (Acc_Energy_Sum'Result (Item'First) = Item (Item'First))
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Energy_Sum'Result (I) =
                        Acc_Energy_Sum'Result (I - 1) + Item (I)
                     and then Acc_Energy_Sum'Result (I) >= 0.0);

   function Scale_Array (
      Item : in Sample_Epoch;
      μ    : in Real)
      return Real_Epoch with
      Ghost    => True,
      Global   => null,
      Pre      => μ in S_First .. S_Last,
      Post     => (for all I in Item'Range =>
                     Scale_Array'Result (I) =
                        (Real (Item (I)) - μ) * (Real (Item (I)) - μ)
                     and then Scale_Array'Result (I) in 0.0 .. Max_Sq);

end Detector.Details.Energy;
