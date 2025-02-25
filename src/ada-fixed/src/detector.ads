package Detector with SPARK_Mode => On is

   Bits : constant := 32;

   Stride_Size : constant := 256;
   Epoch_Size  : constant := 1280;
   Welch_Size  : constant := 512;

   type Count_Type is range 0 .. 1_000_000 with Size => Bits;
   subtype Index_Type is Count_Type range 1 .. Count_Type'Last;

   -- Samples --

   Sample_Mantissa : constant := 4;
   Sample_Delta    : constant := 2.0 ** (-Sample_Mantissa);

   type Sample_Base_Type is
      delta Sample_Delta
      range -2.0 ** (Bits - Sample_Mantissa - 1)
         .. 2.0 ** (Bits - Sample_Mantissa - 1) - Sample_Delta with
      Size => Bits;

   subtype Sample_Type is Sample_Base_Type range -10_000.0 .. 10_000.0;

   type Sample_Base_Array is array (Index_Type range <>) of Sample_Base_Type;
   subtype Sample_Base_Epoch is Sample_Base_Array (1 .. Epoch_Size);
   type Sample_Array is array (Index_Type range <>) of Sample_Type;
   subtype Sample_Epoch is Sample_Array (1 .. Epoch_Size);

   -- Features --

   type Feature_Type is delta 2.0 ** (-10) range -20_000.0 .. 20_000.0 with
      Size => Bits;

   -->> Max distance <<--
   -- The Max_Distance is a function that returns the difference between the
   -- maximum value on an Epoch and the minimum value of an Epoch. Then if
   -- the domain is:
   --
   --    Domain is range First .. Last
   --
   -- We now that:
   --
   --    Maximum (Epoch) >= Minimum (Epoch)
   --
   -- Then if:
   --
   --    Maximum (Epoch) = Minimum (Epoch) => Result := 0.0
   --
   -- If Maximum (Epoch) = Last, and Minimum (Epoch) = First then:
   --
   --    Result := Last - First
   --
   -- Which maximises the function. Then we now that the codomain will be:
   --
   --    Codomain is range 0 .. Last - First

   function Acc_Maximum (
      Item : in Sample_Epoch)
      return Sample_Epoch with
      Ghost    => True,
      Global   => null,
      Post     => (Item (Item'First) = Acc_Maximum'Result (Item'First))
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Maximum'Result (I) =
                        Sample_Type'Max (Item (I),
                                         Acc_Maximum'Result (I - 1)));

   function Acc_Minimum (
      Item : in Sample_Epoch)
      return Sample_Epoch with
      Ghost    => True,
      Global   => null,
      Post     => (Item (Item'First) = Acc_Minimum'Result (Item'First))
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Minimum'Result (I) =
                        Sample_Type'Min (Item (I),
                                         Acc_Minimum'Result (I - 1)));

   function Maximum (
      Item : in Sample_Epoch)
      return Sample_Type with
      Ghost    => True,
      Post     => (for all Sample of Item => Maximum'Result >= Sample)
         and then (for some Sample of Item => Sample = Maximum'Result)
         and then (Maximum'Result = Acc_Maximum (Item) (Item'Last)),
      Global => null;

   function Minimum (
      Item : in Sample_Epoch)
      return Sample_Type with
      Ghost    => True,
      Post     => (for all Sample of Item => Minimum'Result <= Sample)
         and then (for some Sample of Item => Sample = Minimum'Result)
         and then (Minimum'Result = Acc_Minimum (Item) (Item'Last)),
      Global => null;

   function Max_Distance (
      Item : in Sample_Epoch)
      return Feature_Type with
      Global   => null,
      Post     => Max_Distance'Result =
                     Feature_Type (Maximum (Item))
                     - Feature_Type (Minimum (Item))
         and then Max_Distance'Result in 0.0
                                      .. Feature_Type (Sample_Type'Last)
                                         - Feature_Type (Sample_Type'First);

   -->> Mean <<--

   function Acc_Sum (
      Item : in Sample_Epoch)
      return Sample_Base_Epoch with
      Ghost    => True,
      Global   => null,
      Post     => Acc_Sum'Result (Item'First) = Item (Item'First)
         and then (for all I in Item'Range =>
                     Acc_Sum'Result (I)
                        in Sample_Base_Type (I) * Sample_Type'First
                        .. Sample_Base_Type (I) * Sample_Type'Last)
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Sum'Result (I) = Acc_Sum'Result (I - 1) + Item (I));

   function Sum (
      Item : in Sample_Epoch)
      return Sample_Base_Type with
      Global   => null,
      Post     => Sum'Result = Acc_Sum (Item) (Item'Last)
         and then Sum'Result
                     in Sample_Base_Type (Epoch_Size) * Sample_Type'First
                     .. Sample_Base_Type (Epoch_Size) * Sample_Type'Last;

   function Mean (
      Item : in Sample_Epoch)
      return Sample_Type with
      Global => null,
      Post   => Mean'Result = Sum (Item) / Sample_Base_Type (Epoch_Size);

   -- procedure Fast_Fourier_Transform (

   -->> Energy <<--
   -- The Energy is computed as:
   --    Σ (Signal (I) - μ)² / Epoch_Size
   -- We now that
   --    μ ∈ First .. Last
   --    Signal (I) ∈ First .. Last, ∀ I
   -- Therefore:
   --    Signal (I) - μ ∈ Last - First .. First - Last
   -- And then
   --    (Signal (I) - μ)² ∈ 0 .. (Last - First)²
   -- Therefore:
   --    Σ (Signal (I) - μ)² ∈ 0 .. Epoch_Size (Last - First)²
   -- And then:
   --    Σ (Signal (I) - μ)² / Epoch_Size ∈ 0 .. (Last - First)²
   -- That gives us the upper limit for the energy computation.

-- function Energy (
--    Item : in Sample_Epoch)
--    return Feature_Type with
--    Global => null,
--    Post   => Energy'Result in 0.0
--                            .. (Feature_Type (Sample_Type'Last)
--                               - Feature_Type (Sample_Type'First))
--                               * (Feature_Type (Sample_Type'Last)
--                                 - Feature_Type (Sample_Type'First));

   -->> Welch <<--
   -- Overlap := Welch_Window_Size / 2
   -- Step := (Epoch_Size - Welch_Window_Size) / Overlap + 1
   -- Normalisation_Factor :=

   -->> Power Spectral Density <<--
   -- It is the
   --    Simpson (Welch (Item, Overlap, Frequency) (Low .. High));

   -->> Trigonometric Functions <<--

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

end Detector;
