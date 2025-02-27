package Detector with SPARK_Mode => On is

   Bits : constant := 32;

   Stride_Size : constant := 256;
   Epoch_Size  : constant := 1280;
   Welch_Size  : constant := 512;

   type Count_Type is range 0 .. 2 ** (Bits - 1) - 1 with Size => Bits;
   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;
   subtype Index_Type is Count_Type range 1 .. Count_Type'Last;

   function Log_2 (Item : in Count_Type) return Natural is (
      (case Item is
         when          0 ..          1 =>  0,
         when          2 ..          3 =>  1,
         when          4 ..          7 =>  2,
         when          8 ..         15 =>  3,
         when         16 ..         31 =>  4,
         when         32 ..         63 =>  5,
         when         64 ..        127 =>  6,
         when        128 ..        255 =>  7,
         when        256 ..        511 =>  8,
         when        512 ..       1023 =>  9,
         when       1024 ..       2047 => 10,
         when       2048 ..       4095 => 11,
         when       4096 ..       8191 => 12,
         when       8192 ..      16383 => 13,
         when      16384 ..      32767 => 14,
         when      32768 ..      65535 => 15,
         when      65536 ..     131071 => 16,
         when     131072 ..     262143 => 17,
         when     262144 ..     524287 => 18,
         when     524288 ..    1048575 => 19,
         when    1048576 ..    2097151 => 20,
         when    2097152 ..    4194303 => 21,
         when    4194304 ..    8388607 => 22,
         when    8388608 ..   16777215 => 23,
         when   16777216 ..   33554431 => 24,
         when   33554432 ..   67108863 => 25,
         when   67108864 ..  134217727 => 26,
         when  134217728 ..  268435455 => 27,
         when  268435456 ..  536870911 => 28,
         when  536870912 .. 1073741823 => 29,
         when 1073741824 .. 2147483647 => 30)) with
         Static => True;

   pragma Assert (2 ** Log_2 (Welch_Size) = Welch_Size);

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

   -->> Fourier Transform <<--

   subtype Complex_Part is Sample_Base_Type;
   type Complex is
      record
         Re : Complex_Part;
         Im : Complex_Part;
      end record;
   subtype Fourier_Transform_Real_Array is Sample_Array (1 .. Welch_Size);
   type Complex_Array is array (Index_Type range 1 .. Welch_Size) of Complex;

   procedure Fourier_Transform (
      Input  : in     Fourier_Transform_Real_Array;
      Output :    out Complex_Array);

   -->> Power Spectral Density <<--
   -- It is the
   --    Simpson (Welch (Item, Overlap, Frequency) (Low .. High));

   -->> Dynamic Time Warping <<--

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
