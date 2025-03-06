-------------------------------------------------------------------------------
--                                                                           --
--                              D E T E C T O R                              --
--                                                                           --
--                      S E I Z U R E   D E T E C T O R                      --
--                                                                           --
--                              A D A   S P E C                              --
--                                                                           --
-------------------------------------------------------------------------------
--  Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved  --
-------------------------------------------------------------------------------
--                                                                           --
-------------------------------------------------------------------------------

package Detector with SPARK_Mode => On is

   Bits : constant := 32;

   Stride_Size       : constant := 256;
   Strides_Per_Epoch : constant := 5;
   Epoch_Size        : constant := Stride_Size * Strides_Per_Epoch;
   Welch_Size        : constant := 512;
   Welch_Overlap     : constant := Welch_Size / 2;
   Warping_Window    : constant := 16;

   type Count_Type is range 0 .. 2 ** (Bits - 1) - 1 with Size => Bits;
   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;
   subtype Index_Type is Count_Type range 1 .. Count_Type'Last;
   subtype Multiplication_Safe_Count is
      Count_Type range 0 .. 2 ** (Bits / 2 - 1);
   subtype Positive_Multiplication_Safe_Count is
      Multiplication_Safe_Count range 1 .. Multiplication_Safe_Count'Last;

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

   Feature_Mantissa : constant := 8;
   Feature_Delta    : constant := 2.0 ** (-Feature_Mantissa);
   type Feature_Type is
      delta Feature_Delta
      range -2.0 ** (Bits - Feature_Mantissa - 1)
         .. 2.0 ** (Bits - Feature_Mantissa - 1) - Feature_Delta with
      Size => Bits;

   -->> Features <<--

   function Max_Distance (
      Item : in Sample_Epoch)
      return Feature_Type with
      Global   => null,
      Post     => (for some Min of Item =>
                     (for all X of Item => Min <= X) and then
                     (for some Max of Item =>
                        (for all X of Item => Max >= X) and then
                        Max_Distance'Result =
                           Feature_Type (Max) - Feature_Type (Min)))
         and then Max_Distance'Result in 0.0
                                      .. Feature_Type (Sample_Type'Last)
                                         - Feature_Type (Sample_Type'First);

   function Energy (
      Item : in Sample_Epoch)
      return Feature_Type with
      Global => null,
      Post   => Energy'Result >= 0.0;

   function Dynamic_Time_Warping (
      Signal  : in Sample_Epoch;
      Pattern : in Sample_Epoch;
      Maximum : in Feature_Type)
      return Feature_Type;

   -->> Welch <<--
   -- Overlap := Welch_Window_Size / 2
   -- Step := (Epoch_Size - Welch_Window_Size) / Overlap + 1
   -- Normalisation_Factor :=

   type Feature_Array is array (Positive_Count_Type range <>) of Feature_Type;
   procedure Power_Spectral_Density (
      Signal              : in     Sample_Epoch;
      Sampling_Frequency  : in     Feature_Type;
      PSD_1, PSD_2, PSD_3 :    out Feature_Type) with
      Pre => Sampling_Frequency = Feature_Type (Stride_Size),
      Always_Terminates;

   -->> Batchs <<--

   type Pattern_Index is range 1 .. 5;
   subtype Pattern_Type is Sample_Array (1 .. Epoch_Size);
   type Pattern_Array is array (Pattern_Index range <>) of Pattern_Type;
   type Real_Span is record Low, High : Feature_Type; end record;

   type Batch_Type (Count : Pattern_Index := 1) is record
      PSD_1, PSD_2, PSD_3, Energy, Max_Dist : Real_Span;
      d_max_c                               : Feature_Type;
      Patterns                              : Pattern_Array (1 .. Count);
   end record;

   function Is_Seizure (
      Item  : in Sample_Epoch;
      Batch : in Batch_Type)
      return Boolean;

end Detector;
