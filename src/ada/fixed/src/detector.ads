--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector.ads                                                   |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package Detector with Pure, SPARK_Mode is

   -- This package is the parent package for the Seizure Detector. This
   -- implementation uses 32-bit fixed point numbers for computation so that it
   -- performs well on bare metal hardware without losing precision.
   --
   -- The detector is formed out of:
   --  * A Batch (which is

   Bits : constant := 32;

   type Count_Type is range 0 .. 2 ** (Bits - 1) - 1 with Size => Bits;
   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

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
   -- The function computes the floor of logarithm in base 2 of an integer.
   -- It expands the function to 0, so that `Log_2 (0) = 0'. The function
   -- `Log_2 (X) + 1' is equivalent to the number of bits needed to encode X.

   type Fixed_Integer is
      delta 1.0
      range -2.0 ** (Bits - 1) .. 2.0 ** (Bits - 1) - 1.0 with
   Size => Bits;
   -- This type is just an Integer in disguise. The idea behind this type is
   -- that when performing integer division or multiplication to a fixed point
   -- type, the result's type is the fixed point's type not the target type.
   -- For instance:
   --
   --    Fixed_A'(Fixed_B'(1.0) / 3);
   --
   -- Wouldn't work because the result of the division is of type `Fixed_B'.
   -- If we did
   --
   --    Fixed_A'(Fixed_B'(1.0) / Fixed_Integer (3));
   --
   -- It will compile and work as expected.

   type Fixed_Long_Integer is
      delta 1.0
      range -2.0 ** (Bits * 2 - 1) .. 2.0 ** (Bits * 2 - 1) - 1.0 with
   Size => Bits * 2;

end Detector;
