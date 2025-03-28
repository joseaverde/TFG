-------------------------------------------------------------------------------
--                                                                           --
--                          D E T E C T O R . A D S                          --
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

package Detector with Pure, SPARK_Mode is

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
      range -2.0 ** 31 .. 2.0 ** 31 - 1.0 with
      Size => 32;

end Detector;
