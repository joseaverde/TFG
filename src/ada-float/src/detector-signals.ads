--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals.ads                                           |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Numerics.Complex_Types;
package Detector.Signals with Pure, SPARK_Mode => On is

   Max_Signal_Length : constant := 32_000;
   subtype Index_Type is Positive_Count_Type range 1 .. Max_Signal_Length;

   subtype Complex is Numerics.Complex_Types.Complex;
   type Complex_Signal is array (Index_Type range <>) of Complex;
   type Signal_Type is array (Index_Type range <>) of Float_Type;

end Detector.Signals;
