--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals.ads                                           |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Numerics.Generic_Complex_Types;

package Detector.Signals with Pure, SPARK_Mode is

   -- A Signal is a sequence of samples. After some experimentation it is
   -- simpler for the theorem prover and for the implementation of numerous
   -- algorithms to use normalised values instead of scaled samples. Therefore
   -- a sample is a rational value (fixed point) in the set [0, 1).
   --
   -- The nested package `Generic_Normalisation' allows to normalise the input
   -- so that it doesn't lose preceission.

   Max_Signal_Length : constant := 32_000;
   subtype Index_Type is Positive_Count_Type range 1 .. Max_Signal_Length;

   Sample_Bits          : constant := Bits;
   Sample_Whole_Bits    : constant := 0;
   Sample_Fraction_Bits : constant := Bits - Sample_Whole_Bits - 1;
   Sample_Delta         : constant := 2.0 ** (-Sample_Fraction_Bits);
   type Sample_Type is
      delta Sample_Delta
      range -2.0 ** Sample_Whole_Bits + Sample_Delta
         .. 2.0 ** Sample_Whole_Bits - Sample_Delta with
      Size => Sample_Bits;

   package Complex_Types is
      new Detector.Numerics.Generic_Complex_Types (
      Fixed_Type => Sample_Type);
   subtype Complex is Complex_Types.Complex;
   type Complex_Signal is array (Index_Type range <>) of Complex;

   type Signal_Type is array (Index_Type range <>) of Sample_Type;

   -- A Big sample is the same as a sample but with double the bits of
   -- precision. It is returned by some functions such as the variance for
   -- more precision.

   Big_Sample_Bits          : constant := Sample_Bits * 2;
   Big_Sample_Whole_Bits    : constant := 0;
   Big_Sample_Fraction_Bits : constant := 2 * Bits - Big_Sample_Whole_Bits - 1;
   Big_Sample_Delta         : constant := 2.0 ** (-Big_Sample_Fraction_Bits);
   type Big_Sample_Type is
      delta Big_Sample_Delta
      range -2.0 ** Big_Sample_Whole_Bits + Big_Sample_Delta
         .. 2.0 ** Big_Sample_Whole_Bits - Big_Sample_Delta with
      Size => Big_Sample_Bits;

   type Big_Signal_Type is array (Index_Type range <>) of Big_Sample_Type;

   -- Normalisation

   generic
      type Scaled_Sample_Type is delta <>;
      Minimum_Value : in Scaled_Sample_Type := Scaled_Sample_Type'First;
      Maximum_Value : in Scaled_Sample_Type := Scaled_Sample_Type'Last;
   package Generic_Normalisation is

      subtype Valid_Scaled_Sample is Scaled_Sample_Type'Base
         range Minimum_Value .. Maximum_Value;

      Exponent : constant Natural :=
         Natural'Max (
            Natural (abs Minimum_Value + (0.5 - Scaled_Sample_Type'Delta)),
            Natural (abs Maximum_Value + (0.5 - Scaled_Sample_Type'Delta)));
      Factor : constant Positive := 2 ** (1 + Log_2 (Count_Type (Exponent)));

      function Normalise (Item : in Valid_Scaled_Sample)
         return Sample_Type is (
         Item / Fixed_Integer (Factor)) with
         Inline => True;

      function Denormalise (Item : in Sample_Type)
         return Scaled_Sample_Type'Base is (
         Fixed_Integer (Factor) * Item) with
         Inline => True;

   end Generic_Normalisation;

end Detector.Signals;
