package Detector.Signals.Batch_Normalisation with Pure, SPARK_Mode is

   -- NOTE: We divide by Sqrt (σ² - μ²) instead of (σ² + ε)

   -- https://en.wikipedia.org/wiki/Normalization_(statistics)
   -- https://en.wikipedia.org/wiki/Batch_normalization

   Normalised_Bits          : constant := Bits;
   Normalised_Fraction_Bits : constant := 13;
   Normalised_Whole_Bits    : constant :=
      Normalised_Bits - Normalised_Fraction_Bits - 1;
   Normalised_Delta         : constant := 2.0 ** (-Normalised_Fraction_Bits);

   type Base_Normalised_Sample is
      delta Normalised_Delta
      range -2.0 ** Normalised_Whole_Bits
         .. 2.0 ** Normalised_Whole_Bits - Normalised_Delta with
      Size => Bits;
   subtype Normalised_Sample is Base_Normalised_Sample range -16.0 .. 16.0;

   type Normalised_Signal is array (Index_Type range <>) of Normalised_Sample;

-- function Normalise (Item : in Signal_Type)
--    return Normalised_Signal with
--    Global => null;

end Detector.Signals.Batch_Normalisation;
