generic
   type Result_Type is delta <>;
   with package Normalisation is new Generic_Normalisation (<>);
function Detector.Signals.Generic_Energy (
   Item : in Signal_Type)
   return Result_Type with
   Global   => null,
   Pre      => Result_Type'First <= 0.0
      and then Result_Type'Last >= Result_Type (
                  4 * Normalisation.Factor * Normalisation.Factor)
      and then Item'Length > 0
      and then Item'Length < 2 ** ((2 * Bits - 1 - Sample_Fraction_Bits) / 2),
   Pure, SPARK_Mode;
-- The energy is computed as:
--
--      (S (I) - μ)²
--    Σ ------------
--         Length
--
-- The result must fit in the set:
--
--    [0.0, 4 * Q²]
--
-- Where `Q' is the normalisation factor.
