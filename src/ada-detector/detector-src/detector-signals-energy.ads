function Detector.Signals.Energy (
   Item : in Signal_Type)
   return Sample_Type with
   Global   => null,
   Pre      => Item'Length > 0
      and then Item'Length < 2 ** ((2 * Bits - 1 - Sample_Fraction_Bits) / 2),
   Pure, SPARK_Mode;
-- The energy is computed as:
--
--      (S (I) - μ)²
--    Σ ------------
--         Length
--
-- As both the signal and the mean are scaled by a factor of Q. We the have to
-- multiply the result by the scaling factor squared:
--
--         (S' (I) - μ')²
--    Q² Σ --------------     ; S' (I) := S (I) / Q
--             Length         ; μ'     := μ / Q
--
-- The thing is that:
--
--    S' (I) ∈ (-1, 1)
--    μ      ∈ (-1, 1)
--    -> S (I) - μ ∈ (-2, 2)
--
-- And the values might overflow, that's why we do another implicit scale
-- inside that divides everything by two. The result must be multiplied by 4.
--
--           (S" (I) - μ")²
--    4 Q² Σ --------------   ; S" (I) := S' (I) / 2 = S (I) / 2 Q
--               Length       ; μ'     := μ' / Q     = μ / 2 Q
--
-- That way there is no room for an overflow and we can work using our 32 bit
-- type.
--
-- The next problem is accuracy. Inside, as we are doing a reduction, we are
-- going to be using a 64 bit fixed point type as the accumulator. The number
-- of bits we need in order to avoid precision loss is:
--
--    Log_2 (Item'Length) + Sample_Fraction_Bits
--
-- But we also need to make sure that on the left side we have room for all the
-- elements. Which is also
--
--    Log_2 (Item'Length) + Sample_Fraction_Bits
--
-- Accounting for the sign bit. We would need:
--
--    1 + 2 * Log_2 (Item'Length) + Sample_Fraction_Bits = 2 * Bits
--    2 * Log_2 (Item'Length) = 2 * Bits - 1 - Sample_Fraction_Bits
--
--    -> Item'Length = 2 ** ((2 * Bits - 1 - Sample_Fraction_Bits) / 2)
--
-- For storing the result with maximum accuracy. That number is known and is
-- `Max_Signal_Length` :)
