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
-- For storing the result with maximum accuracy.

with Detector.Signals.Mean;

function Detector.Signals.Generic_Energy (
   Item : in Signal_Type)
   return Result_Type is

   -- The type for intermediate operations

   Inter_Bits          : constant := Bits * 2;
   Inter_Whole_Bits    : constant := (Bits * 2 - 1 - Sample_Fraction_Bits) / 2;
   Inter_Fraction_Bits : constant := Inter_Bits - Inter_Whole_Bits - 1;
   Inter_Delta         : constant := 2.0 ** (-Inter_Fraction_Bits);
   type Inter_Type is
      delta Inter_Delta
      range -2.0 ** Inter_Whole_Bits
         .. 2.0 ** Inter_Whole_Bits - Inter_Delta with
      Size => Inter_Bits;
   subtype Uniform_Inter is Inter_Type
      range Inter_Type (Sample_Type'First) .. Inter_Type (Sample_Type'Last);
   subtype Positive_Uniform is Uniform_Inter range 0.0 .. Uniform_Inter'Last;

   function Squared (Item : in Uniform_Inter)
      return Uniform_Inter is (Item * Item) with
      Post => Squared'Result >= 0.0;

   type Positive_Uniform_Array is
      array (Index_Type range <>)
      of Positive_Uniform;
   type Inter_Array is array (Index_Type range <>) of Inter_Type;

   -- The reduction itself

   Max_Length : constant := 2 ** ((2 * Bits - 1 - Sample_Fraction_Bits) / 2);

   function Acc_Sum (
      Item : in Positive_Uniform_Array)
      return Inter_Array with
      Ghost    => True,
      Global   => null,
      Pre      => Item'Length > 0 and then Item'Length < Max_Length,
      Post     => Acc_Sum'Result'First = Item'First
         and then Acc_Sum'Result'Length = Item'Length
         and then Acc_Sum'Result (Item'First) = Inter_Type (Item (Item'First))
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Sum'Result (I - 1) in
                        0.0 .. Uniform_Inter'Last * Positive (I - Item'First)
                     and then Acc_Sum'Result (I) = Acc_Sum'Result (I - 1)
                                                 + Inter_Type (Item (I)))
         and then Acc_Sum'Result (Item'Last) in
            0.0 .. Uniform_Inter'Last * Item'Length;

   function Acc_Sum (
      Item : in Positive_Uniform_Array)
      return Inter_Array is
      Result : Inter_Array (Item'Range) := [others => 0.0];
   begin
      Result (Item'First) := Inter_Type (Item (Item'First));
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (
            Result (Item'First) = Inter_Type (Item (Item'First)));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I - 1) in
                  0.0 .. Uniform_Inter'Last * Positive (I - Item'First)
               and then Result (I) = Result (I - 1) + Inter_Type (Item (I))
               and then Result (I) in
                  0.0 .. Uniform_Inter'Last * Positive (I - Item'First + 1)));
         Result (Index) := Result (Index - 1) + Inter_Type (Item (Index));
      end loop;
      return Result;
   end Acc_Sum;

   function Scale (
      Item : in Signal_Type;
      μ    : in Uniform_Inter)
      return Positive_Uniform_Array with
      Ghost    => True,
      Global   => null,
      Post     => Scale'Result'First = Item'First
         and then Scale'Result'Length = Item'Length
         and then (for all Index in Item'Range =>
                     Scale'Result (Index)
                        = Squared ((Uniform_Inter (Item (Index)) - μ) / 2));

   function Scale (
      Item : in Signal_Type;
      μ    : in Uniform_Inter)
      return Positive_Uniform_Array is
      Result : Positive_Uniform_Array (Item'Range) := [others => 0.0];
   begin
      for Index in Item'Range loop
         Result (Index) := Squared ((Uniform_Inter (Item (Index)) - μ) / 2);
         pragma Loop_Invariant (
            (for all I in Item'First .. Index =>
               Result (I) = Squared ((Uniform_Inter (Item (I)) - μ) / 2)));
      end loop;
      return Result;
   end Scale;

   -- The program

   μ      : constant Uniform_Inter := Uniform_Inter (Mean (Item));
   Mapped : constant Positive_Uniform_Array := Scale (Item, μ) with Ghost;
   Result : Inter_Type;
begin

   Result := Squared ((Uniform_Inter (Item (Item'First)) - μ) / 2);
   for Index in Item'First + 1 .. Item'Last loop
      pragma Loop_Invariant (Result = Acc_Sum (Mapped) (Index - 1));
      pragma Loop_Invariant (
         Mapped (Index) = Squared ((Uniform_Inter (Item (Index)) - μ) / 2));
      Result := Result + Squared ((Uniform_Inter (Item (Index)) - μ) / 2);
   end loop;
   Result := Result / Item'Length;
   pragma Assert (Result in 0.0 .. Uniform_Inter'Last);
   return Result *
          Result_Type (4 * Normalisation.Factor * Normalisation.Factor);
end Detector.Signals.Generic_Energy;
