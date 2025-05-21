--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_simpson.adb                           |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals.Lemmas;

function Detector.Signals.Generic_Simpson (
   Signal : in Signal_Type;
   dx     : in Sample_Type)
   return Result_Type with SPARK_Mode => On is
   -- FIXME: Properly prove it

   -- Internal type for the accumulator

   Internal_Bits          : constant := Bits * 2;
   Internal_Fraction_Bits : constant := Sample_Bits;
   Internal_Whole_Bits    : constant :=
      Internal_Bits - Internal_Fraction_Bits - 1;
   Internal_Delta         : constant := 2.0 ** (-Internal_Fraction_Bits);
   type Internal_Type is
      delta Internal_Delta
      range -2.0 ** Internal_Whole_Bits
         .. 2.0 ** Internal_Whole_Bits - Internal_Delta with
      Size => Internal_Bits;

   -- Accumulator function

   type Internal_Array is array (Index_Type range <>) of Internal_Type;
   subtype Internal_Step is Internal_Type
      range 6 * Internal_Type (Sample_Type'First)
         .. 6 * Internal_Type (Sample_Type'Last);

   function Acc_Sum is
      new Detector.Signals.Lemmas.Generic_Accumulation (
      Fixed_Type => Internal_Type,
      Index_Type => Index_Type,
      Array_Type => Internal_Array,
      First      => Internal_Step'First,
      Last       => Internal_Step'Last);

-- function Compute_At (Index : in Index_Type) return Internal_Step with
--    Inline => True,
--    Pre    => Index * 2 in 2 .. Signal'Length - 1;
-- function Compute_At (Index : in Index_Type) return Internal_Step is
-- begin
--    return Internal_Type (Signal (Signal'First + 2 * Index - 2))
--         + Internal_Type (Signal (Signal'First + 2 * Index - 1)) * 4
--         + Internal_Type (Signal (Signal'First + 2 * Index));
-- end Compute_At;

   function Compute_At (
      Signal : in Signal_Type;
      Index  : in Index_Type)
      return Internal_Step with
      Pre      => Signal'Length >= 3
         and then Index in Signal'First + 1 .. Signal'Last - 3;

   function Compute_At (
      Signal : in Signal_Type;
      Index  : in Index_Type)
      return Internal_Step is (
      4 * Internal_Type (Signal (Index))
      + 2 * Internal_Type (Signal (Index + 1)));

   function Create_Map (
      Signal : in Signal_Type)
      return Internal_Array with
      Ghost    => True,
      Pre      => Signal'Length >= 3,
      Post     => Create_Map'Result'First = 1
         and then Create_Map'Result'Length = (Signal'Length - 2) / 2
         and then (for all I in Create_Map'Result'Range =>
                     Create_Map'Result (I) =
                        Compute_At (Signal, Signal'First + (I - 1) * 2));

   function Create_Map (
      Signal : in Signal_Type)
      return Internal_Array is
      Length : constant Count_Type := (Signal'Length - 2) / 2;
      Result : Internal_Array (1 .. Length);
   begin
      for I in 1 .. Length loop
         pragma Loop_Invariant (
                     I - 1 in 0 .. Length - 1
            and then (I - 1) * 2 in 0 .. (Length - 1) * 2
            and then (Length - 1) * 2 in Signal'Length - 4 .. Signal'Length - 3
            and then (Length - 1) * 2 + 1 in Signal'Length - 3
                                          .. Signal'Length - 2
            and then (I - 1) * 2 + 1 in 1 .. Signal'Length - 2
            and then Signal'First + (I - 1) * 2 + 1
                        in Signal'First + 1
                        .. Signal'First + Signal'Length - 2
            and then Signal'Last = Signal'First + Signal'Length - 1);
         Result (I) := Compute_At (Signal, Signal'First + (I - 1) * 2 + 1);
      end loop;
      return Result;
   end Create_Map;

   -- Variables

   Ghost_Length : constant Count_Type := (Signal'Length - 2) / 2 with Ghost;
   Ghost_Mapped : constant Internal_Array := Create_Map (Signal) with Ghost;
   Ghost_Index : Index_Type := 1 with Ghost;

   Result : Internal_Type;
   Extra  : Internal_Type := 0.0;
   Index  : Index_Type;
begin

   Index := Signal'First + 1;
   Result := Internal_Type (Signal (Signal'First));
   while Index <= Signal'Last - 3 loop
      pragma Loop_Variant (Increases => Index);
      pragma Loop_Variant (Increases => Ghost_Index);
      pragma Loop_Invariant (Ghost_Index = (Index - Signal'First) / 2 + 1);
      pragma Loop_Invariant (Ghost_Index in 1 .. Ghost_Length);
      pragma Loop_Invariant (Index in Signal'First + 1 .. Signal'Last - 3);
      Result := @ + Compute_At (Signal, Index);
      Index := Index + 2;
      Ghost_Index := Ghost_Index + 1;
   end loop;
   pragma Assert (Index in Signal'Last - 2 .. Signal'Last - 1);

   -- pragma Assert (Ghost_Index = (Index - Signal'First) / 2 + 1);
   -- pragma Assert (Ghost_Index = Ghost_Length);
   Result := @ + 4 * Internal_Type (Signal (Index));
   Result := @ + Internal_Type (Signal (Index + 1));
   Result := Result / 3;

   if Signal'Length mod 2 = 0 then
      Extra := 5 * Internal_Type (Signal (Signal'Last));
      pragma Assert (Extra in 5 * Internal_Type (Sample_Type'First)
                           .. 5 * Internal_Type (Sample_Type'Last));
      Extra := @ + 8 * Internal_Type (Signal (Signal'Last - 1));
      pragma Assert (Extra in 13 * Internal_Type (Sample_Type'First)
                           .. 13 * Internal_Type (Sample_Type'Last));
      Extra := @ - Internal_Type (Signal (Signal'Last - 2));
      pragma Assert (Extra in 13 * Internal_Type (Sample_Type'First)
                              - Internal_Type (Sample_Type'Last)
                           .. 13 * Internal_Type (Sample_Type'Last)
                              - Internal_Type (Sample_Type'First));
      Extra := @ / 12;
   end if;

   Result := Result + Extra;
   return Result * dx;

-- pragma Assert (Mapped'First = 1);
-- pragma Assert (
--    (for all I in Mapped'Range =>
--       Mapped (I) in Internal_Step));

-- Index := @ + 2;
-- Result := Internal_Type (Signal (Index - 2))
--         + Internal_Type (Signal (Index - 1)) * 4
--         + Internal_Type (Signal (Index));
-- pragma Assert (Result = Mapped (1));
-- pragma Assert (Acc_Sum (Mapped) (1) = Mapped (1));
-- pragma Assert (Result = Acc_Sum (Mapped) (1));
-- for Iter in 2 .. Length loop
--    Index  := @ + 2;
--    Value := Internal_Type (Signal (Index - 2))
--           + Internal_Type (Signal (Index - 1)) * 4
--           + Internal_Type (Signal (Index));
--    Result := @ + Value;

--    pragma Loop_Invariant (Index - 2 >= Signal'First);
--    pragma Loop_Invariant (Index = Iter * 2 + Signal'First);
--    pragma Loop_Invariant (Mapped (Iter) = Value);
--    pragma Loop_Invariant (
--       Result = Acc_Sum (Mapped) (Iter)
--       and then Result in Internal_Step'First * Positive (Iter)
--                       .. Internal_Step'Last * Positive (Iter));
--    pragma Loop_Variant (Increases => Iter);
-- end loop;
-- -- Result in 6 * First * Length .. 6 * Last * Length
-- Result := @ / 3;
-- -- Result in 2 * First * Length .. 2 * Last * Length

-- if Signal'Length > 2 and then Signal'Length mod 2 = 0 then
--    Extra := (5 * Internal_Type (Signal (Signal'Last))
--            + 8 * Internal_Type (Signal (Signal'Last - 1))
--            -     Internal_Type (Signal (Signal'Last - 2))) / 12;
--    -- Extra in 14 * First / 12 .. 14 * Last / 12;
-- end if;
-- Result := Result + Extra;
-- -- Result in 2 * First * (Length + 2) .. 2 * Last * (Length + 2)
-- return Result * dx;

end Detector.Signals.Generic_Simpson;
