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
   return Result_Type with SPARK_Mode => Off is
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

   -- Variables

   Length : constant Positive_Count_Type := (Signal'Length - 3) / 2 + 1;
   Mapped : constant Internal_Array (1 .. Length) :=
      [for I in 1 .. Length =>
           Internal_Type (Signal (Signal'First + 0 + (I - 1) * 2))
         + Internal_Type (Signal (Signal'First + 1 + (I - 1) * 2)) * 4
         + Internal_Type (Signal (Signal'First + 2 + (I - 1) * 2))] with
      Ghost => True;

   Result : Internal_Type;
   Extra  : Internal_Type := 0.0;
   Index  : Count_Type    := Signal'First;
   Value  : Internal_Step;
begin

   pragma Assert (Mapped'First = 1);
   pragma Assert (
      (for all I in Mapped'Range =>
         Mapped (I) in Internal_Step));

   Index := @ + 2;
   Result := Internal_Type (Signal (Index - 2))
           + Internal_Type (Signal (Index - 1)) * 4
           + Internal_Type (Signal (Index));
   pragma Assert (Result = Mapped (1));
   pragma Assert (Acc_Sum (Mapped) (1) = Mapped (1));
   pragma Assert (Result = Acc_Sum (Mapped) (1));
   for Iter in 2 .. Length loop
      Index  := @ + 2;
      Value := Internal_Type (Signal (Index - 2))
             + Internal_Type (Signal (Index - 1)) * 4
             + Internal_Type (Signal (Index));
      Result := @ + Value;

      pragma Loop_Invariant (Index - 2 >= Signal'First);
      pragma Loop_Invariant (Index = Iter * 2 + Signal'First);
      pragma Loop_Invariant (Mapped (Iter) = Value);
      pragma Loop_Invariant (
         Result = Acc_Sum (Mapped) (Iter)
         and then Result in Internal_Step'First * Positive (Iter)
                         .. Internal_Step'Last * Positive (Iter));
      pragma Loop_Variant (Increases => Iter);
   end loop;
   -- Result in 6 * First * Length .. 6 * Last * Length
   Result := @ / 3;
   -- Result in 2 * First * Length .. 2 * Last * Length

   if Signal'Length > 2 and then Signal'Length mod 2 = 0 then
      Extra := (5 * Internal_Type (Signal (Signal'Last))
              + 8 * Internal_Type (Signal (Signal'Last - 1))
              -     Internal_Type (Signal (Signal'Last - 2))) / 12;
      -- Extra in 14 * First / 12 .. 14 * Last / 12;
   end if;
   Result := Result + Extra;
   -- Result in 2 * First * (Length + 2) .. 2 * Last * (Length + 2)
   return Result * dx;

end Detector.Signals.Generic_Simpson;
