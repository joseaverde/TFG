--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_simpson.adb                           |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

function Detector.Signals.Generic_Simpson (
   Signal : in Signal_Type;
   dx     : in Sample_Type)
   return Result_Type with SPARK_Mode => Off is

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

   -- Variables

   Result : Internal_Type := 0.0;
   Extra  : Internal_Type := 0.0;
   Index  : Count_Type    := Signal'First;
begin

   while Index <= Signal'Last - 2 loop
      Index  := @ + 2;
      Result := @ + Internal_Type (Signal (Index - 2));
      Result := @ + Internal_Type (Signal (Index - 1)) * 4;
      Result := @ + Internal_Type (Signal (Index));
      -- Increments in 6 units each step
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
