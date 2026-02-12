--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_simpson.adb                           |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals.Generic_Simpson_Details;
with Detector.Numerics.Saturating_Arithmetic;

function Detector.Signals.Generic_Simpson (
   Signal : in Signal_Type;
   dx     : in Sample_Type)
   return Result_Type with SPARK_Mode is

   package Details is new Generic_Simpson_Details (Result_Type);
   use Details;

   function Multiply is
      new Numerics.Saturating_Arithmetic.Generic_Saturated_Multiplication (
      Left_Fixed_Type   => Internal_Type,
      Right_Fixed_Type  => Sample_Type,
      Result_Fixed_Type => Result_Type);

   Length : constant Count_Type    := Middle_Length (Signal);
   Evens  : constant Internal_Type := Even_Result (Signal);
   Odds   : constant Internal_Type := Odd_Result (Signal);
   Result : Internal_Type;
   Factor : Natural;

begin

   pragma Assert (Evens in Natural (Length) * Internal_Step'First
                        .. Natural (Length) * Internal_Step'Last);
   pragma Assert (Odds in Natural (Length) * Internal_Step'First
                       .. Natural (Length) * Internal_Step'Last);
   pragma Assert (2 * Evens in 2 * Natural (Length) * Internal_Step'First
                            .. 2 * Natural (Length) * Internal_Step'Last);
   pragma Assert (4 * Odds in 4 * Natural (Length) * Internal_Step'First
                           .. 4 * Natural (Length) * Internal_Step'Last);
   Result := 2 * Evens + 4 * Odds;
   pragma Assert (Result in 6 * Natural (Length) * Internal_Step'First
                         .. 6 * Natural (Length) * Internal_Step'Last);
   Result := @ + First_And_Last_Iterations (Signal);
   pragma Assert (
      Result in (6 * Natural (Length) + 6) * Internal_Step'First
             .. (6 * Natural (Length) + 6) * Internal_Step'Last);
   pragma Assert (6 * Natural (Length) + 6 = 6 * (Natural (Length + 1)));
   pragma Assert (
      Result in (6 * (Natural (Length) + 1)) * Internal_Step'First
             .. (6 * (Natural (Length) + 1)) * Internal_Step'Last);
   pragma Assert (
      (6 * (Natural (Length) + 1)) / 3 = 2 * (Natural (Length + 1)));
   Result := Result / 3;
   pragma Assert (
      Result in (2 * (Natural (Length) + 1)) * Internal_Step'First
             .. (2 * (Natural (Length) + 1)) * Internal_Step'Last);

   Result := Result + Extra_Part (Signal);
   pragma Assert (
      Result in (2 * (Natural (Length) + 1) + 2) * Internal_Step'First
             .. (2 * (Natural (Length) + 1) + 2) * Internal_Step'Last);
   pragma Assert (
      2 * (Natural (Length) + 1) + 2 = 2 * (Natural (Length) + 2));
   pragma Assert (
      Result in (2 * (Natural (Length) + 2)) * Internal_Step'First
             .. (2 * (Natural (Length) + 2)) * Internal_Step'Last);

   -- Now we know the result is within the next range.

   Factor := 2 * Natural (Length + 2);
   pragma Assert (
      Result in Factor * Internal_Step'First .. Factor * Internal_Step'Last);

   -- Check `dx'. Adding the constraints for the `dx' is going to be very
   -- complicated. Therefore I propose saturating the result and maybe marking
   -- the result has been saturated.

   -- FIXME: This can be improved by adding a propper precondition to the
   --        function. However, I don't have the time to be bothering to solve
   --        this problem for a function that isn't bottleneck.

   return Multiply (Result, dx);

end Detector.Signals.Generic_Simpson;
