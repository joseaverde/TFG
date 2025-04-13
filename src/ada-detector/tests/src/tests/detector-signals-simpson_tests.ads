--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-simpson_tests.ads                             |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with AUnit;
with Detector.Tests_Helper; use Detector.Tests_Helper;

package Detector.Signals.Simpson_Tests is

   type Test is new Test_Case with null record;
   overriding procedure Register_Tests (T : in out Test);
   overriding function Name (T : in Test) return AUnit.Message_String is (
      AUnit.Format ("Detector.Numerics.Generic_Simpson"));

   procedure Test_Nil_dx      (T : in out TC);
   procedure Test_0_Values    (T : in out TC);
   procedure Test_1_Value     (T : in out TC);
   procedure Test_2_Values    (T : in out TC);
   procedure Test_Even_Values (T : in out TC);
   procedure Test_Odd_Values  (T : in out TC);

   ε : constant := 1.0 / (1024.0 * 1024.0);

private

   Routines : constant Routine_Array := [
      Make ("Simpson ([...], dx=>0.0) -> Error", Test_Nil_dx'Access),
      Make ("Simpson (0 Values)       -> Error", Test_0_Values'Access),
      Make ("Simpson (1 Value)        -> Error", Test_1_Value'Access),
      Make ("Simpson (2 Values)       -> Error", Test_2_Values'Access),
      Make ("Simpson (Odd)",                     Test_Even_Values'Access),
      Make ("Simpson (Even)",                    Test_Odd_Values'Access)
   ];

end Detector.Signals.Simpson_Tests;
