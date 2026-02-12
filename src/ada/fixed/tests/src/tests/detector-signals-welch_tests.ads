--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-welch_tests.ads                               |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with AUnit;
with Detector.Tests_Helper; use Detector.Tests_Helper;

package Detector.Signals.Welch_Tests is

   ε : constant Sample_Type := 1.0 / (2 ** 10);

   type Test is new Test_Case with null record;

   overriding procedure Register_Tests (T : in out Test);
   overriding function Name (T : in Test) return AUnit.Message_String is (
      AUnit.Format ("Detector.Numerics.Welch"));

   procedure Test_Results (T : in out TC);

private

   Routines : constant Routine_Array := [
      Make ("Welch (X) = Python's one", Test_Results'Access)
   ];

end Detector.Signals.Welch_Tests;
