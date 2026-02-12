--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-welch_tests.adb                               |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

-- with Ada.Text_IO;
with AUnit.Assertions;
-- with Detector.Signals.Generic_Welch;
-- with Generic_Compare;
-- with Python3;

package body Detector.Signals.Welch_Tests is

   overriding
   procedure Register_Tests (T : in out Test) is
   begin
      Register (T, Routines);
   end Register_Tests;

   use AUnit.Assertions;
   pragma Warnings (Off, "formal parameter ""T"" is not referenced");

   procedure Test_Results (T : in out TC) is
   begin
      Assert (False, "Not implemented");
   end Test_Results;

   pragma Warnings (On, "formal parameter ""T"" is not referenced");

end Detector.Signals.Welch_Tests;
