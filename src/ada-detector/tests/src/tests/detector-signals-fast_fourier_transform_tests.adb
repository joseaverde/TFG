with AUnit.Assertions;
-- with Detector.Signals.Fast_Fourier_Transform;
with Python3;

pragma Unreferenced (Python3);
package body Detector.Signals.Fast_Fourier_Transform_Tests is

   overriding
   procedure Register_Tests (T : in out Test) is
   begin
      Register (T, Routines);
   end Register_Tests;

   use AUnit.Assertions;
   pragma Warnings (Off, "formal parameter ""T"" is not referenced");

   procedure Test_1_Value  (T : in out TC) is
      -- Input_1 : Signal_Type (1 .. 1) := [1.0];
   begin
      -- Fast_Fourier_Transform (Input_1
      Assert (True, ":)");
   end Test_1_Value;

   procedure Test_2_Values (T : in out TC) is
   begin
      null;
   end Test_2_Values;

   procedure Test_N_Values (T : in out TC) is
   begin
      null;
   end Test_N_Values;

   pragma Warnings (On, "formal parameter ""T"" is not referenced");

end Detector.Signals.Fast_Fourier_Transform_Tests;
