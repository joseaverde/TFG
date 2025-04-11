with AUnit;
with Detector.Tests_Helper; use Detector.Tests_Helper;

package Detector.Signals.Fast_Fourier_Transform_Tests is

   type Test is new Test_Case with null record;
   overriding procedure Register_Tests (T : in out Test);
   overriding function Name (T : in Test) return AUnit.Message_String is (
      AUnit.Format ("Detector.Numerics.Fast_Fourier_Transform"));

   procedure Test_1_Value  (T : in out TC);
   procedure Test_2_Values (T : in out TC);
   procedure Test_N_Values (T : in out TC);

private

   Routines : constant Routine_Array := [
      Make ("FFT (V), V'Length = 1",      Test_1_Value'Access),
      Make ("FFT (V), V'Length = 2",      Test_2_Values'Access),
      Make ("FFT (V), V'Length = 2 ** N", Test_N_Values'Access)
   ];

end Detector.Signals.Fast_Fourier_Transform_Tests;
