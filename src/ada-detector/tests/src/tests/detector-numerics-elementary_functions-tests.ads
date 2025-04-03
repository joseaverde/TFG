with AUnit;
with Detector.Tests_Helper; use Detector.Tests_Helper;

package Detector.Numerics.Elementary_Functions.Tests is

   type Test is new Test_Case with null record;
   overriding procedure Register_Tests (T : in out Test);
   overriding function Name (T : in Test) return AUnit.Message_String is (
      AUnit.Format ("Detector.Numerics.Elementary_Functions"));

   -->> Cosine tests <<--

   -->> Sine tests <<--

   -->> Square Root tests <<--

   Fixed_Bits          : constant := 32;
   Fixed_Whole_Bits    : constant := 13;
   Fixed_Fraction_Bits : constant := Fixed_Bits - Fixed_Whole_Bits - 1;
   Fixed_Delta         : constant := 2.0 ** (-Fixed_Fraction_Bits);
   type Fixed is delta Fixed_Delta
      range -2.0 ** Fixed_Whole_Bits .. 2.0 ** Fixed_Whole_Bits - Fixed_Delta
      with Size => Fixed_Bits;

   ε : constant := 0.01;

   procedure Test_Square_Root_Of_Zero      (T : in out TC);
   procedure Test_Square_Root_Of_One       (T : in out TC);
   procedure Test_Square_Root_Of_Four      (T : in out TC);
   procedure Test_Square_Root_Of_Negative  (T : in out TC);
   procedure Test_Square_Root_Of_Max       (T : in out TC);
   procedure Test_Square_Root_Of_A_Quarter (T : in out TC);
   procedure Test_Square_Root_Error        (T : in out TC);

private

   Routines : constant Routine_Array := [
      -->> Cosine <<--
      -->> Sine <<--
      -->> Square Root <<--
      Make ("Sqrt (0)",       Test_Square_Root_Of_Zero'Access),
      Make ("Sqrt (1)",       Test_Square_Root_Of_One'Access),
      Make ("Sqrt (4)",       Test_Square_Root_Of_Four'Access),
      Make ("Sqrt (X), X<0",  Test_Square_Root_Of_Negative'Access),
      Make ("Sqrt (Last)",    Test_Square_Root_Of_Max'Access),
      Make ("Sqrt (0.25)",    Test_Square_Root_Of_A_Quarter'Access),
      Make ("Sqrt Error",     Test_Square_Root_Error'Access)
   ];

end Detector.Numerics.Elementary_Functions.Tests;
