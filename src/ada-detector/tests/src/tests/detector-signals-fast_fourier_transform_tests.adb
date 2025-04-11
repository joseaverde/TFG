--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-fast_fourier_transform_tests.adb              |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with AUnit.Assertions;
with Detector.Signals.Fast_Fourier_Transform;
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
      Output   : Complex_Signal (1 .. 1);
      Scale    : Natural;
      Input_1  : constant Signal_Type (1 .. 1)    := [Sample_Type'Last];
      Output_1 : constant Complex_Signal (3 .. 3) := [(Sample_Type'Last, 0.0)];
      Input_2  : constant Signal_Type (2 .. 2)    := [0.5];
      Output_2 : constant Complex_Signal (3 .. 3) := [(0.5, 0.0)];
      Input_3  : constant Signal_Type (2 .. 2)    := [-0.25];
      Output_3 : constant Complex_Signal (3 .. 3) := [(-0.25, 0.0)];
   begin
      Fast_Fourier_Transform (Input_1, Output, 0, Scale);
      Assert (Output = Output_1, Output'Image & " /= " & Output_1'Image);
      Fast_Fourier_Transform (Input_2, Output, 0, Scale);
      Assert (Output = Output_2, Output'Image & " /= " & Output_2'Image);
      Fast_Fourier_Transform (Input_3, Output, 0, Scale);
      Assert (Output = Output_3, Output'Image & " /= " & Output_3'Image);
   end Test_1_Value;

   procedure Test_2_Values (T : in out TC) is
      -- FFT ([X, Y]) = [(X + Y, 0), (X - Y, 0)]
      use all type Complex;
      Output   : Complex_Signal (10 .. 11);
      Input_1  : constant Signal_Type (1 .. 2)    := [0.25, 0.25];
      Output_1 : constant Complex_Signal (1 .. 2) := [(0.5, 0.0), (0.0, 0.0)];
      Input_2  : constant Signal_Type (1 .. 2)    := [0.25, -0.25];
      Output_2 : constant Complex_Signal (1 .. 2) := [(0.0, 0.0), (0.5, 0.0)];
      Scale    : Natural;
   begin
      -- Test 1
      Fast_Fourier_Transform (Input_1, Output, 1, Scale);
      Output := [for X of Output => X * (2 ** Scale)];
      Assert (Output = Output_1, Output'Image & " /= " & Output_1'Image);
      -- Test 2
      Fast_Fourier_Transform (Input_2, Output, 1, Scale);
      Output := [for X of Output => X * (2 ** Scale)];
      Assert (Output = Output_2, Output'Image & " /= " & Output_2'Image);
   end Test_2_Values;

   procedure Test_N_Values (T : in out TC) is
   begin
      null;
   end Test_N_Values;

   pragma Warnings (On, "formal parameter ""T"" is not referenced");

end Detector.Signals.Fast_Fourier_Transform_Tests;
