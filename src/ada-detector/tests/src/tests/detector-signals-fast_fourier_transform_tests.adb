--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-fast_fourier_transform_tests.adb              |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Ada.Text_IO;
with AUnit.Assertions;
with Detector.Signals.Fast_Fourier_Transform;
with Generic_Compare;
with Python3;

package body Detector.Signals.Fast_Fourier_Transform_Tests is

   function Compare is new Generic_Compare (Sample_Type, ε);
   function Compare (Left, Right : in Complex) return Boolean is (
      Compare (Left.Re, Right.Re) and Compare (Left.Im, Right.Im));
   function Compare (Left, Right : in Complex_Signal) return Boolean is (
               Left'Length = Right'Length
      and then (for all I in Count_Type range 0 .. Left'Length - 1 =>
                  Compare (Left (Left'First + I), Right (Right'First + I))));

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

   function Numpy_FFT (Item : in Signal_Type) return Complex_Signal is
      package Sample_IO is new Ada.Text_IO.Fixed_IO (Sample_Type);
      function Call is new Python3.Call_Return_Array (
         Element_Type => Sample_Type,
         Index_Type   => Index_Type,
         Array_Type   => Signal_Type,
         Get          => Sample_IO.Get);
      Data : constant Signal_Type := Call (
         Python3.Script ("fast_fourier_transform.py"), Item'Image);
   begin
      return Result : Complex_Signal (Item'Range) do
         for I in Count_Type range 0 .. Item'Length - 1 loop
            Result (Result'First + I) :=
               (Re => Data (Data'First + I * 2),
                Im => Data (Data'First + I * 2 + 1));
         end loop;
      end return;
   end Numpy_FFT;

   procedure Test_N_Values (T : in out TC) is
   begin
      -- FIXME: Remove the upper limit, fix python interface
      for I in 2 .. 10 loop
         exit when 2 ** I not in Index_Type;
         FFT_I : declare
            Count  : constant Count_Type := 2 ** I;
            Factor : constant Natural    := 2 ** (I + 1);
            Input  : Signal_Type (1 .. Count);
            Output : Complex_Signal (1 .. Count);
            Py_Out : Complex_Signal (1 .. Count);
            Scale  : Natural;
         begin
            -- Ada output
            for I in Input'Range loop
               Input (I) := Fixed_Integer (I - 1) / Fixed_Integer (Count);
               Input (I) := @ / Factor;
            end loop;
            Fast_Fourier_Transform (Input, Output, I, Scale);
            for I in Output'Range loop
               Output (I) := (@.Re * (2 ** Scale), @.Im * (2 ** Scale));
            end loop;
            -- Numpy output
            Py_Out := Numpy_FFT (Input);
            Assert (Compare (Output, Py_Out),
                    "FFT fails at size 2 **" & I'Image & " with:" & ASCII.LF &
                    "Ada   => " & Output'Image & ASCII.LF &
                    "Numpy => " & Py_Out'Image & ASCII.LF);
         end FFT_I;
      end loop;
   end Test_N_Values;

   pragma Warnings (On, "formal parameter ""T"" is not referenced");

end Detector.Signals.Fast_Fourier_Transform_Tests;
