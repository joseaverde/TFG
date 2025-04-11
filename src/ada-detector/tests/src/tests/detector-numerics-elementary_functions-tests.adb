--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-elementary_functions-tests.adb               |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Ada.Numerics.Elementary_Functions;
with AUnit.Assertions;
with Generic_Compare;

package body Detector.Numerics.Elementary_Functions.Tests is

   overriding
   procedure Register_Tests (T : in out Test) is
   begin
      Register (T, Routines);
   end Register_Tests;

   use AUnit.Assertions;
   pragma Warnings (Off, "formal parameter ""T"" is not referenced");

   function Sqrt is new Fixed_Sqrt (Fixed);

   procedure Test_Square_Root_Of_Zero (T : in out TC) is
   begin
      Assert (Sqrt (0.0) = 0.0, "Expected 0.0, got" & Sqrt (0.0)'Image);
   end Test_Square_Root_Of_Zero;

   procedure Test_Square_Root_Of_One (T : in out TC) is
   begin
      Assert (Sqrt (1.0) = 1.0, "Expected 1.0, got" & Sqrt (1.0)'Image);
   end Test_Square_Root_Of_One;

   procedure Test_Square_Root_Of_Four (T : in out TC) is
   begin
      Assert (Sqrt (4.0) = 2.0, "Expected 2.0, got" & Sqrt (4.0)'Image);
   end Test_Square_Root_Of_Four;

   procedure Test_Square_Root_Of_Negative (T : in out TC) is
      Value : Fixed with Unreferenced;
   begin
      Value := Sqrt (-10.0);
      Assert (False, "Sqrt (-10.0) should raise an exception");
   exception
      when others =>
         Assert (True, "Exception raised");
   end Test_Square_Root_Of_Negative;

   procedure Test_Square_Root_Of_Max (T : in out TC) is
   begin
      Assert (Sqrt (Fixed (1024)) = 32.0,
              "Sqrt (1024) = 32 /=" & Sqrt (Fixed (1024))'Image);
   end Test_Square_Root_Of_Max;

   procedure Test_Square_Root_Of_A_Quarter (T : in out TC) is
   begin
      Assert (Sqrt (0.25) = 0.5, "Expected 1/2, got" & Sqrt (0.25)'Image);
   end Test_Square_Root_Of_A_Quarter;

   function Compare is new Generic_Compare (Fixed, ε);

   procedure Test_Square_Root_Error (T : in out TC) is
      use Ada.Numerics.Elementary_Functions;
      type Fixed_Array is array (Positive range <>) of Fixed;
      Values : constant Fixed_Array := [1.0, 2.0, 3.0, 4.0, 7.0, 42.0, 69.0];
   begin
      for Val of Values loop
         Assert (Compare (Sqrt (Val), Fixed (Sqrt (Float (Val)))),
                 "Sqrt (" & Val'Image & "): " &
                 "Expected " & Sqrt (Float (Val))'Image &
                 "; got " & Sqrt (Val)'Image);
         Assert (Compare (Sqrt (1.0 / Val), Fixed (Sqrt (Float (1.0 / Val)))),
                 "Sqrt (" & Fixed'Image (1.0 / Val) & "): " &
                 "Expected " & Sqrt (Float (1.0 / Val))'Image &
                 "; got " & Sqrt (1.0 / Val)'Image);
      end loop;
   end Test_Square_Root_Error;

   pragma Warnings (On, "formal parameter ""T"" is not referenced");

end Detector.Numerics.Elementary_Functions.Tests;
