--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-simpson_tests.ads                             |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Ada.Text_IO;
with AUnit.Assertions;
with Detector.Signals.Generic_Simpson;
with Generic_Compare;
with Python3;

package body Detector.Signals.Simpson_Tests is

   -- Result type for instantiations

   Result_Bits          : constant := Bits;
   Result_Whole_Bits    : constant := 10;
   Result_Fraction_Bits : constant := Result_Bits - Result_Whole_Bits - 1;
   Result_Delta         : constant := 2.0 ** (-Result_Fraction_Bits);
   type Result_Type is
      delta Result_Delta
      range -2.0 ** Result_Whole_Bits
         .. 2.0 ** Result_Whole_Bits - Result_Delta with
      Size => Result_Bits;

   function Compare is new Generic_Compare (Result_Type, ε);

   function Simpson is new Generic_Simpson (Result_Type);

   -- Test registering

   overriding
   procedure Register_Tests (T : in out Test) is
   begin
      Register (T, Routines);
   end Register_Tests;

   -- Python Function call

   function Scipy_Simps (
      Item : in Signal_Type;
      dx   : in Sample_Type)
      return Result_Type is
      package Result_IO is new Ada.Text_IO.Fixed_IO (Result_Type);
      function Call is new Python3.Call_Return_Type (
         Element_Type => Result_Type,
         Get          => Result_IO.Get);
   begin
      return Call (
         Python3.Script ("simpson.py"),
         "{""dx"" : " & dx'Image & ", ""signal"" : " & Item'Image & "}");
   end Scipy_Simps;

   -- Test cases

   use AUnit.Assertions;
   pragma Warnings (Off, "formal parameter ""T"" is not referenced");

   procedure Test_Nil_dx (T : in out TC) is
      Result : Result_Type;
   begin
      Result := Simpson ([0.125, 0.25, 0.0, 0.25, 0.5], 0.0);
      Assert (Result /= Result, "Simpson with dx => 0.0 should not compile");
   exception
      when others =>
         Assert (True, "Excepton raised!");
   end Test_Nil_dx;

   procedure Test_0_Values (T : in out TC) is
      Result : Result_Type;
   begin
      Result := Simpson ([], 0.25);
      Assert (Result /= Result, "Simpson of empty array is erroneous");
   exception
      when others =>
         Assert (True, "Excepton raised!");
   end Test_0_Values;

   procedure Test_1_Value (T : in out TC) is
      Result : Result_Type;
   begin
      Result := Simpson ([0.5], 0.25);
      Assert (Result /= Result, "Simpson of a one-element array is erroneous");
   exception
      when others =>
         Assert (True, "Excepton raised!");
   end Test_1_Value;

   procedure Test_2_Values (T : in out TC) is
      Result : Result_Type;
   begin
      Result := Simpson ([0.5, 0.25], 0.25);
      Assert (Result /= Result, "Simpson of a two-element array is erroneous");
   exception
      when others =>
         Assert (True, "Excepton raised!");
   end Test_2_Values;

   -- Evaluate and fill arrays with function data

   generic
      with function F (X, Y : in Count_Type) return Sample_Type;
   procedure Generic_Evaluate (To : out Signal_Type);
   procedure Generic_Evaluate (To : out Signal_Type) is
   begin
      for I in To'Range loop
         To (I) := F (I - To'First, To'Length);
      end loop;
   end Generic_Evaluate;

   function Slope_Func (X, Y : in Count_Type) return Sample_Type is (
      Fixed_Integer (X) / Fixed_Integer (Y));
   procedure Fill_Slope is new Generic_Evaluate (F => Slope_Func);

   -- Test the functions that call python.

   procedure Test_Even_Values (T : in out TC) is
      T_1      : Signal_Type (1 .. 100);    dx_1 : constant := 0.25;
      T_2      : Signal_Type (1 .. 1000);   dx_2 : constant := 0.125;
      Expected : Result_Type;
      Got      : Result_Type;
   begin
      Fill_Slope (T_1);
      Expected := Scipy_Simps (T_1, dx_1);
      Got := Simpson (T_1, dx_1);
      Assert (Compare (Expected, Got),
              "100 element array: expected " & Expected'Image
              & " got " & Got'Image);

      Fill_Slope (T_2);
      Expected := Scipy_Simps (T_2, dx_2);
      Got := Simpson (T_2, dx_2);
      Assert (Compare (Expected, Got),
              "1000 element array: expected " & Expected'Image
              & " got " & Got'Image);
   end Test_Even_Values;

   procedure Test_Odd_Values  (T : in out TC) is
      T_1      : Signal_Type (1 .. 99);    dx_1 : constant := 0.25;
      T_2      : Signal_Type (1 .. 999);   dx_2 : constant := 0.125;
      Expected : Result_Type;
      Got      : Result_Type;
   begin
      Fill_Slope (T_1);
      Expected := Scipy_Simps (T_1, dx_1);
      Got := Simpson (T_1, dx_1);
      Assert (Compare (Expected, Got),
              "99 element array: expected " & Expected'Image
              & " got " & Got'Image);

      Fill_Slope (T_2);
      Expected := Scipy_Simps (T_2, dx_2);
      Got := Simpson (T_2, dx_2);
      Assert (Compare (Expected, Got),
              "999 element array: expected " & Expected'Image
              & " got " & Got'Image);
   end Test_Odd_Values;

   pragma Warnings (On, "formal parameter ""T"" is not referenced");

end Detector.Signals.Simpson_Tests;
