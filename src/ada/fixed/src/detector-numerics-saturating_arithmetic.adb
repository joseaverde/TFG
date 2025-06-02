--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-saturating_arithmetic.adb                    |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Numerics.Saturating_Arithmetic with SPARK_Mode is

   pragma Unsuppress (All_Checks);

   function Generic_Saturated_Multiplication (
      Left  : in Left_Fixed_Type;
      Right : in Right_Fixed_Type)
      return Result_Fixed_Type is
      pragma SPARK_Mode (Off);
      Result : Result_Fixed_Type;
   begin
      Result := Left * Right;
      return Result;
   exception
      when Constraint_Error =>
         if (Left <= 0.0 and then Right <= 0.0)
            or else (Left >= 0.0 and then Right >= 0.0) then
            return Result_Fixed_Type'Last;
         else
            return Result_Fixed_Type'First;
         end if;
   end Generic_Saturated_Multiplication;

end Detector.Numerics.Saturating_Arithmetic;
