--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-saturating_arithmetic.adb                    |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Numerics.Saturating_Arithmetic with SPARK_Mode is

   type Long_Long_Int is
      range -2 ** (Bits * 4 - 1) .. 2 ** (Bits * 4 - 1) - 1 with
   Size => Bits * 4;

   function Generic_Saturated_Multiplication (
      Left  : in Left_Fixed_Type;
      Right : in Right_Fixed_Type)
      return Result_Fixed_Type is
      Left_Num : constant Fixed_Long_Integer :=
         Left / Left_Fixed_Type'(Left_Fixed_Type'Delta);
      Left_Den : constant Fixed_Long_Integer :=
         1.0 / Left_Fixed_Type'(Left_Fixed_Type'Delta);
      Right_Num : constant Fixed_Long_Integer :=
         Right / Right_Fixed_Type'(Right_Fixed_Type'Delta);
      Right_Den : constant Fixed_Long_Integer :=
         1.0 / Right_Fixed_Type'(Right_Fixed_Type'Delta);

      Result_First : constant Long_Long_Int := Long_Long_Int (
         Fixed_Long_Integer'(Result_Fixed_Type'First
         / Result_Fixed_Type'(Result_Fixed_Type'Delta)));
      Result_Last : constant Long_Long_Int := Long_Long_Int (
         Fixed_Long_Integer'(Result_Fixed_Type'Last
         / Result_Fixed_Type'(Result_Fixed_Type'Delta)));
      Result_Real_Den : constant Long_Long_Int := Long_Long_Int (
         Fixed_Long_Integer'(
            1.0 / Result_Fixed_Type'(Result_Fixed_Type'Delta)));

      Result     : Result_Fixed_Type'Base;
      Result_Num : Long_Long_Int;
      Result_Den : Long_Long_Int;
      Factor     : Long_Long_Int;
   begin
      pragma Assert (Left_Den > 0.0);
      pragma Assert (Right_Den > 0.0);
      pragma Assert (Result_Real_Den > 0);
   -- pragma Assert (Result_Fixed_Type'First = Result_First / Result_Real_Den);
   -- pragma Assert (Result_Fixed_Type'Last = Result_Last / Result_Real_Den);
      pragma Assert (Left_Num / Left_Den = Left);
      pragma Assert (Right_Num / Right_Den = Right);

      Result_Num := Long_Long_Int (Left_Num) * Long_Long_Int (Right_Num);
      Result_Den := Long_Long_Int (Left_Den) * Long_Long_Int (Right_Den);
      pragma Assert (Result_Den > 0);

      -- Rescale the numerator
      if Result_Den > Result_Real_Den then
         Factor := Result_Den / Result_Real_Den;
         pragma Assert (Factor > 1);
         Result_Num := Result_Num / Factor;
      elsif Result_Den < Result_Real_Den then
         Factor := Result_Real_Den / Result_Den;
         pragma Assert (Factor > 1);
         Result_Num := Result_Num * Factor;
      end if;

      -- Check the numerator is in range for the base type.
      if Result_Num < Result_First then
         return Result_Fixed_Type'First;
      elsif Result_Num > Result_Last then
         return Result_Fixed_Type'Last;
      else
         Result := Fixed_Long_Integer (Result_Num)
                   / Fixed_Long_Integer (Result_Real_Den);
         if Result in Result_Fixed_Type then
            return Result;
         elsif Result > 0.0 then
            return Result_Fixed_Type'Last;
         else
            return Result_Fixed_Type'First;
         end if;
      end if;
   end Generic_Saturated_Multiplication;

end Detector.Numerics.Saturating_Arithmetic;
