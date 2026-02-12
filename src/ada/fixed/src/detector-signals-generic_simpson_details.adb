--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_simpson_details.adb                   |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Signals.Generic_Simpson_Details with SPARK_Mode is

   function Extra_Part (
      Signal : in Valid_Signal)
      return Extra_Internal_Part is
      Extra : Internal_Type := 0.0;
   begin
      if Signal'Length mod 2 = 0 then
         Extra := 5 * Internal_Type (Signal (Signal'Last));
         Extra := @ + 8 * Internal_Type (Signal (Signal'Last - 1));
         Extra := @ - Internal_Type (Signal (Signal'Last - 2));
         Extra := Extra / 12;
      end if;
      return Extra;
   end Extra_Part;

   function First_And_Last_Iterations (
      Signal : in Valid_Signal)
      return Rest_Internal_Part is
      Result : Rest_Internal_Part;
      Index  : Index_Type;
   begin
      if Signal'Length mod 2 = 1 then
         Index := Signal'Last - 1;
         pragma Assert (Index in Signal'Range);
         pragma Assert (Index + 1 in Signal'Range);
      else
         pragma Assert (Signal'Length >= 4);
         Index := Signal'Last - 2;
         pragma Assert (Index in Signal'Range);
         pragma Assert (Index + 1 in Signal'Range);
      end if;
      pragma Assert (Index in Signal'Range);
      pragma Assert (Index + 1 in Signal'Range);

      Result := Rest_Internal_Part (Signal (Signal'First));
      Result := @ + 4 * Rest_Internal_Part (Signal (Index));
      Result := @ + Rest_Internal_Part (Signal (Index + 1));
      return Result;
   end First_And_Last_Iterations;

   function Take_Evens (Signal : in Valid_Signal) return Internal_Array is
      Result : Internal_Array (1 .. Middle_Length (Signal)) := [others => 0.0];
   begin
      for Index in Result'Range loop
         Result (Index) := Internal_Type (Signal (Signal'First + Index * 2));
         pragma Loop_Invariant (
            (for all I in 1 .. Index =>
               Result (I) = Internal_Type (Signal (Signal'First + I * 2))
               and then Result (I) in Internal_Step));
      end loop;
      return Result;
   end Take_Evens;

   function Take_Odds (Signal : in Valid_Signal) return Internal_Array is
      Result : Internal_Array (1 .. Middle_Length (Signal)) := [others => 0.0];
   begin
      for Index in Result'Range loop
         Result (Index) :=
            Internal_Type (Signal (Signal'First + Index * 2 - 1));
         pragma Loop_Invariant (
            (for all I in 1 .. Index =>
               Result (I) = Internal_Type (Signal (Signal'First + I * 2 - 1))
               and then Result (I) in Internal_Step));
      end loop;
      return Result;
   end Take_Odds;

   function Even_Result (Signal : in Valid_Signal) return Internal_Type is
      subtype Step is Internal_Step;
      Length : constant Count_Type     := Middle_Length (Signal);
      Taken  : constant Internal_Array := Take_Evens (Signal) with Ghost;
      Result : Internal_Type := 0.0;
   begin
      pragma Assert (Length = Taken'Length);
      if Length > 0 then
         Result := Step (Signal (Signal'First + 2));
         pragma Assert (Result = Taken (1));
         pragma Assert (Taken (1) = Acc_Sum (Taken) (1));
         for Index in 2 .. Length loop
            Result := @ + Step (Signal (Signal'First + Index * 2));
            pragma Loop_Invariant (
               Taken (Index) = Step (Signal (Signal'First + Index * 2)));
            pragma Loop_Invariant (Result = Acc_Sum (Taken) (Index));
         end loop;
      end if;
      return Result;
   end Even_Result;

   function Odd_Result (Signal : in Valid_Signal) return Internal_Type is
      subtype Step is Internal_Step;
      Length : constant Count_Type     := Middle_Length (Signal);
      Taken  : constant Internal_Array := Take_Odds (Signal) with Ghost;
      Result : Internal_Type := 0.0;
   begin
      pragma Assert (Length = Taken'Length);
      if Length > 0 then
         Result := Step (Signal (Signal'First + 1));
         pragma Assert (Result = Taken (1));
         pragma Assert (Taken (1) = Acc_Sum (Taken) (1));
         for Index in 2 .. Length loop
            Result := @ + Step (Signal (Signal'First + Index * 2 - 1));
            pragma Loop_Invariant (
               Taken (Index) = Step (Signal (Signal'First + Index * 2 - 1)));
            pragma Loop_Invariant (Result = Acc_Sum (Taken) (Index));
         end loop;
      end if;
      return Result;
   end Odd_Result;

end Detector.Signals.Generic_Simpson_Details;
