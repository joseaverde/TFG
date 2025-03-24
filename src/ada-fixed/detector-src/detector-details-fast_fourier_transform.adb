package body Detector.Details.Fast_Fourier_Transform with SPARK_Mode => On is

   function Acc_Scaling_Factor (
      Item : in Fast_Fourier_Transform_Input)
      return Fast_Fourier_Transform_Input is
      Result : Fast_Fourier_Transform_Input := [others => 0.0];
   begin
      Result (Item'First) := abs Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result (Item'First) = abs Item (Item'First));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I) = Sample_Type'Max (Result (I - 1), abs Item (I))));
         Result (Index) := Sample_Type'Max (Result (Index - 1),
                                            abs Item (Index));
      end loop;
      return Result;
   end Acc_Scaling_Factor;

   function Get_Scaling_Factor (
      Item : in Fast_Fourier_Transform_Input)
      return Scaling_Factor is
      Result : Scaling_Factor := abs Item (Item'First);
   begin
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (
            Result = Acc_Scaling_Factor (Item) (Index - 1));
         pragma Loop_Invariant (
            (for all I in Item'First .. Index - 1 =>
               Result >= abs Item (I)));
         pragma Loop_Invariant (
            (for some I in Item'First .. Index - 1 =>
               Result = abs Item (I)));
         Result := Sample_Type'Max (Result, abs Item (Index));
      end loop;
      return Result;
   end Get_Scaling_Factor;

   function Scale (
      Item   : in Sample_Type;
      Factor : in Scaling_Factor)
      return Normalised_Sample is
   begin
      return Item / Factor;
   end Scale;

   procedure Scale (
      Item   : in     Fast_Fourier_Transform_Input;
      Result :    out Normalised_Sample_Window;
      Factor :    out Scaling_Factor) is
   begin
      Factor := Get_Scaling_Factor (Item);
      pragma Assert ((for all X of Item => Factor >= abs X));
      pragma Assert (Result'Length = Item'Length);
      if Factor <= 1.0 then
         for I in Result'Range loop
            Result (I) := Normalised_Sample (
               Item (I - Result'First + Item'First));
         end loop;
         Factor := 1.0;
         pragma Assert (Factor >= 1.0);
      else
         for I in Result'Range loop
            pragma Loop_Invariant (Factor > 1.0);
            pragma Loop_Invariant ((for all X of Item => Factor >= abs X));
            Result (I) := Scale (Item (I - Result'First + Item'First), Factor);
         end loop;
         pragma Assert (Factor >= 1.0);
      end if;
      pragma Assert (Factor >= 1.0);
   end Scale;

   function Rescale_Factor (
      Factor : in Scaling_Factor;
      Shifts : in Shift_Count)
      return Output_Sample is
      Power : constant Natural := 2 ** Shifts;
   begin
      pragma Assert (Power in 1 .. Max_Factor_By_Shifting);
      pragma Assert (Factor in 1.0 .. Sample_Type'Last);
      pragma Assert (Output_Sample (Factor) in 1.0 .. Sample_Type'Last);
      return Factor * Power;
   end Rescale_Factor;

   procedure Rescale (
      Item   : in     Normalised_Complex_Window;
      Result :    out Fast_Fourier_Transform_Output;
      Factor : in     Scaling_Factor;
      Shifts : in     Shift_Count) is
      Scale : constant Output_Sample := Rescale_Factor (Factor, Shifts);
   begin
      for I in Item'Range loop
         Result (I) := (Item (I).Re * Scale, Item (I).Im * Scale);
      end loop;
   end Rescale;

-- function "*" (Left, Right : in Normalised_Complex)
--    return Normalised_Complex is
--    Result : Normalised_Complex;
-- begin
--    Result.Re := Left.Re * Right.Re - Left.Im * Right.Im;
--    Result.Im := Left.Re * Right.Im + Left.Re * Right.Im;
--    return Result;
-- end "*";

   procedure Conquer (
      Input  : in     Normalised_Complex_Window;
      Output :    out Normalised_Complex_Window;
      Chunk  : in     Positive_Count_Type;
      Shift  :    out Boolean) is
   begin
      Shift := False;
      Output := Input;
   end Conquer;

   procedure Fast_Fourier_Transform (
      Input  : in     Fast_Fourier_Transform_Input;
      Output :    out Fast_Fourier_Transform_Output) is

      Buffer : array (Boolean) of Normalised_Complex_Window;
      Scaled : Normalised_Sample_Window;
      Factor : Scaling_Factor;
      Shifts : Shift_Count := 0;
      Result : Boolean := True;
      Chunk  : Positive_Count_Type := 1;
      Shift  : Boolean;

   begin
      Scale (Input, Scaled, Factor);
      Buffer := [True  => [for I in Scaled'Range => (Scaled (I), 0.0)],
                 False => [for I in Scaled'Range => (0.0, 0.0)]];
      for Layer in 1 .. Fourier_Transform_Recursion_Depth loop
         Conquer (Buffer (Result), Buffer (not Result), Chunk, Shift);
         pragma Annotate (
            GNATprove,
            False_Positive,
            "formal parameters ""Input"" and ""Output"" might be aliased",
            """Input"" and ""Output"" are not aliased Result /= not Result");
         Chunk := Chunk * 2;
         Result := not Result;
         if Shift then
            Shifts := Shifts + 1;
         end if;
      end loop;
      Rescale (Buffer (Result), Output, Factor, Shifts);
   end Fast_Fourier_Transform;

end Detector.Details.Fast_Fourier_Transform;
