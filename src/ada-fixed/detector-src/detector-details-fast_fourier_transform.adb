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
      Result :    out Normalised_Sample_Epoch;
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
      else
         for I in Result'Range loop
            pragma Loop_Invariant (Factor > 1.0);
            pragma Loop_Invariant ((for all X of Item => Factor >= abs X));
            Result (I) := Scale (Item (I - Result'First + Item'First), Factor);
         end loop;
      end if;
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
      Item   : in     Normalised_Sample_Epoch;
      Result :    out Fast_Fourier_Transform_Output;
      Factor : in     Scaling_Factor;
      Shifts : in     Shift_Count) is
      Scale : constant Output_Sample := Rescale_Factor (Factor, Shifts);
   begin
      for I in Item'Range loop
         Result (I) := Item (I) * Scale;
      end loop;
   end Rescale;

end Detector.Details.Fast_Fourier_Transform;
