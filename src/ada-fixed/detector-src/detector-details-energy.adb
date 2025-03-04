with Detector.Details.Mean;
package body Detector.Details.Energy with SPARK_Mode => On is

   function Acc_Energy_Sum (
      Item : in Real_Epoch)
      return Real_Epoch is
      Result : Real_Epoch := [others => 0.0];
   begin
      Result (Item'First) := Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I) = Result (I - 1) + Item (I)
               and then Result (I) >= 0.0));
         pragma Assert (Result (Index - 1) >= 0.0);
         pragma Assert (Item (Index) >= 0.0);
         Result (Index) := Result (Index - 1) + Item (Index);
      end loop;
      return Result;
   end Acc_Energy_Sum;

   function Scale_Array (
      Item : in Sample_Epoch;
      μ    : in Real)
      return Real_Epoch is
      Result : Real_Epoch := [others => 0.0];
      Value  : Real;
   begin
      for Index in Item'Range loop
         Value := Real (Item (Index)) - μ;
         pragma Assert (Value in S_First - S_Last .. S_Last - S_First);
         Value := Value * Value;
         pragma Assert (Value in 0.0 .. Max_Sq);
         pragma Assert (
            Value = (Real (Item (Index)) - μ) * (Real (Item (Index)) - μ));
         Result (Index) := Value;
         pragma Loop_Invariant (
            (for all I in Item'First .. Index =>
               Result (I) = (Real (Item (I)) - μ) * (Real (Item (I)) - μ)));
      end loop;
      return Result;
   end Scale_Array;

   function Energy (
      Item : in Sample_Epoch)
      return Feature_Type is
      μ      : constant Real := Real (Mean.Mean (Item));
      Value  : Real;
      Result : Real := 0.0;
      As_Arr : constant Real_Epoch := Scale_Array (Item, μ) with Ghost;
   begin

      for I in Item'Range loop
         Value := Real (Item (I)) - μ;
         pragma Assert (Value in S_First - S_Last .. S_Last - S_First);
         Value := Value * Value;
         pragma Assert (Value in 0.0 .. Max_Sq);
         pragma Assert (Value = (Real (Item (I)) - μ) * (Real (Item (I)) - μ));
         pragma Assert (Value = As_Arr (I));
         Result := Result + Value;
         pragma Loop_Invariant (Result = Acc_Energy_Sum (As_Arr) (I));
      end loop;

      pragma Assert (Result >= 0.0);
      Result := Result / Real (Item'Length);
      pragma Assert (Result >= 0.0);
      return Feature_Type ((if Result > F_Last then F_Last else Result));
   end Energy;

end Detector.Details.Energy;
