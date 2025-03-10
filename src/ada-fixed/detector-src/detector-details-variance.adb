package body Detector.Details.Variance is

   function Acc_Sum_Squares (
      Item : in Real_Epoch)
      return Real_Epoch is
      Result : Real_Epoch := [others => 0.0];
   begin
      Result (Item'First) := Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I - 1) >= 0.0 and then Item (I) >= 0.0 and then
               Result (I - 1) in 0.0 .. Real (I - 1) * Max_Sq and then
               Result (I) = Result (I - 1) + Item (I) and then
               Result (I) in 0.0 .. Real (I) * Max_Sq));
         Result (Index) := Result (Index - 1) + Item (Index);
      end loop;
      return Result;
   end Acc_Sum_Squares;

   function Map_Square (
      Item : in Sample_Epoch)
      return Real_Epoch is
      Result : Real_Epoch := [others => 0.0];
   begin
      for Index in Item'Range loop
         Result (Index) := Real (Item (Index)) * Real (Item (Index));
         pragma Loop_Invariant (
            (for all I in Item'First .. Index =>
               Result (I) = Real (Item (I)) * Real (Item (I))));
      end loop;
      return Result;
   end Map_Square;

   function Sum_Squares (
      Item : in Sample_Epoch)
      return Real is
      Result : Real := 0.0;
      State  : constant Real_Epoch := Map_Square (Item) with Ghost;
   begin
      for I in Item'Range loop
         Result := Result + Real (Item (I)) * Real (Item (I));
         pragma Loop_Invariant (Result = Acc_Sum_Squares (State) (I));
      end loop;
      return Result;
   end Sum_Squares;

end Detector.Details.Variance;
