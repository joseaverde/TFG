package body Detector.Details.Max_Distance with SPARK_Mode => On is

   function Acc_Maximum (
      Item : in Sample_Epoch)
      return Sample_Epoch is
      Result : Sample_Epoch := [others => Sample_Type'First];
   begin
      Result (Item'First) := Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (
            Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I) = Sample_Type'Max (Item (I), Result (I - 1))));
         Result (Index) := Sample_Type'Max (Item (Index), Result (Index - 1));
      end loop;
      return Result;
   end Acc_Maximum;

   function Acc_Minimum (
      Item : in Sample_Epoch)
      return Sample_Epoch is
      Result : Sample_Epoch := [others => Sample_Type'First];
   begin
      Result (Item'First) := Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (
            Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I) = Sample_Type'Min (Item (I), Result (I - 1))));
         Result (Index) := Sample_Type'Min (Item (Index), Result (Index - 1));
      end loop;
      return Result;
   end Acc_Minimum;

   function Maximum (
      Item : in Sample_Epoch)
      return Sample_Type is
      Result : Sample_Type := Item (Item'First);
   begin
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (
            (for all I in Item'First .. Index - 1 =>
               Result >= Item (I)));
         pragma Loop_Invariant (
            (for some I in Item'First .. Index - 1 =>
               Item (I) = Result));
         pragma Loop_Invariant (Result = Acc_Maximum (Item) (Index - 1));
         Result := Sample_Type'Max (Result, Item (Index));
      end loop;
      return Result;
   end Maximum;

   function Minimum (
      Item : in Sample_Epoch)
      return Sample_Type is
      Result : Sample_Type := Item (Item'First);
   begin
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (
            (for all I in Item'First .. Index - 1 =>
               Result <= Item (I)));
         pragma Loop_Invariant (
            (for some I in Item'First .. Index - 1 =>
               Item (I) = Result));
         pragma Loop_Invariant (Result = Acc_Minimum (Item) (Index - 1));
         Result := Sample_Type'Min (Result, Item (Index));
      end loop;
      return Result;
   end Minimum;

end Detector.Details.Max_Distance;
