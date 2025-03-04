package body Detector.Details.Mean with SPARK_Mode => On is

   function Lemma_Sample_Increment (
      Left  : in Sample_Base_Type;
      Right : in Sample_Base_Type;
      Index : in Count_Type)
      return Boolean is (True);

   function Acc_Sum (
      Item : in Sample_Epoch)
      return Sample_Base_Epoch is
      Result : Sample_Base_Epoch := [others => 0.0];
   begin
      Result (Item'First) := Item (Item'First);
      pragma Assert (Result (Item'First) in Sample_Type);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1
               =>       Result (I - 1)
                           in Sample_Base_Type (I - 1) * Sample_Type'First
                           .. Sample_Base_Type (I - 1) * Sample_Type'Last
               and then Lemma_Sample_Increment (Result (I - 1), Item (I),
                                                I - 1)
               and then Result (I) = Result (I - 1) + Item (I)
               and then Result (I)
                           in Sample_Base_Type (I) * Sample_Type'First
                           .. Sample_Base_Type (I) * Sample_Type'Last));
         Result (Index) := Result (Index - 1) + Item (Index);
      end loop;
      return Result;
   end Acc_Sum;

   function Sum (
      Item : in Sample_Epoch)
      return Sample_Base_Type is
      Result : Sample_Base_Type := Item (Item'First);
   begin
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result = Acc_Sum (Item) (Index - 1));
         Result := Result + Item (Index);
      end loop;
      return Result;
   end Sum;

   function Mean (
      Item : in Sample_Epoch)
      return Sample_Type is
      Result : Sample_Base_Type;
   begin
      Result := Sum (Item);
      pragma Assert (Result
                        in Sample_Base_Type (Epoch_Size) * Sample_Type'First
                        .. Sample_Base_Type (Epoch_Size) * Sample_Type'Last);
      pragma Assert (Result / Sample_Base_Type (Epoch_Size) in Sample_Type);
      return Result / Sample_Base_Type (Epoch_Size);
   end Mean;

end Detector.Details.Mean;
