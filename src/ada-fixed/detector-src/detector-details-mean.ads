package Detector.Details.Mean with SPARK_Mode => On is

   pragma Warnings (Off,
      "conjunct in postcondition does not check the outcome of calling ""Lemma_Sample_Increment""",
      Reason => "It is checked that the Result is True, but we don't need it");

   function Lemma_Sample_Increment (
      Left  : in Sample_Base_Type;
      Right : in Sample_Base_Type;
      Index : in Count_Type)
      return Boolean with
      Ghost    => True,
      Global   => null,
      Pre      => Index in 1 .. Epoch_Size - 1
         and then Left in Sample_Base_Type (Index) * Sample_Type'First
                       .. Sample_Base_Type (Index) * Sample_Type'Last
         and then Right in Sample_Type,
      Post     => Lemma_Sample_Increment'Result = True
         and then Left + Right
                     in Sample_Base_Type (Index + 1) * Sample_Type'First
                     .. Sample_Base_Type (Index + 1) * Sample_Type'Last;

   function Acc_Sum (
      Item : in Sample_Epoch)
      return Sample_Base_Epoch with
      Ghost    => True,
      Global   => null,
      Post     => Acc_Sum'Result (Item'First) = Item (Item'First)
         and then (for all I in Item'Range =>
                     Acc_Sum'Result (I)
                        in Sample_Base_Type (I) * Sample_Type'First
                        .. Sample_Base_Type (I) * Sample_Type'Last)
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Sum'Result (I) = Acc_Sum'Result (I - 1) + Item (I));

   function Sum (
      Item : in Sample_Epoch)
      return Sample_Base_Type with
      Global   => null,
      Post     => Sum'Result = Acc_Sum (Item) (Item'Last)
         and then Sum'Result
                     in Sample_Base_Type (Epoch_Size) * Sample_Type'First
                     .. Sample_Base_Type (Epoch_Size) * Sample_Type'Last;

   function Mean (
      Item : in Sample_Epoch)
      return Sample_Type with
      Global => null,
      Post   => Mean'Result = Sum (Item) / Sample_Base_Type (Epoch_Size);

end Detector.Details.Mean;
