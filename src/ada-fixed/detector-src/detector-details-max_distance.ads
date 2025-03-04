package Detector.Details.Max_Distance with SPARK_Mode => On is

   -- The Max_Distance is a function that returns the difference between the
   -- maximum value on an Epoch and the minimum value of an Epoch. Then if
   -- the domain is:
   --
   --    Domain is range First .. Last
   --
   -- We now that:
   --
   --    Maximum (Epoch) >= Minimum (Epoch)
   --
   -- Then if:
   --
   --    Maximum (Epoch) = Minimum (Epoch) => Result := 0.0
   --
   -- If Maximum (Epoch) = Last, and Minimum (Epoch) = First then:
   --
   --    Result := Last - First
   --
   -- Which maximises the function. Then we now that the codomain will be:
   --
   --    Codomain is range 0 .. Last - First

   function Acc_Maximum (
      Item : in Sample_Epoch)
      return Sample_Epoch with
      Ghost    => True,
      Global   => null,
      Post     => (Item (Item'First) = Acc_Maximum'Result (Item'First))
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Maximum'Result (I) =
                        Sample_Type'Max (Item (I),
                                         Acc_Maximum'Result (I - 1)));

   function Acc_Minimum (
      Item : in Sample_Epoch)
      return Sample_Epoch with
      Ghost    => True,
      Global   => null,
      Post     => (Item (Item'First) = Acc_Minimum'Result (Item'First))
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Minimum'Result (I) =
                        Sample_Type'Min (Item (I),
                                         Acc_Minimum'Result (I - 1)));

   function Maximum (
      Item : in Sample_Epoch)
      return Sample_Type with
      Ghost    => True,
      Post     => (for all Sample of Item => Maximum'Result >= Sample)
         and then (for some Sample of Item => Sample = Maximum'Result)
         and then (Maximum'Result = Acc_Maximum (Item) (Item'Last)),
      Global => null;

   function Minimum (
      Item : in Sample_Epoch)
      return Sample_Type with
      Ghost    => True,
      Post     => (for all Sample of Item => Minimum'Result <= Sample)
         and then (for some Sample of Item => Sample = Minimum'Result)
         and then (Minimum'Result = Acc_Minimum (Item) (Item'Last)),
      Global => null;

end Detector.Details.Max_Distance;
