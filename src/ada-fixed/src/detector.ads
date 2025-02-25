package Detector with SPARK_Mode => On is

   Bits : constant := 32;

   Stride_Size : constant := 256;
   Epoch_Size  : constant := 1280;

   type Count_Type is range 0 .. 1_000_000;
   subtype Index_Type is Count_Type range 1 .. Count_Type'Last;
   type Sample_Type is delta 2.0 ** (-4) range -10_000.0 .. 10_000.0;
   type Sample_Array is array (Index_Type range <>) of Sample_Type;
   subtype Sample_Epoch is Sample_Array (1 .. Epoch_Size);

   type Feature_Type is delta 2.0 ** (-10) range -20_000.0 .. 20_000.0;

   -->> Max distance <<--

   function Acc_Maximum (
      Item : in Sample_Epoch)
      return Sample_Epoch with
      Ghost,
      Global   => null,
      Post     => (Item (Item'First) = Acc_Maximum'Result (Item'First))
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Maximum'Result (I) =
                        Sample_Type'Max (Item (I),
                                         Acc_Maximum'Result (I - 1)));

   function Acc_Minimum (
      Item : in Sample_Epoch)
      return Sample_Epoch with
      Ghost,
      Global   => null,
      Post     => (Item (Item'First) = Acc_Minimum'Result (Item'First))
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Minimum'Result (I) =
                        Sample_Type'Min (Item (I),
                                         Acc_Minimum'Result (I - 1)));

   function Maximum (
      Item : in Sample_Epoch)
      return Sample_Type with
      Ghost,
      Post     => (for all Sample of Item => Maximum'Result >= Sample)
         and then (for some Sample of Item => Sample = Maximum'Result)
         and then (Maximum'Result = Acc_Maximum (Item) (Item'Last)),
      Global => null;

   function Minimum (
      Item : in Sample_Epoch)
      return Sample_Type with
      Ghost,
      Post     => (for all Sample of Item => Minimum'Result <= Sample)
         and then (for some Sample of Item => Sample = Minimum'Result)
         and then (Minimum'Result = Acc_Minimum (Item) (Item'Last)),
      Global => null;

   function Max_Distance (
      Item : in Sample_Epoch)
      return Feature_Type with
      Global   => null,
      Post     => Max_Distance'Result =
                     Feature_Type (Maximum (Item))
                     - Feature_Type (Minimum (Item))
         and then Max_Distance'Result in 0.0
                                      .. Feature_Type (Sample_Type'Last)
                                         - Feature_Type (Sample_Type'First);

end Detector;
