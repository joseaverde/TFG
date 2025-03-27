package body Detector.Signals.Max_Distance with SPARK_Mode is

   function Generic_Max_Distance (
      Item : in Signal_Type)
      return Result_Type is
      use all type Normalisation.Scaled_Sample_Type;
      Min : Sample_Type := Item (Item'First);
      Max : Sample_Type := Item (Item'First);
   begin
      pragma Assert (Max >= Min);
      for I in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Max >= Min);
         Min := Sample_Type'Min (Min, Item (I));
         Max := Sample_Type'Max (Max, Item (I));
      end loop;
      pragma Assert (Max >= Min);
      return
         (if Min >= 0.0 or else Max < 0.0
            then Result_Type (Normalisation.Denormalise (Max - Min))
            else Result_Type (Normalisation.Denormalise (Max))
                  + Result_Type (-Normalisation.Denormalise (Min)));
   end Generic_Max_Distance;

end Detector.Signals.Max_Distance;
