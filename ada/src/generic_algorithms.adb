with Generic_Real_Accumulation;

package body Generic_Algorithms with SPARK_Mode => On is

-- function Simpson (
--    y     : in Signal;
--    Epoch : in Epoch_Span;
--    dx    : in Real)
--    return Real is (
--    0.0);

   function Max_Dist (
      Signal : in Signals.Signal;
      Epoch  : in Epoch_Span)
      return Real is
      Min : Sample := Signal (Epoch, 1);
      Max : Sample := Signal (Epoch, 1);
   begin
      pragma Assert (Min <= Max);
      for I in 2 .. Epoch_Size loop
         Min := Sample'Min (Min, Signal (Epoch, I));
         Max := Sample'Max (Max, Signal (Epoch, I));
         pragma Loop_Invariant (Min <= Max);
      end loop;
      return Real (Max) - Real (Min);
   end Max_Dist;

-- function Energy (
--    Signal : in Signals.Signal;
--    Epoch  : in Epoch_Span)
--    return Real is
--    pragma Annotate (
--       GNATprove,
--       False_Positive,
--       """Acc"" might not be initialized",
--       "Acc (1) is initialised, and we index Acc (I - 1), with I >= 2");
--    use type Types.Count_Type;
--    subtype Mini_Sample is Sample
--       range Sample'First / Real (Epoch_Size)
--          .. Sample'Last / Real (Epoch_Size);
--    Acc  : array (1 .. Epoch_Size) of Sample;
--    Temp : Mini_Sample;
--    Mean : Sample;
-- begin
--    -- Compute the mean
--    Acc (1) := Signal (Epoch, 1) / Real (Epoch_Size);
--    pragma Assert (Acc (1) in Mini_Sample);
--    for I in 2 .. Epoch_Size loop
--       Temp := Signal (Epoch, I) / Real (Epoch_Size);
--       pragma Assert (
--          if Acc (I - 1) in Mini_Sample'First * Real (I)
--                         .. Mini_Sample'Last * Real (I)
--             then Acc (I - 1) + Temp
--                in Mini_Sample'First * Real (I) + Mini_Sample'First
--                .. Mini_Sample'Last * Real (I) + Mini_Sample'Last);
--       pragma Assert (Mini_Sample'First * Real (Epoch_Size) >= Sample'First);
--       pragma Assert (Mini_Sample'Last * Real (Epoch_Size) <= Sample'Last);
--       pragma Loop_Invariant (
--          Acc (I - 1) in Mini_Sample'First * Real (I)
--                      .. Mini_Sample'Last * Real (I));
--       pragma Loop_Invariant (
--          Acc (I - 1) + Temp
--             in Mini_Sample'First * Real (I) + Mini_Sample'First
--             .. Mini_Sample'Last * Real (I) + Mini_Sample'Last);
--       Acc (I) := Acc (I - 1) + Temp;
--    end loop;
--    Mean := Acc (Acc'Last);
--    -- Compute the energy
--    return Mean;
-- end Energy;

   function Mean (
      Signal : in Signals.Signal;
      Epoch  : in Epoch_Span)
      return Sample is
      subtype Mini_Sample is Sample
         range Sample'First / Real (Epoch_Size)
            .. Sample'Last / Real (Epoch_Size);
      subtype Epoch_Index is Types.Count_Type range 1 .. Epoch_Size;
      type Mini_Sample_Array is array (Epoch_Index range <>) of Mini_Sample;
      package Accumulation is new Generic_Real_Accumulation (
         Index_Type  => Epoch_Index,
         Input_Real  => Mini_Sample,
         Input_Array => Mini_Sample_Array,
         Output_Real => Real);
   begin
      return 0.0;
   end Mean;

end Generic_Algorithms;
