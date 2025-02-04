with Generic_Sum;

package body Algorithms with SPARK_Mode => On is

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
      for I in Count_Type range 2 .. Epoch_Size loop
         Min := Sample'Min (Min, Signal (Epoch, I));
         Max := Sample'Max (Max, Signal (Epoch, I));
         pragma Loop_Invariant (Min <= Max);
      end loop;
      return Real (Max) - Real (Min);
   end Max_Dist;

   type Real_Array is array (Index_Type range <>) of Real;

   subtype Mean_Real is Real
      range Sample'First / Real (Signals.Epoch_Size)
         .. Sample'Last / Real (Signals.Epoch_Size);

   subtype Energy_Real is Real
      range 0.0
         .. (Sample'Last - Sample'First) ** 2 / Real (Epoch_Size);

   package Mean_Sum is new Generic_Sum (
      Size       => Positive (Signals.Epoch_Size),
      Index_Type => Index_Type,
      Real_Array => Real_Array,
      First      => Mean_Real'First,
      Last       => Mean_Real'Last);

   package Energy_Sum is new Generic_Sum (
      Size       => Positive (Signals.Epoch_Size),
      Index_Type => Index_Type,
      Real_Array => Real_Array,
      First      => Energy_Real'First,
      Last       => Energy_Real'Last);
   -- The operation is: (Σ (x - mean)²) / epoch_size
   --
   -- The maximum value is when: x = Last, mean = First.
   -- The minimum value is when x = mean. Which is 0.
   --
   -- Then the safe range of operations is:
   -- 0 .. (Sample'First + Sample'Last) ** 2 / Epoch_Size

   function Energy (
      Signal : in Signals.Signal;
      Epoch  : in Epoch_Span)
      return Real is
      Temp : Real_Array (1 .. Signals.Epoch_Size);
      Mean : constant Sample := Mean_Sum.Sum (
         [for I in 1 .. Signals.Epoch_Size =>
            Signal (Epoch, I) / Real (Signals.Epoch_Size)]);
      function Step (X, Mean : in Sample) return Real with
         Post => Step'Result in Energy_Sum.Input_Real;
      function Step (X, Mean : in Sample) return Real is
         Result : constant Real := (X - Mean) ** 2 / Real (Signals.Epoch_Size);
      begin
         pragma Assume (Result in Energy_Sum.Input_Real);
         return Result;
      end Step;
   begin
      pragma Assume (
         (for all I in Count_Type range 1 .. Signals.Epoch_Size =>
            Step (Signal (Epoch, I), Mean) in Energy_Sum.Input_Real));
      return Energy_Sum.Sum (
         [for I in Count_Type range 1 .. Signals.Epoch_Size =>
            Step (Signal (Epoch, I), Mean)]);
   end Energy;

end Algorithms;
