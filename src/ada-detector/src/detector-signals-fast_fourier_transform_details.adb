package body Detector.Signals.Fast_Fourier_Transform_Details with SPARK_Mode is

-- procedure Conquer (
--    Buffer : in out Double_Buffer;
--    Scale  : in out Natural;
--    Chunk  : in     Positive_Count_Type;
--    Input  : in     Boolean;
--    Bound  : in     Sample_Type;
--    First  : in     Count_Type := 0) is
-- begin
--    null;
-- end Conquer;

   procedure Conquer (
      Buffer : in out Double_Buffer;
      Scaled :    out Boolean;
      Chunk  : in     Positive_Count_Type;
      Input  : in     Boolean;
      Power  : in     Natural) is
      pragma SPARK_Mode (Off);
   begin
      null;
   end Conquer;

   function Chunk_Sizes return Chunk_Size_Array is
      Result : Chunk_Size_Array := [others => 1];
   begin
      for Index in 1 .. Bits - 2 loop
         pragma Loop_Invariant (Result (0) = 1);
         pragma Loop_Invariant (Result (0) = 2 ** 0);
         pragma Loop_Invariant (
            (for all I in 1 .. Index - 1 =>
               Result (I) = Result (I - 1) * 2));
         Result (Index) := Result (Index - 1) * 2;
      end loop;
      pragma Assert (Result (0) = 1);
      pragma Assert (
         (for all I in 1 .. Bits - 2 =>
            Result (I) = Result (I - 1) * 2));              -- B
      pragma Assert (Result (0) = 2 ** 0);                  -- A(0)
      pragma Assert (
         (for all I in 1 .. Bits - 2 =>
            2 ** (I - 1) * 2 = 2 ** I));                    -- C
      pragma Assert (
         (for all I in 1 .. Bits - 2 =>
            (if Result (I - 1) = 2 ** (I - 1)               -- A
               and then Result (I) = Result (I - 1) * 2     -- B
               and then 2 ** (I - 1) * 2 = 2 ** I           -- C
               then Result (I) = 2 ** I)));
      pragma Assert (
         (for all I in 1 .. Bits - 2 => Result (I) = 2 ** I));
      return Result;
   end Chunk_Sizes;

end Detector.Signals.Fast_Fourier_Transform_Details;
