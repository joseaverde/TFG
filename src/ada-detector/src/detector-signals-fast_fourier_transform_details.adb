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
      Input  : in     Boolean) is
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

   procedure Lemma_Power_Of_Two_Module_Another_Lower_Power_Of_Two_Is_Zero (
      Left  : in Natural;
      Right : in Natural) is
      Sizes : constant Chunk_Size_Array := Chunk_Sizes;
   begin
      pragma Assert (2 ** Left = Sizes (Left));
      pragma Assert (2 ** Right = Sizes (Right));
      pragma Assert (2 ** Left >= 2 ** Right);
      pragma Assert (Left >= Right);
      if Left = Right then
         pragma Assert (2 ** Left mod 2 ** Right = 0);
      else
         pragma Assert (Left > Right);
         pragma Assert (
            (for all I in Left + 1 .. Right =>
               Sizes (I) = Sizes (I - 1) * 2
               and then Sizes (I) mod Sizes (I - 1) = 0
               and then Sizes (I) mod 2 ** Left = 0));
         pragma Assert (Sizes (Right) mod 2 ** Left = 0);
         pragma Assert (2 ** Left mod 2 ** Right = 0);
      end if;
   end Lemma_Power_Of_Two_Module_Another_Lower_Power_Of_Two_Is_Zero;

end Detector.Signals.Fast_Fourier_Transform_Details;
