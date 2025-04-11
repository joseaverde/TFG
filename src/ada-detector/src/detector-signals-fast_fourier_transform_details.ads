private package Detector.Signals.Fast_Fourier_Transform_Details with
   Preelaborate, SPARK_Mode => On
is

   type Double_Buffer is
      array (Boolean range <>, Count_Type range <>) of Complex;

-- procedure Conquer_Part (
--    Buffer : in out Double_Buffer;
--    Scale  : in out Natural;
--    Chunk  : in     Positive_Count_Type;
--    Input  : in     Boolean;
--    Bound  : in     Sample_Type;
--    First

-- procedure Conquer (
--    Buffer : in out Double_Buffer;
--    Scale  : in out Natural;
--    Chunk  : in     Positive_Count_Type;
--    Input  : in     Boolean;
--    Bound  : in     Sample_Type;
--    First  : in     Count_Type := 0) with
--    Global   => null,
--    Pre      => Bound > 0.0
--       and then First in Buffer'Range (2)
--       -- The chunk must be a power of 2.
--       and then Chunk < Buffer'Length (2)
--       and then Buffer'Length (2) mod Chunk = 0
--       -- All the elements must be within the given range.
--       and then (for all B in Buffer'Range (1) =>
--                   (for all I in Buffer'Range (2) =>
--                               Buffer (B, I).Re in -Bound .. Bound
--                      and then Buffer (B, I).Im in -Bound .. Bound)),
--    Post     => (if Scale = Natural'Last then Scale = Scale
--                   else Scale in Scale'Old .. Scale'Old + 1);

   procedure Conquer (
      Buffer : in out Double_Buffer;
      Scaled :    out Boolean;
      Chunk  : in     Positive_Count_Type;
      Input  : in     Boolean) with
      Global   => null,
      Inline   => True,
      Pre      => Buffer'Length (2) > 0
         and then Buffer'Length (2) <= 2 ** (Bits - 2)
         and then (for some Power in 0 .. Bits - 2 =>
                     Buffer'Length (2) = 2 ** Power)
         and then Buffer'Length (2) mod Chunk = 0,
      Always_Terminates;

   type Chunk_Size_Array is array (0 .. Bits - 2)
      of Positive_Count_Type;

   function Chunk_Sizes return Chunk_Size_Array with
      Ghost    => True,
      Global   => null,
      Post     => Chunk_Sizes'Result (0) = 1
         and then (for all I in 1 .. Bits - 2 =>
                     Chunk_Sizes'Result (I) = Chunk_Sizes'Result (I - 1) * 2)
         and then Chunk_Sizes'Result (0) = 2 ** 0
         and then (for all I in 1 .. Bits - 2 =>
                     Chunk_Sizes'Result (I) = 2 ** I);
   -- Note: This function requires level=3 to be proven. It is not that trivial

   procedure Lemma_Power_Of_Two_Module_Another_Lower_Power_Of_Two_Is_Zero (
      Left  : in Natural;
      Right : in Natural) with
      Ghost    => True,
      Global   => null,
      Pre      => Left in 0 .. Bits - 2
         and then Right in 0 .. Bits - 2
         and then Left >= Right,
      Post     => 2 ** Left mod 2 ** Right = 0;

end Detector.Signals.Fast_Fourier_Transform_Details;
