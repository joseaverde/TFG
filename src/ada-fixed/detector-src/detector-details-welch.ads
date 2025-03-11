package Detector.Details.Welch with SPARK_Mode => On is

   subtype Welch_Array is Feature_Array (1 .. Welch_Size);

   -- ----------------------- A S S U M P T I O N S ----------------------- --

   -- In Welch we have a window which has size Welch_Size and certain overlap.
   -- Such overlap is usually user defined and is lower than the window size.
   -- But people usually use half the window size. And this study also uses
   -- that value. To make the proof easier we are going to suppose the overlap
   -- will be half the window size:
   -- Note: Welch_Size MUST be a power of two as per assertion in Detector
   --       package. Therefore Overlap is also a power of two. This allows the
   --       compiler to make better optimisations (using a shift instead of
   --       the slow division).

   Overlap : constant := Welch_Size / 2;

   -- The frequency is also constant. The frequency is the Stride_Size, the
   -- amount of samples we read per second. Therefore, to simplify the proof
   -- and improve the algorithm, we are going to suppose the frequency is also
   -- constant.

   Frequency : constant := Stride_Size;

   -- We have another big problem which is that we need to compute the norm
   -- squared of the values returned by the fourier transform. The
   -- postcondition is yet to be proved for the `Fourier_Transform' function,
   -- but let's suppose the worst case:
   --
   --    Fourier_Transform (Input, Output);
   --    Assert ((for all X of Output in Complex_Part));
   --
   -- Which means that all the values from `Output' cover the whole
   -- `Complex_Part' range. If we can prove it for the worst case, proving it
   -- for a smaller range is trivial. The problem is:
   --
   --    Norm_Squared (C) = Re (C) ** 2 + Im (C) ** 2
   --
   -- For such an operation we need 65 bits: 64 bits to store the square, 1 bit
   -- for the addition. However, we have good news, every Norm_Squared must be
   -- multiplied by `Factor':
   --
   --    Factor = 2.0 / (Normalisation_Factor * Frequency * Steps)
   --
   -- Where:

   pragma Assert (Epoch_Size > Welch_Size);
   Steps : constant := (Epoch_Size - Welch_Size) / Overlap + 1;

   -- And as we will be proving later: Normalisation_Factor is a number:
   --
   --    Normalisation_Factor ∈ [0 .. Welch_Size]
   --
   -- As for the Norm_Squared we can divide by:
   --
   --    Frequency / 2

   pragma Assert (Frequency > 1);
   Norm_Squared_Scale : constant := Frequency / 2;

   -- Let's suppose that we are using 65 bits for the output Norm_Squared.
   -- The loop is executed `Steps' times. Therefore the number in `Pxx (I)' is
   -- 65 bit number multiplied by `Steps'. This value needs:
   -- 
   --    65 + Ceiling (Log_2 (Steps)) bits
   --
   -- To store it. At the end we will need to divide by `Frequency / 2', by
   -- the number of `Steps' and by the `Normalisation_Factor'. The
   -- `Normalisation_Factor' is known to be constant and (although I'm not able
   -- to prove it as of now) is greater than 1. Finally the result is to be
   -- truncated to 32 bits (`Feature_Type'). If it overflow we have to show it,
   -- but there is no problem because we have to check whether the given PSD
   -- is to be within certain range, it it isn't, bad luck.
   --

   procedure Welch (
      Signal : in     Sample_Array;
      Pxx    :    out Welch_Array) with
      Always_Terminates;

end Detector.Details.Welch;
