procedure Detector.Signals.Fast_Fourier_Transform (
   Input  : in     Signal_Type;
   Output :    out Complex_Signal;
   Power  : in     Natural;
   Scale  :    out Positive) with
   Global         => null,
   Preelaborate   => True,
   SPARK_Mode     => On,
   Pre            => Input'Length > 0
            -- The Input must be a power of 2 for this algorithm to work.
            and then Power in 0 .. Bits - 2
            and then (for all I in 0 .. Bits - 2 => 2 ** I in Natural)
            and then Input'Length = 2 ** Power
            and then Output'Length = Input'Length,
   Post           => Scale in 1 .. Bits - 2,
   Always_Terminates;
