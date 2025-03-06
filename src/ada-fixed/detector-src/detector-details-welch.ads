package Detector.Details.Welch with SPARK_Mode => On is

   subtype Welch_Array is Feature_Array (1 .. Welch_Size);

   procedure Welch (
      Signal    : in     Sample_Array;
      Pxx       :    out Welch_Array;
      Overlap   : in     Positive_Count_Type;
      Frequency : in     Feature_Type) with
      Always_Terminates;

end Detector.Details.Welch;
