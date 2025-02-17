package Detector.Algorithms with SPARK_Mode => On is

   PSD_1_Bounds           : constant Real_Span := (2.0,  12.0);
   PSD_2_Bounds           : constant Real_Span := (12.0, 18.0);
   PSD_3_Bounds           : constant Real_Span := (18.0, 35.0);
   PSD_Sampling_Frequency : constant Real      := Real (Stride_Size);

   function Energy (
      Signal : in Sample_Array)
      return Real with
      Pre => Signal'Length = Epoch_Size;

   function Max_Distance (
      Signal : in Sample_Array)
      return Real with
      Pre => Signal'Length = Epoch_Size;

   function Power_Spectral_Density (
      Signal             : in Sample_Array;
      Sampling_Frequency : in Sample;
      Low                : in Sample;
      High               : in Sample)
      return Real with
      Pre => Signal'Length = Epoch_Size;

   function Dynamic_Time_Warping (
      Signal  : in Sample_Array;
      Pattern : in Sample_Array;
      Maximum : in Real)
      return Real with
      Pre => Signal'Length = Epoch_Size;

   function Is_Seizure (
      Signal : in Sample_Array;
      Batch  : in Batch_Type)
      return Boolean with
      Pre => Signal'Length = Epoch_Size;

   function Within (
      Item : in Real;
      Span : in Real_Span)
      return Boolean is (
      Span.Low <= Item and Item <= Span.High);

end Detector.Algorithms;
