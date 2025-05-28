package Detector.Algorithms with SPARK_Mode => On is

   use type Real;

   type Span_Array is array (Positive_Count_Type range <>) of Real_Span;

   PSD_1_Bounds           : constant Real_Span := (2.5,  12.0);
   PSD_2_Bounds           : constant Real_Span := (12.0, 18.0);
   PSD_3_Bounds           : constant Real_Span := (18.0, 35.0);
   PSD_Bounds : constant Span_Array (1 .. 3) := [
      PSD_1_Bounds, PSD_2_Bounds, PSD_3_Bounds];
   PSD_Sampling_Frequency : constant Real      := Real (Stride_Size);

   -- TODO: Make it a parameter
   Welch_Window_Size      : constant := 512;
   Welch_Window_Overlap   : constant := Welch_Window_Size / 2;
   Result_Array_Size      : constant := Welch_Window_Size / 2 + 1;
   Warping_Window         : constant := 16;

   subtype Welch_Array is Real_Array (1 .. Result_Array_Size);
   subtype Epoch_Array is Sample_Array (1 .. Epoch_Size);
   subtype Stride_Array is Sample_Array (1 .. Stride_Size);
   -- TODO: Replace everything with Epoch_Array

   function Simpson (
      Signal : in Real_Array;
      dx     : in Sample)
      return Real with
      Pre => Signal'Length in 1 .. Epoch_Size;

   procedure Welch (
      Signal    : in     Sample_Array;
      Pxx       :    out Welch_Array;
      Overlap   : in     Positive_Count_Type;
      Frequency : in     Real);

   function Energy (
      Signal : in Sample_Array)
      return Real with
      Pre => Signal'Length = Epoch_Size;

   function Max_Distance (
      Signal : in Sample_Array)
      return Real with
      Pre => Signal'Length = Epoch_Size;

   procedure FFT (
      Input  : in     Sample_Array;
      Output :    out Complex_Array) with
      Pre => Input'Length = Output'Length and then Input'Length >= 1;

   function Power_Spectral_Density (
      Signal             : in Epoch_Array;
      Sampling_Frequency : in Sample;
      Low                : in Sample;
      High               : in Sample)
      return Real with
      Pre => Signal'Length = Epoch_Size;

   procedure Power_Spectral_Density (
      Signal             : in Epoch_Array;
      Sampling_Frequency : in Sample;
      Bounds             : in Span_Array;
      Result             : out Real_Array) with
      Pre      => Signal'Length = Epoch_Size
         and then Bounds'First = Result'First
         and then Bounds'Last = Result'Last;

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
