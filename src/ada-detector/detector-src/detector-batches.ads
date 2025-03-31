with Detector.Signals;
with Detector.Signals.Max_Distance;
with Detector.Signals.Generic_Energy;

generic
   type Sample_Type is delta <>;
   type Feature_Type is delta <>;
   Samples_Per_Stride : in Positive_Count_Type;
   Strides_Per_Epoch  : in Positive_Count_Type;
   Max_Patterns       : in Positive_Count_Type := 5;
package Detector.Batches with Pure, SPARK_Mode is

   Epoch_Size : constant Positive_Count_Type :=
      Strides_Per_Epoch * Samples_Per_Stride;

   -- Feature types with valid ranges

   type Span_Type is record
      Low, High : Feature_Type;
   end record;

   function Is_In (Item : in Feature_Type; Span : in Span_Type)
      return Boolean is (
      Item >= Span.Low and then Item <= Span.High);

   type Sample_Array is array (Positive_Count_Type range <>) of Sample_Type;
   subtype Epoch_Type is Sample_Array (1 .. Epoch_Size);
   subtype Pattern_Type is Epoch_Type;
   subtype Pattern_Count is Positive_Count_Type range 1 .. Max_Patterns;
   type Pattern_Array is array (Pattern_Count range <>) of Pattern_Type;

   type Batch_Type is limited private;

   function Make_Batch (
      PSD_1, PSD_2, PSD_3, Max_Dist, Energy, DTW : in Span_Type;
      Patterns                                   : in Pattern_Array)
      return Batch_Type with
      Global => null,
      Inline => True,
      Pre    => Patterns'Length > 0;

   procedure Is_Seizure (
      Batch  : in out Batch_Type with Unreferenced;
      Result :    out Boolean) with
      Global => null,
      Always_Terminates;

   -->> Instantiation <<--

   package Normalisation is
      new Detector.Signals.Generic_Normalisation (
      Scaled_Sample_Type => Sample_Type,
      Minimum_Value      => Sample_Type'First,
      Maximum_Value      => Sample_Type'Last);

   function Max_Distance is
      new Detector.Signals.Max_Distance.Generic_Max_Distance (
      Result_Type   => Feature_Type,
      Normalisation => Normalisation);

   function Energy is
      new Detector.Signals.Generic_Energy (
      Normalisation => Normalisation,
      Result_Type   => Feature_Type);

private

   -- TODO: Store the patterns normalised

   type Batch_Type (Count : Pattern_Count := 1) is limited record
      PSD_1, PSD_2, PSD_3, Max_Dist, Energy : Span_Type;
      d_max_c                               : Span_Type;
      Patterns                              : Pattern_Array (1 .. Count);
   end record;

   function Make_Batch (
      PSD_1, PSD_2, PSD_3, Max_Dist, Energy, DTW : in Span_Type;
      Patterns                                   : in Pattern_Array)
      return Batch_Type is (
      Count    => Patterns'Length,
      PSD_1    => PSD_1,
      PSD_2    => PSD_2,
      PSD_3    => PSD_3,
      Max_Dist => Max_Dist,
      Energy   => Energy,
      d_max_c  => DTW,
      Patterns => Patterns);

end Detector.Batches;
