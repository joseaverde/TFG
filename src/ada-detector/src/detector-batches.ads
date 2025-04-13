--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-batches.ads                                           |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals;
with Detector.Signals.Batch_Normalisation;
with Detector.Signals.Generic_Dynamic_Time_Warping;
with Detector.Signals.Generic_Energy;
with Detector.Signals.Generic_Simpson;
with Detector.Signals.Generic_Max_Distance;

generic
   type Sample_Type is delta <>;
   type Feature_Type is delta <>;
   Samples_Per_Stride : in Positive_Count_Type;
   Strides_Per_Epoch  : in Positive_Count_Type;
   Welch_Window_Size  : in Positive_Count_Type;
   Max_Patterns       : in Positive_Count_Type := 5;
package Detector.Batches with Preelaborate, SPARK_Mode is

   Warping_Window : constant := 16;
   -- TODO: Make it a parameter of the generic

   Stride_Size : constant Positive_Count_Type := Samples_Per_Stride;
   Epoch_Size  : constant Positive_Count_Type :=
      Strides_Per_Epoch * Samples_Per_Stride;

   -- Feature types with valid ranges

   type Span_Type is record
      Low, High : Feature_Type;
   end record;

   function Is_In (Item : in Feature_Type; Span : in Span_Type)
      return Boolean is (
      Item >= Span.Low and then Item <= Span.High);

   type Feature_Array is array (Positive_Count_Type range <>) of Feature_Type;
   subtype Pattern_Count is Positive_Count_Type range 1 .. Max_Patterns;
   subtype Pattern_Type is
      Signals.Batch_Normalisation.Normalised_Signal (1 ..  Epoch_Size);
   type Pattern_Array is array (Pattern_Count range <>) of Pattern_Type;

   type Sample_Array is array (Positive_Count_Type range <>) of Sample_Type;
   subtype Epoch_Type is Sample_Array (1 .. Epoch_Size);
   subtype Stride_Type is Sample_Array (1 .. Stride_Size);
   type Epoch_Array is array (Pattern_Count range <>) of Epoch_Type;

   type Batch_Type is limited private;

   function Make_Batch (
      PSD_1, PSD_2, PSD_3, Max_Dist, Energy, DTW : in Span_Type;
      Patterns                                   : in Epoch_Array)
      return Batch_Type with
      Global => null,
      Inline => True,
      Pre    => Patterns'Length > 0;

   function Copy (
      Item : in Batch_Type)
      return Batch_Type with
      Global => null,
      Inline => True;

   procedure Is_Seizure (
      Batch  : in out Batch_Type;
      Epoch  : in     Epoch_Type;
      Result :    out Boolean) with
      Global => null,
      Always_Terminates;

   -->> Instantiation <<--

   package Normalisation is
      new Detector.Signals.Generic_Normalisation (
      Scaled_Sample_Type => Sample_Type,
      Minimum_Value      => Sample_Type'First,
      Maximum_Value      => Sample_Type'Last);

   procedure Normalise (
      Input  : in     Epoch_Type;
      Output :    out Detector.Signals.Signal_Type) with
      Global => null,
      Pre    => Input'Length = Output'Length;

   procedure Normalise (
      Input  : in     Detector.Signals.Signal_Type;
      Output :    out Pattern_Type) with
      Global => null,
      Pre    => Input'Length = Output'Length;

   procedure Normalise_Epochs (
      Input  : in     Epoch_Array;
      Output :    out Pattern_Array) with
      Pre    => Input'Length = Output'Length,
      Global => null;

   function Max_Distance is
      new Detector.Signals.Generic_Max_Distance (
      Result_Type   => Feature_Type,
      Normalisation => Normalisation);

   function Dynamic_Time_Warping is
      new Detector.Signals.Generic_Dynamic_Time_Warping (
      Result_Type => Feature_Type);

   function Energy is
      new Detector.Signals.Generic_Energy (
      Normalisation => Normalisation,
      Result_Type   => Feature_Type);

   function Simpson is
      new Detector.Signals.Generic_Simpson (
      Result_Type => Feature_Type);

   -- TODO: Make it generic with as many ranges as possible
   procedure Power_Spectral_Densities (
      Signal : in     Signals.Signal_Type;
      PSD_1  :    out Feature_Type;
      PSD_2  :    out Feature_Type;
      PSD_3  :    out Feature_Type) with
      Global => null,
      Pre    => Signal'Length = Epoch_Size,
      Always_Terminates;

private

   -- TODO: Store the patterns normalised

   type Batch_Type (Count : Pattern_Count := 1) is limited record
      PSD_1, PSD_2, PSD_3, Max_Dist, Energy : Span_Type;
      d_max_c                               : Span_Type;
      Patterns                              : Pattern_Array (1 .. Count);

      Lookback                              : Count_Type := 0;
      Was_Seizure                           : Boolean    := False;
      Streak                                : Count_Type := 0;
   end record;

   function Copy (
      Item : in Batch_Type)
      return Batch_Type is (
      Count       => Item.Count,
      PSD_1       => Item.PSD_1,
      PSD_2       => Item.PSD_2,
      PSD_3       => Item.PSD_3,
      Max_Dist    => Item.Max_Dist,
      Energy      => Item.Energy,
      d_max_c     => Item.d_max_c,
      Patterns    => Item.Patterns,
      Lookback    => Item.Lookback,
      Was_Seizure => False,
      Streak      => 0);

end Detector.Batches;
