--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-batches-validator.ads                                 |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

generic
   Cores      : in Positive            := 1;
   Chunk_Size : in Positive_Count_Type := 100;
   Exclusion  : in Positive_Count_Type := 10;
package Detector.Batches.Validator with SPARK_Mode => On is

   pragma Unreferenced (Cores);

   type Span_Type is record
      First : Positive_Count_Type;
      Last  : Positive_Count_Type;
   end record;

   type Span_Array is array (Positive_Count_Type range <>) of Span_Type;
   subtype Metric_Count_Type is Count_Type range 0 .. Count_Type'Last / 4;

   type Quality_Metrics is record
      True_Positives  : Metric_Count_Type := 0;
      True_Negatives  : Metric_Count_Type := 0;
      False_Positives : Metric_Count_Type := 0;
      False_Negatives : Metric_Count_Type := 0;
   end record;

   Score_Bits          : constant := Bits;
   Score_Whole_Bits    : constant := 2;
   Score_Fraction_Bits : constant := Score_Bits - Score_Whole_Bits - 1;
   Score_Delta         : constant := 2.0 ** (-Score_Fraction_Bits);
   type Score_Type is delta Score_Delta range 0.0 .. 1.0 with
      Size => Score_Bits;

   function Precision   (Item : in Quality_Metrics) return Score_Type;
   function Sensitivity (Item : in Quality_Metrics) return Score_Type;
   function F1_Score    (Item : in Quality_Metrics) return Score_Type;

   function Is_Ordered (Item : in Span_Array)
      return Boolean is (
      (for all I in Item'First .. Item'Last - 1
         =>       Item (I).First <= Item (I).Last
         and then Item (I).Last < Item (I + 1).First - 1)
      and then Item (Item'Last).First <= Item (Item'Last).Last) with
      Ghost  => True,
      Pre    => Item'Length > 0,
      Global => null;

   procedure Validate (
      Signal   : in     Sample_Array;
      Batch    : in     Batch_Type;
      Seizures : in     Span_Array;
      Quality  :    out Quality_Metrics) with
      Pre      => Seizures'Length > 0 and then Is_Ordered (Seizures)
         and then Signal'Length >= Epoch_Size,
      Always_Terminates;

private

   function Precision (Item : in Quality_Metrics) return Score_Type is (
      (if Item.True_Positives = 0 and then Item.False_Positives = 0 then 1.0
       else Fixed_Integer (Item.True_Positives)
            / Fixed_Integer (Item.True_Positives + Item.False_Positives)));

   function Sensitivity (Item : in Quality_Metrics) return Score_Type is (
      (if Item.True_Positives = 0 and then Item.False_Negatives = 0 then 1.0
       else Fixed_Integer (Item.True_Positives)
            / Fixed_Integer (Item.True_Positives + Item.False_Negatives)));

   function F1_Score (Item : in Quality_Metrics) return Score_Type is (
      (if Item.True_Positives = 0 and then Item.False_Negatives = 0
          and then Item.False_Positives = 0 then 1.0
       else Fixed_Integer (2 * Item.True_Positives)
            / Fixed_Integer (2 * Item.True_Positives
                             + Item.False_Negatives + Item.False_Positives)));

end Detector.Batches.Validator;
