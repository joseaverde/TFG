private with Ada.Numerics.Generic_Elementary_Functions;

generic
   type Real is digits <> or use Float;
   Stride_Samples : Positive := 256;
   Epoch_Strides  : Positive := 5;
package Seizure with Pure, SPARK_Mode => On is

   type Count_Type is range 0 .. 1_000_000_000;
   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;
   subtype Extended_Index is Count_Type range 0 .. Count_Type'Last - 1;
   subtype Index_Type is Extended_Index range 1 .. Extended_Index'Last - 1;

   Stride_Size : constant Count_Type := Count_Type (Stride_Samples);
   Epoch_Size  : constant Count_Type := Count_Type (Epoch_Strides)
                                      * Stride_Size;

   -->> Spans <<--

   type Span_Type is record
      First : Index_Type;
      Last  : Extended_Index;
   end record;

   subtype Stride_Span is Span_Type with
      Dynamic_Predicate => Size (Stride_Span) = Stride_Size,
      Predicate_Failure => raise Constraint_Error with "Not a stride";

   subtype Epoch_Span is Span_Type with
      Dynamic_Predicate => Size (Epoch_Span) = Epoch_Size,
      Predicate_Failure => raise Constraint_Error with "Not an epoch";

   function Size (Item : in Span_Type) return Count_Type is (
      (if Item.Last < Item.First then 0
         else 1 + Item.Last - Item.First));

   -->> Signals <<--

   subtype Sample is Real range -100_000.0 .. 100_000.0;
   type Sample_Array is array (Index_Type range <>) of Sample;

   type Signal (Last : Index_Type) is tagged limited record
      Samples : Sample_Array (1 .. Last);
   end record with
   Constant_Indexing => Get;

   function Is_Valid_Span (
      Item : in Signal;
      Span : in Span_Type)
      return Boolean is (
      Span.First in 1 .. Item.Last and then
      Span.Last in 0 .. Item.Last);

   function Get (
      Item  : in Signal;
      Span  : in Span_Type;
      Index : in Count_Type)
      return Real is (
      Item.Samples (Span.First - 1 + Index)) with
      Pre'Class => Is_Valid_Span (Item, Span)
                     and then Index in 1 .. Size (Span);

   procedure Set (
      Item  : in out Signal;
      Span  : in     Span_Type;
      Index : in     Count_Type;
      Value : in     Sample) with
      Pre'Class => Is_Valid_Span (Item, Span)
                     and then Index in 1 .. Size (Span);

   -->> Batch <<--

   subtype Pattern_Type is Signal (Epoch_Size);
   type Pattern_Array is array (Positive range <>) of Pattern_Type;

   type Bound_Type is record
      Low  : Real;
      High : Real;
   end record;

   type Batch_Type (Patterns : Positive) is limited record
      PSD_1    : Bound_Type;
      PSD_2    : Bound_Type;
      PSD_3    : Bound_Type;
      Energy   : Bound_Type;
      Max_Dist : Bound_Type;
      d_th     : Real;
      Pj       : Pattern_Array (1 .. Patterns);
   end record;

private

   package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Real);

end Seizure;
