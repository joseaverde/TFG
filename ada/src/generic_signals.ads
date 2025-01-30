with Ada.Unchecked_Deallocation;
with Types; use Types;

generic
   type Real is digits <> or use Float;
   Stride_Samples : Positive_Count_Type := 256;
   Epoch_Strides  : Positive_Count_Type := 5;
package Generic_Signals with Preelaborate, SPARK_Mode => On is

   Stride_Size : constant Positive_Count_Type := Stride_Samples;
   Epoch_Size  : constant Positive_Count_Type := Stride_Size * Epoch_Strides;

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
   type Sample_Array is array (Index_Type range <>) of Sample with
      Default_Component_Value => 0.0;

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

   function Full_Span (
      Item : in Signal)
      return Span_Type is (
      First => 1,
      Last  => Item.Last) with
      Post'Class => Is_Valid_Span (Item, Full_Span'Result);

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
                     and then Index in 1 .. Size (Span),
      Post'Class => Is_Valid_Span (Item, Span);

   type Signal_Access is access Signal;

   procedure Free is new Ada.Unchecked_Deallocation (Signal, Signal_Access);

end Generic_Signals;
