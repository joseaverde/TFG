with Ada.Unchecked_Deallocation;
with Seizure_Algorithm_Config; use Seizure_Algorithm_Config;
with Reals; use Reals;

package Signals with Preelaborate, SPARK_Mode => On is

   -->> Count Type <<--

   type Count_Type is range 0 .. 1_000_000_000 with Size => 32;
   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;
   subtype Extended_Index is Count_Type range 0 .. Count_Type'Last - 1;
   subtype Index_Type is Extended_Index range 1 .. Extended_Index'Last;

   -->> Constants <<--

   Stride_Size : constant := Samples_Per_Stride;
   Epoch_Size  : constant := Stride_Size * Strides_Per_Epoch;

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

   function Nearest_Multiple (Num, Mult : in Positive)
      return Positive is (
      (if Num rem Mult = 0 then Num
         elsif Num - (Num rem Mult) >= Positive'Last - Mult then Num
         else Num + (Mult - (Num rem Mult)))) with
      Static;
   Maximum_Sample : constant :=
      Real (Nearest_Multiple (100_000, Positive (Epoch_Size)));

   subtype Sample is Real range -Maximum_Sample .. Maximum_Sample;
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

end Signals;
