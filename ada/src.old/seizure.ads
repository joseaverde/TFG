generic
   type Real is digits <> or use Float;
   The_Stride_Size : Positive := 256;
package Seizure with Pure, SPARK_Mode => On is

   subtype Sample is Real range -10_000.0 .. 10_000.0;

   type Count_Type is range 0 .. 1_000;
   subtype Extended_Index is Count_Type range 0 .. Count_Type'Last - 1;
   subtype Index_Type is Extended_Index range 1 .. Extended_Index'Last;

   Stride_Size : constant Count_Type := Count_Type (The_Stride_Size);

   type Span_Type is record
      First : Index_Type;
      Last  : Extended_Index;
   end record;

   type Sample_Array is array (Index_Type range <>) of Sample;
   type Signal_Type (Size : Extended_Index) is tagged limited record
      Data : Sample_Array (1 .. Size);
   end record with
   Constant_Indexing => Get;

   function Is_Valid_Span (
      Item : in Signal_Type;
      Span : in Span_Type)
      return Boolean is (
      Span.First in 1 .. Item.Size and then
      Span.Last in 0 .. Item.Size) with
      Ghost;

   function Get (
      Item  : in Signal_Type;
      Span  : in Span_Type;
      Index : in Count_Type)
      return Real is (
      Item.Data (Span.First - 1 + Index)) with
      Pre'Class => Is_Valid_Span (Item, Span)
                     and then Index in 1 .. Size (Span);

   function Size (Item : in Span_Type) return Count_Type is (
      (if Item.Last < Item.First then 0
         else 1 + Item.Last - Item.First));

   function Is_An_Epoch (Span : in Span_Type) return Boolean is (
      Size (Span) = Stride_Size) with Ghost;

   function Make_Span (Item : in Signal_Type) return Span_Type is (
      First => 1,
      Last  => Item.Size);

end Seizure;
