private with Ada.Finalization;

generic
package Generic_Signals.Holders with Preelaborate, SPARK_Mode => On is

   type Signal_Holder is limited private with
      Default_Initial_Condition => Is_Empty (Signal_Holder),
      Preelaborable_Initialization;

   procedure Clear (
      Holder : in out Signal_Holder) with
      Post   => Is_Empty (Holder),
      Global => null;

   procedure Create (
      Holder :    out Signal_Holder;
      Last   : in     Index_Type) with
      Post   => not Is_Empty (Holder),
      Global => null;

   function Get (
      Item  : in Signal_Holder;
      Span  : in Span_Type;
      Index : in Count_Type)
      return Real with
      Pre => Is_Valid_Span (Item, Span) and then Index in 1 .. Size (Span);

   function Is_Empty (
      Item : in Signal_Holder)
      return Boolean with
      Global => null;

   function Is_Valid_Span (
      Item : in Signal_Holder;
      Span : in Span_Type)
      return Boolean with
      Global => null;

   function Last (
      Holder : in Signal_Holder)
      return Index_Type with
      Pre    => not Is_Empty (Holder),
      Global => null;

   procedure Update (
      Holder  : in out Signal_Holder;
      Process : not null access procedure (Item : in out Signal)) with
      Pre    => not Is_Empty (Holder),
      Post   => not Is_Empty (Holder),
      Global => null;

private

   pragma SPARK_Mode (Off);

   type Signal_Access is access Signal;

   type Signal_Holder is
      new Ada.Finalization.Limited_Controlled with
   record
      Data : Signal_Access := null;
   end record;

   overriding
   procedure Finalize (
      Object : in out Signal_Holder);

   function Get (
      Item  : in Signal_Holder;
      Span  : in Span_Type;
      Index : in Count_Type)
      return Real is (
      Item.Data.Get (Span, Index));

   function Is_Empty (
      Item : in Signal_Holder)
      return Boolean is (
      Item.Data = null);

   function Is_Valid_Span (
      Item : in Signal_Holder;
      Span : in Span_Type)
      return Boolean is (
      Span.First in 1 .. Item.Data.Last and then
      Span.Last in 0 .. Item.Data.Last);

   function Last (
      Holder : in Signal_Holder)
      return Index_Type is (
      Holder.Data.Last);

end Generic_Signals.Holders;
