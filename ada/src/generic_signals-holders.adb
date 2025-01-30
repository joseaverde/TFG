with Ada.Unchecked_Deallocation;

package body Generic_Signals.Holders with SPARK_Mode => Off is

   procedure Clear (
      Holder : in out Signal_Holder) is
      procedure Free is
         new Ada.Unchecked_Deallocation (
         Object => Signal,
         Name   => Signal_Access);
   begin
      if Holder.Data /= null then
         Free (Holder.Data);
      end if;
   end Clear;

   procedure Create (
      Holder :    out Signal_Holder;
      Last   : in     Index_Type) is
   begin
      Clear (Holder);
      Holder.Data := new Signal'(Last => Last, Samples => [others => 0.0]);
   end Create;

   overriding
   procedure Finalize (
      Object : in out Signal_Holder) is
   begin
      Clear (Object);
   end Finalize;

   procedure Update (
      Holder  : in out Signal_Holder;
      Process : not null access procedure (Item : in out Signal)) is
   begin
      Process.all (Holder.Data.all);
   end Update;

end Generic_Signals.Holders;
