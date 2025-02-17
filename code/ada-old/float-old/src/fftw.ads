private with Ada.Finalization;
with Reals, Signals;

package FFTW with Preelaborate, SPARK_Mode => On is

   type Complex_Type is record
      Img  : Reals.Real;
      Real : Reals.Real;
   end record with Convention => C;

   subtype Epoch_Range is Signals.Count_Type range 1 .. Signals.Epoch_Size;
   type Complex_Array is array (Epoch_Range) of aliased Complex_Type;

   type Plan_Type is limited private with
      Default_Initial_Condition => not Is_Valid (Plan_Type);

   function Is_Valid (Plan : in Plan_Type) return Boolean;

   function Create return Plan_Type with
      Post => Is_Valid (Create'Result);

   procedure Execute (
      Plan      : in out Plan_Type;
      In_Signal : in     Signals.Signal;
      In_Epoch  : in     Signals.Epoch_Span;
      Result    :    out Complex_Array) with
      Pre  => In_Signal.Is_Valid_Span (In_Epoch) and then Is_Valid (Plan),
      Post => Is_Valid (Plan);

private

   pragma SPARK_Mode (Off);

   type Opaque_Plan is null record with Convention => C;
   type Plan_Pointer is access all Opaque_Plan;

   type Real_Array is array (Epoch_Range) of aliased Reals.Real;
   type Complex_Access is access all Complex_Type;
   type Real_Access is access all Reals.Real;

   type Plan_Type is
      new Ada.Finalization.Limited_Controlled with
   record
      Pointer : Plan_Pointer := null;
      Input   : Real_Array;
      Output  : Complex_Array;
   end record;

   overriding procedure Finalize (Object : in out Plan_Type);

   function Is_Valid (Plan : in Plan_Type) return Boolean is (
      Plan.Pointer /= null);

end FFTW;
