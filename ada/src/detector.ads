with Batchs, Signals;
use Batchs, Signals;

package Detector with
   SPARK_Mode     => On,
   Abstract_State => State
is

   subtype Extended_Stride_Index is Count_Type range 0 .. Stride_Size;
   subtype Stride_Index is Extended_Stride_Index
      range 1 .. Extended_Stride_Index'Last;

   function Invariant return Boolean with Global => (Input => State);

   procedure Reset with Global => (Output => State), Post => Invariant;

   procedure Write (
      Signal : in Signals.Signal;
      Stride : in Stride_Span) with
      Pre    => Signal.Is_Valid_Span (Stride) and then Invariant,
      Post   => Invariant,
      Global => (In_Out => State);

   function Is_Seizure (
      Batch : in Batch_Type)
      return Boolean with
      Pre    => Invariant,
      Global => (Input => State);

end Detector;
