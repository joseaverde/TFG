with Generic_Signals, Types;
pragma Warnings (Off, "unused variable ""Detector.State""",
                 Reason => "It is used, maybe a compiler bug.");
generic
   with package Signals is new Generic_Signals (<>);
package Generic_Detector with
   SPARK_Mode     => On,
   Abstract_State => State
is

   subtype Extended_Stride_Index is Types.Count_Type
      range 0 .. Signals.Stride_Size;
   subtype Stride_Index is Extended_Stride_Index
      range 1 .. Extended_Stride_Index'Last;

   function Invariant return Boolean with Global => (Input => State);

   procedure Reset with Global => (Output => State), Post => Invariant;

   procedure Write (
      Signal : in Signals.Signal;
      Stride : in Signals.Stride_Span) with
      Pre    => Signal.Is_Valid_Span (Stride) and then Invariant,
      Post   => Invariant,
      Global => (In_Out => State);

   function Is_Seizure return Boolean with
      Pre    => Invariant,
      Global => (Input => State);

end Generic_Detector;
