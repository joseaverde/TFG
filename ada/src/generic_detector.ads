with Generic_Signals;
pragma Warnings (Off, "unused variable ""Detector.State""",
                 Reason => "It is used, maybe a compiler bug.");
generic
   with package Signals is new Generic_Signals (<>);
package Generic_Detector with
   SPARK_Mode     => On,
   Abstract_State => State
is

   function Invariant return Boolean with Global => (Input => State);

   procedure Reset with Global => (Output => State), Post => Invariant;

   procedure Feed_Stride (
      Process : not null access function return Signals.Sample) with
      Pre    => Invariant,
      Post   => Invariant,
      Global => (In_Out => State);

   function Is_Seizure return Boolean with
      Pre    => Invariant,
      Global => (Input => State);

end Generic_Detector;
