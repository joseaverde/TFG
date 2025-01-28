generic
package Seizure.Detector with
   SPARK_Mode     => On,
   Abstract_State => State
is

   function Invariant return Boolean with Global => (Input => State);

   procedure Reset with Global => (Output => State), Post => Invariant;

   procedure Feed_Stride (
      Process : not null access function return Sample) with
      Pre    => Invariant,
      Post   => Invariant,
      Global => (In_Out => State);

   function Is_Seizure return Boolean with
      Pre    => Invariant,
      Global => (Input => State);

end Seizure.Detector;
