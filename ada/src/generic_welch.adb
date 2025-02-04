with Ada.Numerics.Generic_Elementary_Functions;
with Generic_Algorithms;

package body Generic_Welch with
   SPARK_Mode    => On,
   Refined_State => (The_Buffer => (Plan))
is

   Plan : FFT.Plan_Type := FFT.Create (10);

   package Algorithms is new Generic_Algorithms (Signals);

   package Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Signals.Real);

   procedure Execute (
      In_Signal  : in     Signals.Signal;
      In_Span    : in     Signals.Span_Type;
      Out_Signal :    out Signals.Signal;
      Out_Span   : in     Signals.Span_Type) is
   begin
      null;
   end Execute;

end Generic_Welch;
