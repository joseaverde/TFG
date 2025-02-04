with FFTW;
with Generic_Signals;

generic
   with package FFT is new FFTW.Formal (<>);
   with package Signals is new Generic_Signals (FFT.Float_Type, <>);
package Generic_Welch with
   SPARK_Mode     => On,
   Abstract_State => The_Buffer
-- Initializes    => The_Buffer
is

   procedure Execute (
      In_Signal  : in     Signals.Signal;
      In_Span    : in     Signals.Span_Type;
      Out_Signal :    out Signals.Signal;
      Out_Span   : in     Signals.Span_Type) with
      Pre      => In_Signal.Is_Valid_Span (In_Span)
         and then Out_Signal.Is_Valid_Span (Out_Span);
      -- Global   => (In_Out => The_Buffer);

end Generic_Welch;
