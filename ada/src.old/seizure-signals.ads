generic
package Seizure.Signals with Pure, SPARK_Mode => On is

   function Simpson (
      y    : in Signal_Type;
      Span : in Span_Type;
      dx   : in Real)
      return Real with
      Pre => Is_Valid_Span (y, Span);

   function Sum (
      y    : in Signal_Type;
      Span : in Span_Type)
      return Real with
      Pre => Is_Valid_Span (y, Span) and then Is_An_Epoch (Span);

end Seizure.Signals;
