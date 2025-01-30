package body Generic_Signals with SPARK_Mode => On is

   procedure Set (
      Item  : in out Signal;
      Span  : in     Span_Type;
      Index : in     Count_Type;
      Value : in     Sample) is
   begin
      Item.Samples (Span.First - 1 + Index) := Value;
   end Set;

end Generic_Signals;
