with Generic_Algorithms;

package body Generic_Detector with
   SPARK_Mode    => On,
   Refined_State => (State => (Buffer, Span))
is

   package Algorithms is new Generic_Algorithms (Signals);

   use Signals, Types;

   Buffer : Signal (Epoch_Size);
   Span   : Span_Type;

   function Invariant return Boolean is (
      Is_Valid_Span (Buffer, Span) and then
      Span.First = 1);

   procedure Reset is
   begin
      Buffer.Samples := [others => 0.0];
      Span := (1, 0);
   end Reset;

   procedure Write (
      Signal : in Signals.Signal;
      Stride : in Signals.Stride_Span) is
   begin
      pragma Assert (Span.Last <= Epoch_Size);
      if Span.Last < Epoch_Size then
         for Index in Stride_Index when Span.Last < Epoch_Size loop
            Span.Last := @ + 1;
            pragma Loop_Invariant (Span.First = 1);
            pragma Loop_Invariant (Span.Last >= Index);
            pragma Loop_Invariant (Is_Valid_Span (Buffer, Span));
            Buffer.Set (Span, Index, Signal (Stride, Index));
         end loop;
         Span.First := 1;
      else
         Buffer.Samples (Stride_Size .. Epoch_Size) :=
            Buffer.Samples (1 .. Epoch_Size - Stride_Size + 1);
         for Index in Stride_Index loop
            Buffer.Set (Span, Index, Signal (Stride, Index));
         end loop;
      end if;
   end Write;

   function Is_Seizure (
      Batch : in Batchs.Batch_Type)
      return Boolean is
      use Algorithms, Batchs;
   begin
      if Span not in Epoch_Span then
         return False;
      else
         if Is_In (Max_Dist (Buffer, Span), Batch.Max_Dist) then
            return True;
         else
            return False;
         end if;
      end if;
   end Is_Seizure;

end Generic_Detector;
