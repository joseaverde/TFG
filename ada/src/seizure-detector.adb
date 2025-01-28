package body Seizure.Detector with
   SPARK_Mode    => On,
   Refined_State => (State => (Buffer, Span))
is

   Buffer : Signal (Epoch_Size);
   Span   : Span_Type := (1, 0);

   function Invariant return Boolean is (
      Is_Valid_Span (Buffer, Span) and then
      Span.First = 1);

   procedure Reset is
   begin
      Buffer.Samples := [others => 0.0];
      Span := (1, 0);
   end Reset;

   procedure Feed_Stride (Process : not null access function return Sample) is
      Index : Count_Type := 0;
   begin
      pragma Assert (Invariant);
      pragma Assert (Is_Valid_Span (Buffer, Span));
      pragma Assert (Span.Last <= Epoch_Size);
      if Span.Last < Epoch_Size then
         pragma Assert (Span.Last >= Index);
         while Index < Stride_Size and then Span.Last < Epoch_Size loop
            Span.Last := @ + 1;
            Index := @ + 1;
            pragma Loop_Invariant (Span.Last >= Index);
            pragma Loop_Invariant (Index in 1 .. Stride_Size);
            pragma Loop_Invariant (Is_Valid_Span (Buffer, Span));
            Buffer.Set (Span, Index, Process.all);
         end loop;
      else
         pragma Assert (Span.Last = Epoch_Size);
         Buffer.Samples (Stride_Size .. Epoch_Size) :=
            Buffer.Samples (1 .. Epoch_Size - Stride_Size + 1);
         for Index in 1 .. Stride_Size loop
            Buffer.Set (Span, Index, Process.all);
         end loop;
      end if;
   end Feed_Stride;

   function Is_Seizure return Boolean is
   begin
      if Span not in Epoch_Span then
         return False;
      else
         return True;
      end if;
   end Is_Seizure;

end Seizure.Detector;
