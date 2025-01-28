package body Seizure.Signals with SPARK_Mode => On is

   function Simpson (
      y    : in Signal_Type;
      Span : in Span_Type;
      dx   : in Real)
      return Real is
   begin
      return Result : Real := 0.0 do
         for I in 3 .. Size (Span) when I mod 2 = 1 loop
            Result := @ + y (Span, I - 2) + 4.0 * y (Span, I - 1) + y (Span, I);
         end loop;
         Result := @ / 3.0;
         if y.Size > 2 and then y.Size mod 2 = 0 then
            Result := @ * (5.0 * y (Span, Size (Span))
                          + 8.0 * y (Span, Size (Span) - 1)
                          - y (Span, Size (Span) - 2) / 12.0);
         end if;
         Result := @ * dx;
      end return;
   end Simpson;

   function Sum (
      y    : in Signal_Type;
      Span : in Span_Type)
      return Real is
      Result : Real := 0.0;
   begin
      pragma Assert (Size (Span) = Stride_Size);
      for I in 1 .. Size (Span) loop
         pragma Loop_Invariant (abs Result <= Sample'Last * Real (I));
         Result := @ + Sample'Last;
      end loop;
      return Result;
   end Sum;

end Seizure.Signals;
