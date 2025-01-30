with Generic_Input_File, Types;

procedure Generic_Loader (
   Name   : in     String;
   Signal :    out Signals.Signal_Access) is
   pragma SPARK_Mode (On);
   use Signals, Types;
   package File is new Generic_Input_File (Name);
   procedure Read is new File.Read (Count_Type'Base);
   procedure Read is new File.Read (Real'Base);
   Size : Count_Type'Base;
   Span : Span_Type;
   Val  : Real'Base;
begin
   Signal := null;
   -- Read the header
   Read (Size);
   if Size not in Index_Type then
      return;
   end if;
   -- Read the contents
   Signal := new Signals.Signal'(Last => Size, others => <>);
   Span := Signal.Full_Span;
   pragma Assert (Signal.Is_Valid_Span (Span));
   for Index in 1 .. Signals.Size (Span) loop
      pragma Loop_Invariant (Signal /= null);
      pragma Loop_Invariant (Signal.Is_Valid_Span (Span));
      Read (Val);
      if Val in Sample then
         pragma Assert (Val in Sample);
         Signal.Set (Span, Index, Val);
      end if;
   end loop;
end Generic_Loader;
