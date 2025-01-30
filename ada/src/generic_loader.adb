with Ada.Streams.Stream_IO, Types;

procedure Generic_Loader (
   Name   : in     String;
   Signal :    out Signals.Signal_Access) is
   pragma SPARK_Mode (Off);
   use Ada.Streams.Stream_IO, Signals, Types;
   File : File_Type;
   Size : Count_Type'Base;
   Str  : Stream_Access;
   Span : Span_Type;
   Val  : Real'Base;
begin
   Open (File, In_File, Name);
   Str := Stream (File);
   -- Read the header
   Count_Type'Base'Read (Str, Size);
   if Size not in Index_Type then
      raise Program_Error with
         "The amount of samples in the file was" & Size'Image &
         "which isn't in the constraint's range: " &
         Index_Type'First'Image & " .." & Index_Type'Last'Image;
   end if;
   -- Read the contents
   Signal := new Signals.Signal'(Last => Size, Samples => [others => 0.0]);
   for Index in 1 .. Signals.Size (Span) loop
      Real'Base'Read (Str, Value);
      if Value not in Sample then
         raise Program_Error with
            "Error while deserialising signal, the read value" &
            Value'Image & " is outside of the Sample's constraint " &
            "range " & Sample'First'Image & " .." & Sample'Last'Image;
      end if;
      Item.Set (Span, Index, Value);
   end loop;
   Close (File);
end Generic_Loader;
