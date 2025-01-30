with Ada.Streams.Stream_IO;
with Generic_Signals.Holders;

package body Generic_Loader with
   SPARK_Mode    => On,
   Refined_State => (The_Signal => (Buffer),
                     The_Cursor => (Current))
is

   package Holders is new Signals.Holders;
   use Holders, Signals;

   Buffer  : Signal_Holder;
   Current : Extended_Index;

   function Invariant return Boolean is (
      not Is_Empty (Buffer) and then
      Current in 0 .. Last (Buffer));

   function Remaining return Count_Type is (
      Last (Buffer) - Current);

   procedure Load (Name : in String) is
      pragma SPARK_Mode (Off);
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Size : Count_Type'Base;
      Str  : Stream_Access;
      procedure Process (Item : in out Signal) is
         Span  : constant Span_Type := Item.Full_Span;
         Value : Real'Base;
      begin
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
      end Process;
   begin
      Open (File, In_File, Name);
      Str := Stream (File);
      Count_Type'Base'Read (Str, Size);
      if Size not in Index_Type then
         raise Program_Error with
            "The amount of samples in the file was" & Size'Image &
            "which isn't in the constraint's range: " &
            Index_Type'First'Image & " .." & Index_Type'Last'Image;
      end if;
      Current := 0;
      Create (Buffer, Size);
      Update (Buffer, Process'Access);
      Close (File);
   end Load;

   procedure Feeder (Result : out Signals.Sample) is
      Span : Span_Type;
   begin
      Current := @ + 1;
      Span := (Current, Last (Buffer));
      Result := Get (Buffer, Span, 1);
   end Feeder;

end Generic_Loader;
