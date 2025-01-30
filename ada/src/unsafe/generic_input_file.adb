with Ada.Finalization;
with Ada.Streams.Stream_IO;

package body Generic_Input_File with
   SPARK_Mode    => Off,
   Refined_State => (Reader => File)
is

   -->> State <<--

   type Controlled_File is
      new Ada.Finalization.Limited_Controlled with
   record
      File   : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;
   end record;

   overriding procedure Finalize (Object : in out Controlled_File);

   File : Controlled_File;

   -->> Implementation <<--

   overriding
   procedure Finalize (Object : in out Controlled_File) is
   begin
      Ada.Streams.Stream_IO.Close (File.File);
      File.Stream := null;
   end Finalize;

   procedure Read (Item : out Element_Type) is
   begin
      Element_Type'Read (File.Stream, Item);
   end Read;

begin
   Ada.Streams.Stream_IO.Open (
      File => File.File,
      Mode => Ada.Streams.Stream_IO.In_File,
      Name => Name);
   File.Stream := Ada.Streams.Stream_IO.Stream (File.File);
end Generic_Input_File;
