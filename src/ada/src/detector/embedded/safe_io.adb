with Interfaces.C;
package body Safe_IO is

   function putchar (Item : in Interfaces.C.int) return Interfaces.C.int with
      Import        => True,
      Convention    => C,
      External_Name => "putchar";

   procedure Put (Item : in Character) is
      Dummy : Interfaces.C.int with Unreferenced;
   begin
      Dummy := putchar (Character'Pos (Item));
   end Put;

   procedure Put (Item : in String) is
   begin
      for C of Item loop
         Put (C);
      end loop;
   end Put;

   procedure Put_Line (Item : in String) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;

   procedure New_Line is
   begin
      Put (Character'Val (10));
   end New_Line;

end Safe_IO;
