--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    debug_io.adb                                                   |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Interfaces.C; use Interfaces.C;
package body Debug_IO is

   function putchar (c : int) return int with
      Import, Convention => C, External_Name => "putchar";

   procedure Put (Item : in Character) is
      Dummy : int with Unreferenced;
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

   procedure New_Line (Count : in Positive := 1) is
   begin
      for I in 1 .. Count loop
         Put (ASCII.LF);
      end loop;
   end New_Line;

end Debug_IO;
