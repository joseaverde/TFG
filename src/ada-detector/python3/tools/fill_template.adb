with Ada.Text_IO;
with GNATCOLL.Templates;

procedure Fill_Template (
   Input_Path   : in String;
   Output_Path  : in String;
   Substitution : in String_Maps.Map) is

   use Ada.Text_IO;

   Line : Natural := 0;

   function Callback (
      Name   : in String;
      Quoted : in Boolean)
      return String is
      pragma Unreferenced (Quoted);
   begin
      if Substitution.Contains (Name) then
         return Substitution (Name);
      else
         return raise Constraint_Error with
            "`" & Name & "' unknown key at line" & Line'Image;
      end if;
   end Callback;

   Input_File  : File_Type;
   Output_File : File_Type;

begin

   Open (Input_File, In_File, Input_Path);
   Create (Output_File, Out_File, Output_Path);

   while not End_Of_File (Input_File) loop
      Line := Line + 1;
      Replace_Line : declare
         Input_Line : constant String := Get_Line (Input_File);
         Output_Line : constant String := GNATCOLL.Templates.Substitute (
            Str        => Input_Line,
            Callback   => Callback'Unrestricted_Access,
            Delimiter  => '$',
            Recursive  => False,
            Errors     => GNATCOLL.Templates.Report_Error);
      begin
         Put_Line (Output_File, Output_Line);
      end Replace_Line;
   end loop;

   Close (Input_File);
   Close (Output_File);

end Fill_Template;
