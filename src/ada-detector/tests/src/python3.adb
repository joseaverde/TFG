with Ada.Containers.Vectors;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Util.Processes;
with Util.Streams.Buffered;
with Util.Streams.Pipes;
with Util.Streams.Texts;

package body Python3 is

   Buffer_Size : constant := 65_536 * 266;

   function Call (
      Script : in String;
      Params : in String)
      return Unbounded_String is
      Command : constant String := "python3 """ & Script & """";
      Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
      Input   : Util.Streams.Buffered.Input_Buffer_Stream;
      Output  : Util.Streams.Texts.Print_Stream;
   begin
      -- Open the pipe
      Pipe.Open (Command => Command, Mode => Util.Processes.READ_WRITE);
      Input.Initialize (Input => Pipe'Unchecked_Access, Size => Buffer_Size);
      Output.Initialize (Pipe'Unchecked_Access);

      -- Write the parameters
      Output.Write (Params);
      Output.Close;
      return Result : Unbounded_String do
         -- FIXME: Can't read too much
         Input.Read (Into => Result);
      end return;
   end Call;

   function Call_Return_Type (
      Script : in String;
      Params : in String)
      return Element_Type is
      Result : constant String := To_String (Call (Script, Params));
      Last   : Positive;
   begin
      return Element : Element_Type do
         Get (Result, Element, Last);
      end return;
   end Call_Return_Type;

   function Call_Return_Array (
      Script : in String;
      Params : in String)
      return Array_Type is
      use Ada.Strings.Fixed;
      package Element_Vectors is
         new Ada.Containers.Vectors (
         Index_Type   => Index_Type,
         Element_Type => Element_Type);
      Result : constant String := To_String (Call (Script, Params));
      Values : Element_Vectors.Vector;
      Value  : Element_Type;
      Iter   : Natural;
      Next   : Natural;
   begin
      Iter := Index (Result, "[", Result'First);
      if Iter = 0 then
         raise Constraint_Error with "Invalid output expected a list";
      end if;
      loop
         -- Skip spaces
         Iter := Iter + 1;
         if Iter not in Result'Range then
            raise Constraint_Error with "Unexpected end of file!";
         end if;
         Next := Index_Non_Blank (Result, Iter);
         if Next not in Result'Range then
            raise Constraint_Error with "Unexpected end of file!";
         end if;

         -- Get the character
         exit when Result (Next) = ']';

         -- Get the value
         Get (Result (Next .. Result'Last), Value, Iter);
         Values.Append (Value);
         Iter := Iter + 1;
         if Iter not in Result'Range then
            raise Constraint_Error with "Unexpected end of file!";
         end if;
         Next := Index_Non_Blank (Result, Iter);
         if Next not in Result'Range then
            raise Constraint_Error with "Unexpected end of file!";
         end if;

         exit when Result (Next) = ']';

         if Result (Next) /= ',' then
            raise Constraint_Error with "Expected a comma!";
         end if;
         Iter := Next;
      end loop;

      return Result : Array_Type (Index_Type'First .. Values.Last_Index) do
         for I in Result'Range loop
            Result (I) := Values (I);
         end loop;
      end return;

   end Call_Return_Array;

   function Script (
      Name : in String)
      return String is
      use Ada.Directories, Ada.Command_Line;
      function Parent (Item : in String) return String
         renames Containing_Directory;
      function "/" (Left, Right : in String) return String is (
         Compose (Left, Right));
   begin
      return Parent (Parent (Command_Name)) / "scripts" / Name;
   end Script;

end Python3;
