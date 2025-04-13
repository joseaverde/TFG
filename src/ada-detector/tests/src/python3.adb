with Ada.Containers.Vectors;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Spawn.Processes;
with Spawn.Processes.Monitor_Loop;
with Spawn.String_Vectors;

package body Python3 is

   use Ada.Command_Line;

   Buffer_Size : constant := 65_536;

   function Parent (Item : in String) return String
      renames Ada.Directories.Containing_Directory;

   function Root return String is (
      Parent (Parent (Ada.Directories.Full_Name (Command_Name))));

   function "/" (Left, Right : in String) return String is (
      Ada.Directories.Compose (Left, Right));

   package Listeners is

      type Listener is limited new Spawn.Processes.Process_Listener with record
         Process : Spawn.Processes.Process;
         Stdin   : Unbounded_String;
         Stdout  : Unbounded_String;
         Stderr  : Unbounded_String;
         Started : Boolean := False;
         Stopped : Boolean := False;
         Error   : Integer := 0;
      end record;

      overriding procedure Standard_Output_Available (L : in out Listener);
      overriding procedure Standard_Error_Available  (L : in out Listener);
      overriding procedure Standard_Input_Available  (L : in out Listener);
      overriding procedure Started                   (L : in out Listener);
      overriding procedure Finished (
         L           : in out Listener;
         Exit_Status : in     Spawn.Processes.Process_Exit_Status;
         Exit_Code   : in     Spawn.Processes.Process_Exit_Code);
      overriding procedure Error_Occurred (
         L             : in out Listener;
         Process_Error : in     Integer);

      generic
         with procedure Read (
            Process : in out Spawn.Processes.Process'Class;
            Data    :    out Ada.Streams.Stream_Element_Array;
            Last    :    out Ada.Streams.Stream_Element_Count;
            Success : in out Boolean);
      procedure Read_Into (
         Process : in out Spawn.Processes.Process'Class;
         Result  :     out Unbounded_String);

   end Listeners;

   package body Listeners is

      use all type Ada.Streams.Stream_Element_Offset;

      procedure Read_Into (
         Process : in out Spawn.Processes.Process'Class;
         Result  :     out Unbounded_String) is
         Data    : Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);
         Last    : Ada.Streams.Stream_Element_Count;
         Success : Boolean := True;
      begin
         Read_Loop : loop
            Read (Process, Data, Last, Success);
            exit Read_Loop when Last < Data'First;
            for Char of Data (1 .. Last) loop
               Append (Result, Character'Val (Char));
            end loop;
         end loop Read_Loop;
      end Read_Into;

      overriding
      procedure Standard_Output_Available (L : in out Listener) is
         procedure Reader is
            new Read_Into (Spawn.Processes.Read_Standard_Output);
      begin
         Reader (L.Process, L.Stdout);
      end Standard_Output_Available;

      overriding
      procedure Standard_Error_Available (L : in out Listener) is
         procedure Reader is
            new Read_Into (Spawn.Processes.Read_Standard_Error);
      begin
         Reader (L.Process, L.Stderr);
         Ada.Text_IO.Put_Line (
            File => Ada.Text_IO.Standard_Error,
            Item => To_String (L.Stderr));
      end Standard_Error_Available;

      overriding
      procedure Standard_Input_Available (L : in out Listener) is
         Last    : Ada.Streams.Stream_Element_Count :=
            Ada.Streams.Stream_Element_Count (Length (L.Stdin));
         Data    : Ada.Streams.Stream_Element_Array (1 .. Last);
         Success : Boolean := True;
      begin
         for J in 1 .. Last loop
            Data (J) := Character'Pos (Element (L.Stdin, Positive (J)));
         end loop;
         L.Process.Write_Standard_Input (Data, Last, Success);
         if Last = Data'Last then
            L.Process.Close_Standard_Input;
         else
            L.Stdin :=
               Unbounded_Slice (L.Stdin, Integer (Last + 1), Integer (Data'Last));
         end if;
      end Standard_Input_Available;

      overriding procedure Started (L : in out Listener) is
      begin
         L.Started := True;
      end Started;

      overriding procedure Finished (
         L         : in out Listener;
         Exit_Status : in     Spawn.Processes.Process_Exit_Status;
         Exit_Code   : in     Spawn.Processes.Process_Exit_Code) is
         use type Spawn.Processes.Process_Exit_Code;
      begin
         if Exit_Code /= 0 then
            Ada.Text_IO.Put_Line ("Unexpected exit code" & (Exit_Code'Img));
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         end if;
         L.Stopped := True;
      end Finished;

      overriding procedure Error_Occurred (
         L           : in out Listener;
         Process_Error : in     Integer) is
      begin
         Ada.Text_IO.Put_Line ("Error_Occurred:" & (Process_Error'Img));
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         L.Error := Process_Error;
         L.Stopped := True;
      end Error_Occurred;

   end Listeners;

   function Call (
      Script : in String;
      Params : in String)
      return Unbounded_String is

      Arguments : Spawn.String_Vectors.UTF_8_String_Vector;
      Listener  : aliased Listeners.Listener;

   begin
      -- TODO: Search in PATH
      Arguments.Append (Script);
      Listener.Stdin := To_Unbounded_String (Params);
      Append (Listener.Stdin, ASCII.LF);
      Listener.Process.Set_Program (Root / ".venv" / "bin" / "python3");
      Listener.Process.Set_Arguments (Arguments);
      Listener.Process.Set_Working_Directory (Root / "scripts");
      Listener.Process.Set_Listener (Listener'Unchecked_Access);
      Listener.Process.Start;

      while not Listener.Stopped loop
         Spawn.Processes.Monitor_Loop (0.01);
      end loop;

      return Listener.Stdout;
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
   begin
      return Root / "scripts" / Name;
   end Script;

end Python3;
