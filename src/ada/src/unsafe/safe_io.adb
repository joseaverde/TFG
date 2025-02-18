package body Safe_IO with SPARK_Mode => On is

   package T_IO renames Ada.Text_IO;

   procedure Put (Item : in Character) is
   begin
      T_IO.Put (Item);
   end Put;

   procedure Put (Item : in String) is
   begin
      T_IO.Put (Item);
   end Put;

   procedure Put_Line (Item : in String) is
   begin
      T_IO.Put_Line (Item);
   end Put_Line;

   procedure New_Line is
   begin
      T_IO.New_Line;
   end New_Line;

   procedure Get_Word (
      Value : in out String;
      Last  :    out Natural;
      Valid : in out Boolean) is
      Char : Character := ' ';
      EOL  : Boolean;
   begin
      Last := Value'First - 1;
      if not Valid then
         return;
      end if;
      -- Skip spaces
      while not T_IO.End_Of_File loop
         T_IO.Look_Ahead (Char, EOL);
         exit when not EOL and then Char /= ' ';
         if EOL then
            T_IO.Skip_Line;
         else
            T_IO.Get (Char);
         end if;
      end loop;
      -- Read word
      while Last + 1 in Value'Range loop
         exit when T_IO.End_Of_File;
         T_IO.Look_Ahead (Char, EOL);
         exit when EOL or else Char = ' ';
         if EOL then
            T_IO.Skip_Line;
         else
            Last := Last + 1;
            T_IO.Get (Value (Last));
         end if;
      end loop;
      EOL := EOL or else T_IO.End_Of_File;
      Valid := Last in Value'Range and then (EOL or else Char = ' ');
   end Get_Word;

   procedure Generic_Get (
      Value :    out Object_Type;
      Valid : in out Boolean) is
      Buffer : String (1 .. Max_Length);
      Last   : Natural;
   begin
      if not Valid then
         return;
      end if;
      Get_Word (Buffer, Last, Valid);
      if Valid then
         Value := From_String (Buffer (1 .. Last));
      end if;
   exception
      when others =>
         Valid := False;
   end Generic_Get;

end Safe_IO;
