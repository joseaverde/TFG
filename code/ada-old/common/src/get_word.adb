with Ada.Text_IO;

procedure Get_Word (
   Result : in out String;
   Last   :    out Natural;
   Valid  :    out Boolean) is
   use Ada.Text_IO;
   Char : Character := ' ';
   EOL  : Boolean;
begin
   Last := Result'First - 1;
   -- Skip spaces
   while not End_Of_File loop
      Look_Ahead (Char, EOL);
      exit when not EOL and then Char /= ' ';
      Get (Char);
   end loop;
   -- Read word
   while Last + 1 in Result'Range loop
      exit when End_Of_File;
      Look_Ahead (Char, EOL);
      exit when EOL or else Char = ' ';
      Last := Last + 1;
      Get (Result (Last));
   end loop;
   Valid := Last in Result'Range and then (EOL or else Char = ' ');
end Get_Word;
