with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Long_Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Long_Integer_Text_IO;
procedure Program with SPARK_Mode is
   X, Y : Integer;
   R : Long_Integer;
begin
   Put ("Introduzca dos números: ");
   Get (X);
   Get (Y);

   Put ("La suma de ");
   Put (X, 1);
   Put (" + ");
   Put (Y, 1);
   Put (" es ");
   R := Long_Integer (X) + Long_Integer (Y);
   Put (R, 1);
   New_Line;
exception
   when others =>
      Put_Line ("Ha ocurrido un error");
end Program;
