with Ada.Text_IO;

procedure Get_Word (
   Result : in out String;
   Last   :    out Natural;
   Valid  :    out Boolean) with
   SPARK_Mode => On,
   Post       => Last in Result'First - 1 .. Result'Last,
   Global     => (In_Out => Ada.Text_IO.File_System);
