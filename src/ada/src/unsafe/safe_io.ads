with Ada.Text_IO;

package Safe_IO with SPARK_Mode => On is

   procedure Put (Item : in Character) with
      Global => (In_Out => Ada.Text_IO.File_System);
   procedure Put (Item : in String) with
      Global => (In_Out => Ada.Text_IO.File_System);
   procedure Put_Line (Item : in String) with
      Global => (In_Out => Ada.Text_IO.File_System);
   procedure New_Line with
      Global => (In_Out => Ada.Text_IO.File_System);
   procedure Put_Error (Item : in String) with
      Global => (In_Out => Ada.Text_IO.File_System);
   procedure Put_Line_Error (Item : in String) with
      Global => (In_Out => Ada.Text_IO.File_System);

   procedure Get_Word (
      Value : in out String;
      Last  :    out Natural;
      Valid : in out Boolean) with
      Post     => Last in Value'First - 1 .. Value'Last
         and then  Valid = Value'Initialized,
      Global   => (In_Out => Ada.Text_IO.File_System);

   generic
      type Object_Type is private;
      with function From_String (Item : in String) return Object_Type;
      Max_Length : Positive := 32;
   procedure Generic_Get (
      Value :    out Object_Type;
      Valid : in out Boolean) with
      Post   => Valid = Value'Initialized,
      Global => (In_Out => Ada.Text_IO.File_System);

end Safe_IO;
