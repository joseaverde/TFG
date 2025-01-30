with Ada.Command_Line;
package body CLI with
   SPARK_Mode => Off
is

   function Count return Natural is (
      Ada.Command_Line.Argument_Count);

   function Argument (Item : in Positive) return String is (
      Ada.Command_Line.Argument (Item));

   function Command_Name return String is (
      Ada.Command_Line.Command_Name);

end CLI;
