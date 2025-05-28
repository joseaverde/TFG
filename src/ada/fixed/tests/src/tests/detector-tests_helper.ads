with AUnit.Test_Cases;
package Detector.Tests_Helper is

   Max_Name_Length : constant := 32;

   subtype Test_Case is AUnit.Test_Cases.Test_Case;
   subtype TC is AUnit.Test_Cases.Test_Case'Class;

   type Routine is private;
   type Routine_Array is array (Positive range <>) of Routine;

   function Make (
      Name : in String;
      Item : in AUnit.Test_Cases.Test_Routine)
      return Routine;

   procedure Register (
      Test     : in out Test_Case'Class;
      Routines : in     Routine_Array);

private

   subtype Name_Length is Natural range 0 .. Max_Name_Length;

   type Routine (Length : Name_Length := 0) is record
      Name : String (1 .. Length);
      Item : AUnit.Test_Cases.Test_Routine := null;
   end record;

   function Make (
      Name : in String;
      Item : in AUnit.Test_Cases.Test_Routine)
      return Routine is (
      Length => Natural'Min (Max_Name_Length, Name'Length),
      Name   => Name (Name'First .. Name'First
                      + Natural'Min (Max_Name_Length, Name'Length) - 1),
      Item   => Item);

end Detector.Tests_Helper;
