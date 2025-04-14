-- with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;
with String_Maps;
with Fill_Template;

procedure Gen_Fixed_Type (
   Gen_Path      : in String;
   Template_Path : in String) is
   use Ada.Directories;
   use Ada.Characters.Handling, Ada.Strings, Ada.Strings.Fixed;
   File_Name : constant String := "detector-python-fixed-types-"
                                & To_Lower (Name);
   Extern_Prefix : constant String := "___detector__" & To_Lower (Name);
begin
   Fill_Template (
      Input_Path   => Compose (Template_Path, "fixed_type.ads", "in"),
      Output_Path  => Compose (Gen_Path, File_Name, "ads"),
      Substitution => [
         "Type_Name"   => Name,
         "Fixed_Bits"  => Trim (Fixed_Type'Size'Image, Left),
         "Fixed_Delta" => Trim (Fixed_Type'Delta'Image, Left),
         "Prefix"      => Extern_Prefix
      ]);
end Gen_Fixed_Type;
