with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;
with Interfaces;
with Fill_Template;

procedure Gen_Fixed_Type (
   Gen_Path      : in String;
   Template_Path : in String) is
   use Ada.Directories, Interfaces;
   use Ada.Characters.Handling, Ada.Strings, Ada.Strings.Fixed;
   File_Name : constant String := "detector-python3-fixed_types-"
                                & To_Lower (Name);
   Extern_Prefix : constant String := "___detector__" & To_Lower (Name);
   Delta_Bits    : Natural := 0;
   Temp          : Integer_64 := Integer_64 (1.0 / Fixed_Type'Delta);
begin

   while Temp > 0 loop
      Temp := Temp / 2;
      Delta_Bits := @ + 1;
   end loop;
   Delta_Bits := @ - 1;

   Fill_Template (
      Input_Path   => Compose (Template_Path, "fixed_type.ads", "in"),
      Output_Path  => Compose (Gen_Path, File_Name, "ads"),
      Substitution => [
         "Type_Name"        => Name,
         "Fixed_Bits"       => Trim (Fixed_Type'Size'Image, Left),
         "Fixed_First"      => Trim (Fixed_Type'First'Image, Left),
         "Fixed_Last"       => Trim (Fixed_Type'Last'Image, Left),
         "Fixed_Delta_Bits" => Trim (Delta_Bits'Image, Left),
         "Prefix"           => Extern_Prefix
      ]);
end Gen_Fixed_Type;
