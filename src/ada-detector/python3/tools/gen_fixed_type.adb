with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Interfaces;
with Fill_Template;

procedure Gen_Fixed_Type (
   Gen_Path      : in String;
   Template_Path : in String) is
   package Fixed_IO is new Ada.Text_IO.Fixed_IO (Fixed_Type);
   use Ada.Directories, Interfaces;
   use Ada.Characters.Handling, Ada.Strings, Ada.Strings.Fixed;
   function Image (Item : in Fixed_Type) return String is
      Extra : constant Ada.Text_IO.Field := 38 - Fixed_Type'Aft;
   begin
      return Result : String (1 .. Fixed_Type'Width + Extra) do
         Fixed_IO.Put (Result, Item, Fixed_Type'Aft + Extra);
      end return;
   end Image;
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
         "Fixed_First"      => Trim (Image (Fixed_Type'First), Left),
         "Fixed_Last"       => Trim (Image (Fixed_Type'Last), Left),
         "Fixed_Delta_Bits" => Trim (Delta_Bits'Image, Left),
         "Prefix"           => Extern_Prefix
      ]);
end Gen_Fixed_Type;
