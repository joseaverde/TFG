with Ada.Directories; use Ada.Directories;
with Default_Detector;
with Gen_Fixed_Type;

procedure Gen is
   package Batches renames Default_Detector.Batches;
   procedure Gen_Raw_Sample_Type is
      new Gen_Fixed_Type (
      Fixed_Type => Default_Detector.Sample_Type,
      Name       => "Raw_Sample");
   procedure Gen_Feature_Type is
      new Gen_Fixed_Type (
      Fixed_Type => Default_Detector.Feature_Type,
      Name       => "Feature");
   Gen      : constant String := "gen";
   Template : constant String := "templates";
begin
   if not Exists (Gen) then
      Create_Path (Gen);
   end if;
   Gen_Raw_Sample_Type (Gen, Template);
   Gen_Feature_Type (Gen, Template);
end Gen;
