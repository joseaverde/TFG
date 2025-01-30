with Ada.Text_IO;
with CLI;
with Generic_Signals;
with Generic_Detector;
with Generic_Loader;
procedure Detect with SPARK_Mode => On is
   subtype Real is Long_Float;
   package T_IO renames Ada.Text_IO;
   package Signals is new Generic_Signals (Real);
   package Detector is new Generic_Detector (Signals);
   procedure Loader is new Generic_Loader (Signals);
   Signal : Signals.Signal_Access;
begin
   if CLI.Count /= 2 then
      T_IO.Put ("USAGE: `");
      T_IO.Put (CLI.Command_Name);
      T_IO.Put (" signal batch'");
      T_IO.New_Line;
      return;
   end if;
   Loader (CLI.Argument (1), Signal);
   Signals.Free (Signal);
end Detect;
