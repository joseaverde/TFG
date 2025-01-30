with Ada.Text_IO;
with CLI;
with Generic_Signals, Generic_Detector, Generic_Batchs, Generic_Loader;
procedure Detect with SPARK_Mode => On is
   subtype Real is Long_Float;
   package T_IO renames Ada.Text_IO;
   package Signals is new Generic_Signals (Real);
   package Batchs is new Generic_Batchs (Signals);
   package Detector is new Generic_Detector (Signals);
   procedure Loader is new Generic_Loader (Signals, Batchs);
   Signal : Signals.Signal_Access;
   Batch  : Batchs.Batch_Access;
begin
   if CLI.Count /= 2 then
      T_IO.Put ("USAGE: `");
      T_IO.Put (CLI.Command_Name);
      T_IO.Put (" signal batch'");
      T_IO.New_Line;
      return;
   end if;
   Loader (CLI.Argument (1), Signal, Batch);
   Signals.Free (Signal);
   Batchs.Free (Batch);
end Detect;
