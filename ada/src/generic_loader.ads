with Generic_Signals, Generic_Batchs;

pragma Warnings (Off, "unused variable ""Name""", Reason => "It is though");
generic
   with package Signals is new Generic_Signals (<>);
   with package Batchs is new Generic_Batchs (Signals);
procedure Generic_Loader (
   Name   : in     String;
   Signal :    out Signals.Signal_Access;
   Batch  :    out Batchs.Batch_Access) with
   SPARK_Mode => On;
