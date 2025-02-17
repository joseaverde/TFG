with Batchs, Signals;
pragma Warnings (Off, "unused variable ""Name""", Reason => "It is though");
procedure Loader (
   Name   : in     String;
   Batch  :    out Batchs.Batch_Access;
   Signal :    out Signals.Signal_Access) with
   SPARK_Mode => On;
