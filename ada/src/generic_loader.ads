with Generic_Signals;

generic
   with package Signals is new Generic_Signals (<>);
procedure Generic_Loader (
   Name   : in     String;
   Signal :    out Signals.Signal_Access) with
   SPARK_Mode => On;
