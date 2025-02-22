with Interfaces.C;
with System;

package body Safe_Time is

   type long is range -2 ** 63 .. 2 ** 63 - 1 with Size => 64;

   subtype time_t is long;
   subtype suseconds_t is long;

   type timeval is record
      tv_sec  : time_t;
      tv_usec : suseconds_t;
   end record with Convention => C;

   function gettimeofday (
      tp  :     out timeval;
      tzp : in      System.Address)
      return Interfaces.C.int with
      Import        => True,
      Convention    => C,
      External_Name => "gettimeofday";

   function Clock return Time is
      Dummy : Interfaces.C.int with Unreferenced;
      Value : timeval;
   begin
      Dummy := gettimeofday (Value, System.Null_Address);
      return Time (Value.tv_sec) * 1_000_000 + Time (Value.tv_usec);
   end Clock;

   function "-" (Left, Right : in Time) return Duration is
   begin
      return Duration (Int (Left) - Int (Right)) / 1_000_000.0;
   end "-";

end Safe_Time;
