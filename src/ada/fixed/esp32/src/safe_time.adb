with System;

package body Safe_Time with SPARK_Mode is

   type time_t is range -2 ** 63 .. 2 ** 63 - 1 with Size => 64;
   type suseconds_t is range -2 ** 31 .. 2 ** 31 - 1 with Size => 32;

   type timeval is record
      tv_sec  : time_t;
      tv_usec : suseconds_t;
   end record with Convention => C;

   package Details with SPARK_Mode => Off is

      function Get_Time return timeval with
         Inline   => True,
         Post     => Get_Time'Result.tv_sec >= 0
            and then Get_Time'Result.tv_usec >= 0;

   end Details;

   package body Details with SPARK_Mode => Off is

      function gettimeofday (
         tp  :    out timeval;
         tzp : in     System.Address)
         return Interfaces.C.int with
         Import        => True,
         Convention    => C,
         External_Name => "gettimeofday",
         Post          => tp.tv_sec >= 0 and then tp.tv_usec >= 0;

      function Get_Time return timeval is
         Dummy  : Interfaces.C.int with Unreferenced;
         Result : timeval;
      begin
         Dummy := Details.gettimeofday (Result, System.Null_Address);
         return Result;
      end Get_Time;

   end Details;

   function Clock return Time is
      Value : constant timeval := Details.Get_Time;
   begin
      return Time (Value.tv_sec) * 1_000_000 + Time (Value.tv_usec);
   end Clock;

   function "-" (Left, Right : in Time) return Duration is
   begin
      return (Fixed_Long (Left) - Fixed_Long (Right)) / Duration (1_000_000.0);
   end "-";

end Safe_Time;
