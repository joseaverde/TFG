private with Interfaces.C;

package Safe_Time with SPARK_Mode is

   type Time is private;

   function Clock return Time;
   function "-" (Left, Right : in Time) return Duration;

private

   Long_Bits : constant := Interfaces.C.long'Size;
   type Fixed_Long is delta 1.0
      range -2.0 ** (Long_Bits - 1) .. 2.0 ** (Long_Bits - 1) - 1.0 with
   Size => Long_Bits;

   type Time is new Fixed_Long;

end Safe_Time;
