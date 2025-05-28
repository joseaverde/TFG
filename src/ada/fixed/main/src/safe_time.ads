package Safe_Time is

   type Time is private;

   function Clock return Time;
   function "-" (Left, Right : in Time) return Duration;

private

   type Long is range -2 ** 63 .. 2 ** 63 - 1 with Size => 64;
   type Time is new Long;

end Safe_Time;
