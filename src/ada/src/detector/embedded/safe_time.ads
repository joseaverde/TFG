package Safe_Time is

   type Time is private;

   function Clock return Time;
   function "-" (Left, Right : in Time) return Duration;

private

   subtype Int is Long_Long_Integer;
   type Time is new Int;

end Safe_Time;
