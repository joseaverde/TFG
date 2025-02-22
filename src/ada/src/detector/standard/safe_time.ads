private with Ada.Calendar;

package Safe_Time is

   type Time is private;

   function Clock return Time;
   function "-" (Left, Right : in Time) return Duration;

private

   type Time is record
      Value : Ada.Calendar.Time;
   end record;

   function Clock return Time is (Value => Ada.Calendar.Clock);
   function "-" (Left, Right : in Time) return Duration is (
      Ada.Calendar."-" (Left.Value, Right.Value));

end Safe_Time;
