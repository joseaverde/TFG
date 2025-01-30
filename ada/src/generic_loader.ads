with Generic_Signals;
with Types; use Types;

generic
   with package Signals is new Generic_Signals (<>);
package Generic_Loader with
   SPARK_Mode     => On,
   Abstract_State => (The_Signal, The_Cursor)
is

   function Invariant return Boolean with
      Global => (Input => (The_Signal, The_Cursor));

   function Remaining return Types.Count_Type with
      Pre    => Invariant,
      Global => (Input => (The_Cursor, The_Signal));

   procedure Load (Name : in String) with
      Post   => Invariant,
      Global => (Output => (The_Signal, The_Cursor));

   procedure Feeder (Result : out Signals.Sample) with
      Pre    => Invariant and then Remaining > 0,
   -- Post   => (declare
   --              Old : constant Count_Type := Remaining'Old;
   --           begin
   --              Invariant and then Remaining = Old),
      Global => (Input  => (The_Signal),
                 In_Out => (The_Cursor));

end Generic_Loader;
