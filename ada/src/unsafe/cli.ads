package CLI with
   SPARK_Mode     => On,
   Abstract_State => The_Arguments,
   Initializes    => The_Arguments
is

   function Count return Natural with
      Global => (Input => The_Arguments);

   function Argument (Item : in Positive) return String with
      Pre    => Item in 1 .. Count,
      Global => (Input => The_Arguments);

   function Command_Name return String with
      Global => (Input => The_Arguments);

end CLI;
