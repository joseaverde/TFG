generic
   Name : in String;
package Generic_Input_File with
   SPARK_Mode     => On,
   Abstract_State => Reader,
   Initializes    => Reader
is

   generic
      type Element_Type is private;
   procedure Read (Item : out Element_Type) with
      Global => (In_Out => Reader);

end Generic_Input_File;
