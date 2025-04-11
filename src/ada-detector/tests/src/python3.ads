with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Python3 is

   function Call (
      Script : in String;
      Params : in Unbounded_String)
      return Unbounded_String;

   generic
      type Element_Type is private;
      with procedure Get (
         From : in      String;
         Item :    out Element_Type;
         Last :    out Positive);
   function Call_Return_Type (
      Script : in String;
      Params : in Unbounded_String)
      return Element_Type;

   generic
      type Element_Type is private;
      type Index_Type is range <>;
      type Element_Array is array (Index_Type range <>) of Element_Type;
      with procedure Get (
         From : in      String;
         Item :    out Element_Type;
         Last :    out Positive);
   function Call_Return_Array (
      Script : in String;
      Params : in Unbounded_String)
      return Element_Array;

   function Script (
      Name : in String)
      return String;

end Python3;
