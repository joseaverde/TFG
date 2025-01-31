generic
   type Index_Type is range <>;
   type Input_Real is digits <>;
   type Input_Array is array (Index_Type range <>) of Input_Real;
   type Output_Real is digits <>;
package Generic_Real_Accumulation with Pure, SPARK_Mode => On is

   type Output_Array is array (Index_Type range <>) of Output_Real;

   First : constant Output_Real := Output_Real (Input_Real'First);
   Last  : constant Output_Real := Output_Real (Input_Real'Last);

   function Low_Bounds (Item : in Input_Array) return Output_Array with
      Ghost    => True,
      Pre      => Item'Length > 0,
      Post     => Low_Bounds'Result'Length = Item'Length
         and then Low_Bounds'Result'First  = Item'First
         and then Low_Bounds'Result (Item'First) = First
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Low_Bounds'Result (I) = Low_Bounds'Result (I) + First);

-- function Partial_Accumulate (Item : in Input_Array) return Output_Array with
--   Ghost => True,
--   Pre   => Item'Length > 0,
--   Post  => Partial_Accumulate'Result'Length = Item'Length
--   and then Partial_Accumulate'Result'First  = Item'First
--   and then (for all I in Item'First .. Item'Last =>
--               Partial_Accumulate'Result (I)
--               >= Output_Real (I - Item'First + 1) * First)
--   and then (for all I in Item'First .. Item'Last =>
--               Partial_Accumulate'Result (I)
--               <= Output_Real (I - Item'First + 1) * Last)
--   and then Partial_Accumulate'Result (Item'First)
--            = Output_Real (Item (Item'First))
--   and then (for all I in Item'First + 1 .. Item'Last =>
--               Partial_Accumulate'Result (I)
--               = Partial_Accumulate'Result (I - 1) + Output_Real (Item (I)));

end Generic_Real_Accumulation;
