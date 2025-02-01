generic
   type Index_Type is range <>;
   type Real is digits <>;
   type Real_Array is array (Index_Type range <>) of Real;
   Size  : in Positive;
   First : in Real;
   Last  : in Real;
package Generic_Real_Sum with Pure, SPARK_Mode => On is

   subtype Input_Real is Real range First .. Last;
   subtype Output_Real is Real range Real (Size) * First .. Real (Size) * Last;
   type Output_Array is array (Index_Type range <>) of Output_Real;

   function Sum_Acc (Item : in Real_Array) return Output_Array with
      Ghost,
      Pre      => Item'Length = Size
         and then (for all X of Item => X in Input_Real),
      Post     => Sum_Acc'Result'Length = Item'Length
         and then Sum_Acc'Result'First  = Item'First
         and then (for all I in Item'Range =>
                     Sum_Acc'Result (I)
                        in Real (I - Item'First + 1) * First
                        .. Real (I - Item'First + 1) * Last)
         and then Sum_Acc'Result (Item'First) = Item (Item'First)
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Sum_Acc'Result (I) =
                        Sum_Acc'Result (I - 1) + Item (I));

   function Sum (Item : in Real_Array) return Output_Real with
      Pre  => Item'Length = Size
               and then (for all X of Item => X in Input_Real),
      Post => Sum'Result = Sum_Acc (Item) (Item'Last);

end Generic_Real_Sum;
