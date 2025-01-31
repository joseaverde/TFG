generic
   Size  : in Positive;
   type Index_Type is range <>;
   type Real is digits <>;
   type Real_Array is array (Index_Type range <>) of Real;
   First : in Real;
   Last  : in Real;
package Fold_N with SPARK_Mode is

   subtype Input_Real is Real range First .. Last;
   subtype Output_Real is Real range Real (Size) * First .. Real (Size) * Last;
   type Output_Array is array (Index_Type range <>) of Output_Real;

   function Fold_Acc (Item : in Real_Array) return Output_Array with
      Ghost,
      Pre      => Item'Length = Size
         and then (for all X of Item => X in Input_Real),
      Post     => Fold_Acc'Result'Length = Item'Length
         and then Fold_Acc'Result'First  = Item'First
         and then (for all I in Item'Range =>
                     Fold_Acc'Result (I)
                        in Real (I - Item'First + 1) * First
                        .. Real (I - Item'First + 1) * Last)
         and then Fold_Acc'Result (Item'First) = Item (Item'First)
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Fold_Acc'Result (I) =
                        Fold_Acc'Result (I - 1) + Item (I));

   function Fold_N (Item : in Real_Array) return Output_Real with
      Pre  => Item'Length = Size
               and then (for all X of Item => X in Input_Real),
      Post => Fold_N'Result = Fold_Acc (Item) (Item'Last);

end Fold_N;
