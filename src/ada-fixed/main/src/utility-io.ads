package Utility.IO is

   generic
      type Float_Type is digits <>;
   procedure Put_Float (Item : in Float_Type);

   generic
      type Fixed_Type is delta <>;
   procedure Put_Fixed (Item : in Fixed_Type);

end Utility.IO;
