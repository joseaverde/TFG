package body Detector.Module.Fixed_Types is

   -- Conversions

   function To_Str   (Item : in Fixed) return chars_ptr with Convention => C;
   function To_Float (Item : in Fixed) return double    with Convention => C;
   procedure As_Frac (Num, Den : out long)              with Convention => C;

end Detector.Module.Fixed_Types;
