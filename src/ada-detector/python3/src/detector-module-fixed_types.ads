with Interfaces.C.Strings;

generic
   type Fixed is delta <>;
package Detector.Module.Fixed_Types is

   use Interfaces.C, Interfaces.C.Strings;

   -- Constructors

   function Zero                             return Fixed with Convention => C;
   function From_Float (Item : in double)    return Fixed with Convention => C;
   function From_Long  (Item : in long)      return Fixed with Convention => C;
   function From_Frac  (Num, Den : in long)  return Fixed with Convention => C;
   function From_Str   (Item : in chars_ptr) return Fixed with Convention => C;

   -- Attributes

   function First                   return Fixed with Convention => C;
   function Last                    return Fixed with Convention => C;
   function Size                    return long  with Convention => C;
   function Delt                    return Fixed with Convention => C;
   function Succ  (Item : in Fixed) return Fixed with Convention => C;
   function Pred  (Item : in Fixed) return Fixed with Convention => C;
   function Pos   (Item : in Fixed) return long  with Convention => C;
   function Val   (Item : in long)  return Fixed with Convention => C;

   -- Binary operations

   function Add (Left, Right : in Fixed) return Fixed with Convention => C;
   function Sub (Left, Right : in Fixed) return Fixed with Convention => C;
   function Cmp (Left, Right : in Fixed) return int   with Convention => C;

   -- Unary operations

   function Absf (Right : in Fixed) return Fixed with Convention => C;
   function Plus (Right : in Fixed) return Fixed with Convention => C;
   function Neg  (Right : in Fixed) return Fixed with Convention => C;

   -- Conversions

   function To_Long  (Item : in Fixed) return long      with Convention => C;
   function To_Str   (Item : in Fixed) return chars_ptr with Convention => C;
   function To_Float (Item : in Fixed) return double    with Convention => C;
   procedure As_Frac (Num, Den : out long)              with Convention => C;

private

   -- Constructors

   function Zero                             return Fixed is (0.0);
   function From_Float (Item : in double)    return Fixed is (Fixed (Item));
   function From_Long  (Item : in long)      return Fixed is (Fixed (Item));
   function From_Frac  (Num, Den : in long)  return Fixed is (
      Fixed_Long (Num) / Fixed_Long (Den));
   function From_Str   (Item : in chars_ptr) return Fixed is (
      Fixed'Value (Value (Item)));

   -- Attributes

   function First                   return Fixed is (Fixed'First);
   function Last                    return Fixed is (Fixed'Last);
   function Size                    return long  is (Fixed'Size);
   function Delt                    return Fixed is (Fixed'Delta);
   function Succ  (Item : in Fixed) return Fixed is (Fixed'Succ (Item));
   function Pred  (Item : in Fixed) return Fixed is (Fixed'Pred (Item));
   function Pos   (Item : in Fixed) return long  is (Fixed'Pos (Item));
   function Val   (Item : in long)  return Fixed is (Fixed'Val (Item));

   -- Binary operations

   function Add (Left, Right : in Fixed) return Fixed is (Left + Right);
   function Sub (Left, Right : in Fixed) return Fixed is (Left - Right);
   function Cmp (Left, Right : in Fixed) return int   is (
      (if Left = Right then 0 elsif Left < Right then -1 else 1));

   -- Unary operations

   function Absf (Right : in Fixed) return Fixed is (abs Right);
   function Plus (Right : in Fixed) return Fixed is (+Right);
   function Neg  (Right : in Fixed) return Fixed is (-Right);

   -- Conversions

   function To_Long  (Item : in Fixed) return long      is (long (Item));
   function To_Float (Item : in Fixed) return double    is (double (Item));
   function To_Str   (Item : in Fixed) return chars_ptr is (
      New_String (Item'Image));

end Detector.Module.Fixed_Types;
