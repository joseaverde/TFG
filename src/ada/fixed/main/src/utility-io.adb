with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

package body Utility.IO is

   Digit_Images : constant array (0 .. 15) of String (1 .. 1) :=
      ["0", "1", "2", "3", "4", "5", "6", "7",
       "8", "9", "A", "B", "C", "D", "E", "F"];

   procedure Put_Float (Item : in Float_Type) is

      -- Robert G. Burger and R. Kent Dybvig: "Printing floating-point numbers
      -- quickly and accurately." In proceedings of ACM SIGPLAN 1996 conference
      -- on Programming Language Design and Implementation, Philadelphia, PA,
      -- USA, May 1996, pp. 108-116. doi: 10.1.1.67.4438

      -- Note that in Ada it is illegal to have `+inf', `-inf' or `NaN'. That's
      -- why we don't use consider them in this algorithm.

      -- The algorithm only works with positive values. We have to take the
      -- absolute value of the given value first.

      -- The idea of the algorithm is as follows:
      --  - Separate the number in mantissa and exponent: 0.DDDDDD * B^K
      --    * B is the Base
      --    * K is the Exponent
      --    * D... are the digits
      --  - If we multiply by B is like doing a shift to left in the current
      --    base. If we substract its floor. We obtain a digit.
      --  - Then we check whether if converting it to a floating point from
      --    its string representation would yield the same number as the
      --    input. That's why we take the number's 'Pred and 'Succ.
      --  - Stop when the approximation is just fine.
      -- This algorithm is modified to use any base, allow negative numbers,
      -- and allow printing numbers with certain exponents.

      subtype FT is Float_Type;
      package EF is new Ada.Numerics.Generic_Elementary_Functions (Float_Type);
      use EF;

      V   : constant FT := abs Item;                  -- The positive value
      B   : constant FT := FT (10.0);                 -- The base
      L   : constant FT := FT'Pred (V);               -- Low value
      H   : constant FT := FT'Succ (V);               -- High value
      K   : constant FT := FT'Ceiling (Log (H, B));   -- 0.DDDD...·B^K
      B_K : constant FT := B ** K;                    -- Scale it

      Q   : FT := V / B_K;                            -- Scaled value
      Acc : FT := 0.0;                                -- For approximation
      Pow : FT := B;                                  -- B^-i
      Run : Boolean := True;                          -- Is Running?

      function Iterate return Integer is
         -- This function is used to get the next digit.
         D   : FT;
         A_L : FT;   -- Low approximation
         A_H : FT;   -- High approximation
         Low : Boolean := True;
      begin
         D   := FT'Floor (Q * B);
         Q   := (Q * B) - D;
         A_L := B_K * (Acc + D / Pow);
         A_H := B_K * (Acc + (D + 1.0) / Pow);
         Acc := Acc + D / Pow;
         Pow := Pow * B;
         Run := False;
         if A_L > L and A_H < H then
            Low := V - A_L < A_H - V;
         elsif A_L > L then
            Low := True;
         elsif A_H < H then
            Low := False;
         else
            Run := True;
         end if;
         return Integer (D) + (if Low then 0 else 1);
      end Iterate;

      -- Minimum number of digits to the left and to the right to consider
      -- using M.MMMMeE notation or DDDD.DDDD notation (with or without the
      -- exponent part)
      Low_Exp  : constant := -3;
      High_Exp : constant := 5;
   begin
      if Item < 0.0 then
         Ada.Text_IO.Put ("-");
      end if;

      if Integer (K) not in Low_Exp .. High_Exp then
         -- Print the number with the exponent.
         Ada.Text_IO.Put (Digit_Images (Iterate));
         Ada.Text_IO.Put (".");
         Ada.Text_IO.Put (Digit_Images (Iterate));
         while Run loop
            Ada.Text_IO.Put (Digit_Images (Iterate));
         end loop;
         if Integer (K) - 1 /= 0 then
            Ada.Text_IO.Put ("e");
            -- Put_Integer (Ada.Text_IO, Integer (K) - 1, Base);
         end if;
      elsif Integer (K) in 1 .. High_Exp then
         -- The number is greater or equal than 1. Then it has the form:
         -- `DDDD.DDD...'
         for I in 1 .. Integer (K) loop
            Ada.Text_IO.Put (Digit_Images (Iterate));
         end loop;
         Ada.Text_IO.Put (".");
         Ada.Text_IO.Put (Digit_Images (Iterate));
         while Run loop
            Ada.Text_IO.Put (Digit_Images (Iterate));
         end loop;
      else
         -- The number is lower than 1, its form is: 0.00...000DDD...
         Ada.Text_IO.Put ("0.");
         for I in Integer (K) .. -1 loop
            Ada.Text_IO.Put ("0");
         end loop;
         while Run loop
            Ada.Text_IO.Put (Digit_Images (Iterate));
         end loop;
      end if;
   end Put_Float;

   procedure Put_Standard_Float is new Put_Float (Float);

   procedure Put_Fixed (Item : in Fixed_Type) is
   begin
      Put_Standard_Float (Float (Item));
   end Put_Fixed;

end Utility.IO;
