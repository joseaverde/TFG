--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_simpson_details.ads                   |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals.Lemmas;

private generic
package Detector.Signals.Generic_Simpson_Details with Pure, SPARK_Mode is

   -- This package's only purpose is proving the abscence of errors for the
   -- `Detector.Signals.Generic_Simpson' function. The function computes an
   -- approximation of the integral given a set of points `Signal' separated by
   -- a distance of `dx' units. It computes:
   --
   --    1 N/2-1 +-                           -+
   --   ---  Σ   | f(2i) + 4 f(2i+1) + f(2i+2) | + e
   --    3  i=0  +-                           -+
   --
   -- where `e' is:
   --          5          4             1
   --    e = ---- f(N) + --- f(N-1) - ---- f(N-2), if N is even
   --         12          6            12
   --
   --    e = 0                                   , if N is odd
   --
   -- and `N' is the array's length and `f' is the function to be evaluated
   -- in range 0 .. N - 1. The division is the whole division. The result of
   -- said division is floored truncated.

   -->> Internal Type <<--

   -- Internally it is just an accumulation. The only thing is that in each
   -- iteration we add up to 6 times the base range of Sample_Type. But we are
   -- going to do a different approach.

   Internal_Bits          : constant := Bits * 2;
   Internal_Fraction_Bits : constant := Sample_Bits;
   Internal_Whole_Bits    : constant :=
      Internal_Bits - Internal_Fraction_Bits - 1;
   Internal_Delta         : constant := 2.0 ** (-Internal_Fraction_Bits);
   type Internal_Type is
      delta Internal_Delta
      range -2.0 ** Internal_Whole_Bits
         .. 2.0 ** Internal_Whole_Bits - Internal_Delta with
      Size => Internal_Bits;

   -- In order to prove that there is no overflow we are going to rewrite the
   -- terms of the first sum. If we expand it we see:
   --
   --    f(0) + 4f(1) + f(2)
   --    f(2) + 4f(3) + f(4)
   --    f(4) + 4f(5) + f(6)
   --    ...
   --
   -- We can see that the elements in odd positions are always multiplied by 4.
   -- And all the elements in even positions are added once at the end of each
   -- iteration and another time at the begining of the first iteration, except
   -- for the first and last elements.
   --
   -- We can rewrite the formula as
   --
   --    1 N/2-1 +-         -+    1 N/2-1 +-       -+
   --   ---  Σ   | 4 f(2i+1) | + ---  Σ   | 2 f(2i) | + f(0) + f(2(N/2-1)+2)
   --    3  i=0  +-         -+    3  i=1  +-       -+
   --
   -- We only have to compute two reductions. We also defer the multiplication
   -- to the end and hopefully save some time. The number of iterations of
   -- the first sum is greater than the second one. We can fix it by moving the
   -- last iteration outside and fixing the ranges:
   --
   --    4 N/2-2            2 N/2-2
   --   ---  Σ   f(2i+1) + ---  Σ   2 f(2i+2) + rest
   --    3  i=0             3  i=0
   --
   --
   --   rest = f(0) + f(2(N/2-1)+2) + 4f(2(N/2-1)+1)
   --
   -- Let's call the first sum: `Odds' and the second one `Evens'. The number
   -- of elements they have:
   --
   --    (Length - 3) / 2
   --
   -- elements.

   -->> Subroutines <<--

   subtype Valid_Signal is Signal_Type with
      Dynamic_Predicate => Valid_Signal'Length >= 3;

   -- Let's divide the function in smaller, but provable subroutines. First
   -- the extra part that is added if the length is even. We can accotate it to
   -- a known range.

   subtype Extra_Internal_Part is Internal_Type
      range 2 * Internal_Type (Sample_Type'First)
         .. 2 * Internal_Type (Sample_Type'Last);

   function Extra_Part (
      Signal : in Valid_Signal)
      return Extra_Internal_Part with
      Inline => True,
      Global => null;

   -- Then let's add the values of the first and last iterations of the
   -- original sum. Which are going to be in range.

   subtype Rest_Internal_Part is Internal_Type
      range 6 * Internal_Type (Sample_Type'First)
         .. 6 * Internal_Type (Sample_Type'Last);

   function First_And_Last_Iterations (
      Signal : in Valid_Signal)
      return Rest_Internal_Part with
      Inline => True,
      Global => null;

   -- Finally let's work with the even and odd parts.

   type Internal_Array is array (Index_Type range <>) of Internal_Type;
   subtype Internal_Step is Internal_Type
      range Internal_Type (Sample_Type'First)
         .. Internal_Type (Sample_Type'Last);

   function Acc_Sum is
      new Detector.Signals.Lemmas.Generic_Accumulation (
      Fixed_Type => Internal_Type,
      Index_Type => Index_Type,
      Array_Type => Internal_Array,
      First      => Internal_Step'First,
      Last       => Internal_Step'Last);

   function Middle_Length (Signal : in Valid_Signal) return Count_Type is (
      (Signal'Length - 3) / 2) with
      Post   => Middle_Length'Result = (Signal'Length - 3) / 2,
      Inline => True,
      Global => null;

   function Take_Evens (Signal : in Valid_Signal) return Internal_Array with
      Post     => Take_Evens'Result'Length = Middle_Length (Signal)
         and then Take_Evens'Result'First = 1
         and then (for all I in Take_Evens'Result'Range =>
                     Take_Evens'Result (I)
                     = Internal_Type (Signal (Signal'First + I * 2)))
         and then (for all X of Take_Evens'Result => X in Internal_Step),
      Ghost    => True,
      Global   => null;

   function Take_Odds (Signal : in Valid_Signal) return Internal_Array with
      Post     => Take_Odds'Result'Length = Middle_Length (Signal)
         and then Take_Odds'Result'First = 1
         and then (for all I in Take_Odds'Result'Range =>
                     Take_Odds'Result (I)
                     = Internal_Type (Signal (Signal'First + I * 2 - 1)))
         and then (for all X of Take_Odds'Result => X in Internal_Step),
      Ghost    => True,
      Global   => null;

   function Even_Result (Signal : in Valid_Signal) return Internal_Type with
      Post   =>
         Even_Result'Result
            in Natural (Middle_Length (Signal)) * Internal_Step'First
            .. Natural (Middle_Length (Signal)) * Internal_Step'Last,
      Inline => True,
      Global => null;

   function Odd_Result (Signal : in Valid_Signal) return Internal_Type with
      Post   =>
         Odd_Result'Result
            in Natural (Middle_Length (Signal)) * Internal_Step'First
            .. Natural (Middle_Length (Signal)) * Internal_Step'Last,
      Inline => True,
      Global => null;

end Detector.Signals.Generic_Simpson_Details;
