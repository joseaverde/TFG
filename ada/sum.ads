package Sum with SPARK_Mode is

   --  The ranges of the list's index and element discrete types must be
   --  limited in order to prevent overflow during summation, i.e.:
   --
   --     Nat'Last * Int'First >= Integer'First   and
   --     Nat'Last * Int'Last  <= Integer'Last
   --
   --  In this case +/-1000 * +/-1000 = +/-1_000_000 which is well within the 
   --  range of the Ada Integer type on typical platforms.
   
   subtype Int is Integer range -1000 .. 1000;
   subtype Nat is Integer range     0 .. 1000;
   
   type List is array (Nat range <>) of Int;
   
   
   --  The function "Sum_Acc" below is Ghost code to help the prover proof the
   --  postcondition (result) of the "Sum" function. It computes a list of
   --  partial sums. For example:
   --
   --     Input   :  [ 1  2  3  4  5  6 ]
   --     Output  :  [ 1  3  6 10 15 21 ]
   --
   --  Note that the lengths of lists are the same, the first elements are
   --  identical and the last element of the output (here: "21"), is the
   --  result of the actual function under consideration, "Sum".
   --
   --  REMARK: In this case, the input of "Sum_Acc" and "Sum" is limited
   --          to non-empty lists for convenience.
   
   type Partial_Sums is array (Nat range <>) of Integer;
   
   function Sum_Acc (L : List) return Partial_Sums with 
     Ghost,
     Pre  =>  (L'Length > 0),
     Post =>  (Sum_Acc'Result'Length = L'Length) 
     and then (Sum_Acc'Result'First = L'First) 
     and then (for all I in L'First .. L'Last =>
                 Sum_Acc'Result (I) in 
                   (I - L'First + 1) * Int'First .. 
                   (I - L'First + 1) * Int'Last)
     and then (Sum_Acc'Result (L'First) = L (L'First))
     and then (for all I in L'First + 1 .. L'Last =>
                 Sum_Acc'Result (I) = Sum_Acc'Result (I - 1) + L (I));
   
   
   function Sum (L : List) return Integer with
     Pre  => L'Length > 0,
     Post => Sum'Result = Sum_Acc (L) (L'Last);

end Sum;
