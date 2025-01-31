package body Sum with SPARK_Mode is

   -------------
   -- Sum_Acc --
   -------------
            
   function Sum_Acc (L : List) return Partial_Sums is
      PS : Partial_Sums (L'Range) := (others => 0);
   begin
      
      PS (L'First) := L (L'First);
      
      for Index in L'First + 1 .. L'Last loop
      
         --  Head equal.
         pragma Loop_Invariant
           (PS (L'First) = L (L'First));
         
         --  Tail equal.
         pragma Loop_Invariant
           (for all I in L'First + 1 .. Index - 1 =>
              PS (I) = PS (I - 1) + L (I)); 
         
         --  NOTE: The loop invariant below holds only when the range of "Int" 
         --        is symmetric, i.e -Int'First = Int'Last. If not, then this
         --        loop invariant may have to be adjusted.
         
         --  Result within bounds.
         pragma Loop_Invariant 
           (for all I in L'First .. Index - 1 =>
              PS (I) in (I - L'First + 1) * Int'First ..
                        (I - L'First + 1) * Int'Last);
               
         PS (Index) := PS (Index - 1) + L (Index);
      
      end loop;
      
      return PS;
      
   end Sum_Acc;

   ---------
   -- Sum --
   ---------
   
   function Sum (L : List) return Integer is
      Result : Integer := L (L'First);
   begin
      
      for I in L'First + 1 .. L'Last loop
         
         pragma Loop_Invariant
           (Result = Sum_Acc (L) (I - 1));
         
         Result := Result + L (I);
        
      end loop;
      
      return Result;
      
   end Sum;

end Sum;
