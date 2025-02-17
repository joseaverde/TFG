with Float_Algorithms, Generic_Functional_Test;
procedure Main is
   procedure Functional_Test is
      new Generic_Functional_Test (Float_Algorithms.Formal);
begin
   Functional_Test;
end Main;
