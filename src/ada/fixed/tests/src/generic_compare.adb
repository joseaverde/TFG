function Generic_Compare (Left, Right : in Fixed_Type) return Boolean is
begin
   return abs (Left - Right) < ε;
end Generic_Compare;
