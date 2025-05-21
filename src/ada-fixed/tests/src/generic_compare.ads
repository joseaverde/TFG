generic
   type Fixed_Type is delta <>;
   ε : in Fixed_Type;
function Generic_Compare (Left, Right : in Fixed_Type) return Boolean with
   Inline => True,
   Global => null,
   Pure;
