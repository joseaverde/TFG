package Detector.Module.Fixed_Class with Pure is

   use Interfaces.C;

   -- Constructors

   Func_Zero       : constant int := 10;
   Func_From_Float : constant int := 11;
   Func_Long       : constant int := 12;
   Func_Frac       : constant int := 13;
   Func_Str        : constant int := 14;

   -- Attributes

   Func_First : constant int := 20;
   Func_Last  : constant int := 21;
   Func_Size  : constant int := 22;
   Func_Delt  : constant int := 23;
   Func_Succ  : constant int := 24;
   Func_Pred  : constant int := 25;
   Func_Pos   : constant int := 26;
   Func_Val   : constant int := 27;

   -- Binary operations

   Func_Add : constant int := 30;
   Func_Sub : constant int := 31;
   Func_Cmp : constant int := 32;

   -- Unary operations

   Func_Absf : constant int := 40;
   Func_Plus : constant int := 41;
   Func_Neg  : constant int := 42;

   -- Conversions

   Func_To_Long  : constant int := 50;
   Func_To_Str   : constant int := 51;
   Func_To_Float : constant int := 52;
   Func_As_Frac  : constant int := 53;

end Detector.Module.Fixed_Class;
