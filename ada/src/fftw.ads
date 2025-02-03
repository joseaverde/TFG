with Types;

package FFTW with Preelaborate, SPARK_Mode => On is

   generic
      type Float_Type is digits <>;
   package Complex_Types is
      type Complex_Type is record
         Real : Float_Type;
         Img  : Float_Type;
      end record with
      Convention => C;
      type Complex_Array is array (Positive range <>) of aliased Complex_Type;
      type Complex_Pointer is access all Complex_Type;
      type Float_Array is array (Positive range <>) of aliased Float_Type;
      type Float_Pointer is access all Float_Type;
   end Complex_Types;

   generic
      type Plan_Type is private;
      with package Complex is new Complex_Types (<>);
      with function Create (Length : in Types.Positive_Count_Type)
         return Plan_Type;
      with procedure Execute (Plan : in out Plan_Type);
      with function Result (Plan : in Plan_Type) return Complex.Complex_Array;
   package Formal is

   end Formal;

private

   type Opaque_Plan is null record with Convention => C;
   type Plan_Type is access all Opaque_Plan;

end FFTW;
