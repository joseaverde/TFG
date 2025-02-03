private with Ada.Finalization;
with Interfaces.C;
private generic
   with package Complex is new Complex_Types (<>);
   with function Create (
      Length : in Interfaces.C.int;
      Input  : in Complex.Float_Pointer;
      Output : in Complex.Complex_Pointer;
      Flags  : in Interfaces.C.unsigned)
      return Plan_Type; -- with
   -- Post => Create'Result /= null;
   with procedure Execute (Plan : in Plan_Type); -- with
   -- Pre => Plan /= null;
   with procedure Destroy (Plan : in out Plan_Type); -- with
   -- Pre => Plan /= null,
   -- Post => Plan = null;
package FFTW.Implement with Preelaborate, SPARK_Mode => Off is

   type Plan_Type (Last : Positive) is limited private;

   function Create (Length : in Types.Positive_Count_Type) return Plan_Type;
   function Execute (Plan : in Plan_Type) return Complex.Complex_Array;

private

   pragma SPARK_Mode (Off);

   type Plan_Type (Last : Positive) is
      new Ada.Finalization.Limited_Controlled with
   record
      Plan   : FFTW.Plan_Type;
      Input  : Complex.Float_Array (1 .. Last);
      Output : Complex.Complex_Array (1 .. Last);
   end record;

   overriding procedure Finalize (Object : in out Plan_Type);

end FFTW.Implement;
