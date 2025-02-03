with Ada.Finalization
private generic
   with package Complex is new Complex_Types (<>);
   with function Create (
      Length : in Interfaces.C.int,
      Input  : in Complex.Float_Pointer;
      Output : in Complex.Complex_Pointer;
      Flags  : in Interfaces.C.unsigned)
      return Plan_Type with
      Post => Create'Result /= null;
   with procedure Execute (Plan : in Plan_Type) with
      Pre => Plan /= null;
   with procedure Destroy (Plan : in out Plan_Type) with
      Pre => Plan /= null,
      Post => Plan = null;
package FFTW.Implement with Preelaborate, SPARK_Mode => On is

   type Plan_Type is private;

private

end FFTW.Implement;
