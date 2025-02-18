with Interfaces.C;
private package FFTW.Details with Preelaborate, SPARK_Mode => Off is

   pragma Warnings (Off,
      """Always_Terminates"" is not a valid aspect identifier");

   function Create (
      Length : in Interfaces.C.int;
      Input  : in Real_Access;
      Output : in Complex_Access;
      Flags  : in Interfaces.C.unsigned)
      return Plan_Pointer with
      Post          => Create'Result /= null,
      Global        => null,
      Import        => True,
      Convention    => C,
      External_Name => "fftw_plan_dft_r2c_1d";

   procedure Execute (Plan : in Plan_Pointer) with
      Pre           => Plan /= null,
      Global        => null,
      Import        => True,
      Convention    => C,
      External_Name => "fftw_execute",
      Always_Terminates;

   procedure Destroy (Plan : in out Plan_Pointer) with
      Pre           => Plan /= null,
      Post          => Plan = null,
      Global        => null,
      Import        => True,
      Convention    => C,
      External_Name => "fftw_destroy_plan",
      Always_Terminates;

end FFTW.Details;
