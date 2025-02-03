private with FFTW.Implement;
private with Interfaces.C;

package FFTW.Double with Preelaborate, SPARK_Mode => On is

   type Plan_Type (<>) is limited private;

   package Complex is new Complex_Types (Long_Float);
   function Create (Length : in Types.Positive_Count_Type) return Plan_Type;
   function Execute (Plan : in Plan_Type) return Complex.Complex_Array;

private

   pragma SPARK_Mode (Off);

   function dft_r2c_1d (
      Length : in Interfaces.C.int;
      Input  : in Complex.Float_Pointer;
      Output : in Complex.Complex_Pointer;
      Flags  : in Interfaces.C.unsigned)
      return FFTW.Plan_Type with
      Post          => dft_r2c_1d'Result /= null,
      Global        => null,
      Import        => True,
      Convention    => C,
      External_Name => "fftw_plan_dft_r2c_1d";

   procedure fftwf_execute (Plan : in FFTW.Plan_Type) with
      Pre           => Plan /= null,
      Global        => null,
      Import        => True,
      Convention    => C,
      External_Name => "fftw_execute",
      Always_Terminates;

   procedure fftwf_destroy_plan (Plan : in out FFTW.Plan_Type) with
      Pre           => Plan /= null,
      Post          => Plan = null,
      Global        => null,
      Import        => True,
      Convention    => C,
      External_Name => "fftw_destroy_plan",
      Always_Terminates;

   package Impl is new Implement (
      Complex => Complex,
      Create  => dft_r2c_1d,
      Execute => fftwf_execute,
      Destroy => fftwf_destroy_plan);

   type Plan_Type (Size : Positive) is limited record
      Plan : Impl.Plan_Type (Size);
   end record;

   function Create (Length : in Types.Positive_Count_Type)
      return Plan_Type is (
      Size => Positive (Length),
      Plan => Impl.Create (Length));

   function Execute (Plan : in Plan_Type)
      return Complex.Complex_Array is (
      Impl.Execute (Plan.Plan));

end FFTW.Double;
