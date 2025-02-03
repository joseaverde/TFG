package body FFTW.Implement is

   FFTW_ESTIMATE : constant := 2#0100_0000#;

   function Create (Length : in Types.Positive_Count_Type) return Plan_Type is
   begin
      return Result : Plan_Type (Positive (Length)) do
         Result.Input := [others => 0.0];
         Result.Output := [others => (0.0, 0.0)];
         Result.Plan := Create (
            Length => Interfaces.C.int (Length),
            Input  => Result.Input (Result.Input'First)'Unchecked_Access,
            Output => Result.Output (Result.Output'First)'Unchecked_Access,
            Flags  => FFTW_ESTIMATE);
      end return;
   end Create;

   function Execute (Plan : in Plan_Type) return Complex.Complex_Array is
   begin
      Execute (Plan.Plan);
      return Plan.Output;
   end Execute;

   overriding procedure Finalize (Object : in out Plan_Type) is
   begin
      if Object.Plan /= null then
         Destroy (Object.Plan);
      end if;
   end Finalize;

end FFTW.Implement;
