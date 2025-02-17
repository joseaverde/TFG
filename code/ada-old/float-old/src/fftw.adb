with FFTW.Details, Interfaces.C;

package body FFTW is

   FFTW_ESTIMATE : constant := 2#0100_0000#;

   function Create return Plan_Type is
   begin
      return Result : Plan_Type do
         Result.Input := [others => 0.0];
         Result.Output := [others => (0.0, 0.0)];
         Result.Pointer := Details.Create (
            Length => Interfaces.C.int (Signals.Epoch_Size),
            Input  => Result.Input (Result.Input'First)'Unchecked_Access,
            Output => Result.Output (Result.Output'First)'Unchecked_Access,
            Flags  => FFTW_ESTIMATE);
      end return;
   end Create;

   procedure Execute (
      Plan      : in out Plan_Type;
      In_Signal : in     Signals.Signal;
      In_Epoch  : in     Signals.Epoch_Span;
      Result    :    out Complex_Array) is
   begin
      for I in Signals.Count_Type range 1 .. Signals.Epoch_Size loop
         Plan.Input (I) := In_Signal (In_Epoch, I);
      end loop;
      Details.Execute (Plan.Pointer);
      Result := Plan.Output;
   end Execute;

   overriding procedure Finalize (Object : in out Plan_Type) is
   begin
      Details.Destroy (Object.Pointer);
      Object.Pointer := null;
   end Finalize;

end FFTW;
