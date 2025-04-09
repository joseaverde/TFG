with Detector.Signals.Fast_Fourier_Transform_Details;

procedure Detector.Signals.Fast_Fourier_Transform (
   Input  : in     Signal_Type;
   Output :    out Complex_Signal;
   Power  : in     Natural;
   Scale  :    out Positive) is

   use Detector.Signals.Fast_Fourier_Transform_Details;

   Buffer : Double_Buffer (Boolean, 0 .. Output'Length - 1);
   Result : Boolean    := True;
   Layer  : Count_Type := 1;
   -- Scaled : Normalised_Sample_Window;
   -- Factor : Scaling_Factor;
   -- Shifts : Shift_Count := 0;
   -- Chunk  : Positive_Count_Type := 1;
   -- Shift  : Boolean;

begin

   Buffer := [True  => [for I in Input'Range => (Input (I), 0.0)],
              False => [for I in Input'Range => (0.0, 0.0)]];

   while Layer < Input'Length loop
      Result := not Result;
      Layer := Layer * 2;
   end loop;

   Output := [for I in Buffer'Range (2) => Buffer (Result, I)];

end Detector.Signals.Fast_Fourier_Transform;
