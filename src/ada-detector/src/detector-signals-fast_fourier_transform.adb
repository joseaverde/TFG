with Detector.Signals.Fast_Fourier_Transform_Details;

procedure Detector.Signals.Fast_Fourier_Transform (
   Input  : in     Signal_Type;
   Output :    out Complex_Signal;
   Power  : in     Natural;
   Scale  :    out Positive) is
   pragma SPARK_Mode (On);

   use Detector.Signals.Fast_Fourier_Transform_Details;

   Buffer : Double_Buffer (Boolean, 0 .. Output'Length - 1);
   Result : Boolean             := True;
   Layer  : Positive_Count_Type := 1;
   -- Scaled : Normalised_Sample_Window;
   -- Factor : Scaling_Factor;
   -- Shifts : Shift_Count := 0;
   -- Chunk  : Positive_Count_Type := 1;
   -- Shift  : Boolean;

begin

   Buffer := [True  => [for I in Input'Range => (Input (I), 0.0)],
              False => [for I in Input'Range => (0.0, 0.0)]];
   Scale := 1;

   while Layer < Input'Length loop
      Result := not Result;
      Layer := Layer * 2;
      pragma Loop_Variant (Increases => Layer);
   end loop;

   pragma Assert (Buffer'Length (2) = Output'Length);
   Output := [for I in Output'Range => Buffer (Result, I - Output'First)];

end Detector.Signals.Fast_Fourier_Transform;
