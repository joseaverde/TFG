with Detector.Signals.Fast_Fourier_Transform_Details;

procedure Detector.Signals.Fast_Fourier_Transform (
   Input  : in     Signal_Type;
   Output :    out Complex_Signal;
   Power  : in     Natural;
   Scale  :    out Natural) is
   pragma SPARK_Mode (On);

   use Detector.Signals.Fast_Fourier_Transform_Details;

   Buffer : Double_Buffer (Boolean, 0 .. Output'Length - 1);
   Result : Boolean             := True;
   Layer  : Positive_Count_Type := 1;
   Scaled : Boolean;
   Sizes  : constant Chunk_Size_Array := Chunk_Sizes with Ghost;

begin

   Buffer := [True  => [for I in Input'Range => (Input (I), 0.0)],
              False => [for I in Input'Range => (0.0, 0.0)]];
   Scale := 0;

   pragma Assert (Input'Length = Sizes (Power));
   for I in 0 .. Power loop
      pragma Loop_Invariant (Layer = Sizes (I));
      pragma Loop_Invariant (Scale in 0 .. I);
      pragma Loop_Variant (Increases => Layer);
      exit when Layer > Input'Length;
      pragma Assert (Layer = 2 ** I);
      pragma Assert (Input'Length = 2 ** Power);
      pragma Assert (Input'Length >= Layer);
      pragma Assert (Power >= I);
      Lemma_Power_Of_Two_Module_Another_Lower_Power_Of_Two_Is_Zero (Power, I);
      Conquer (Buffer, Scaled, Layer, Result);
      if Scaled then
         Scale := Scale + 1;
      end if;
      Result := not Result;
      Layer := Layer * 2;
   end loop;

   pragma Assert (Buffer'Length (2) = Output'Length);
   Output := [for I in Output'Range => Buffer (Result, I - Output'First)];

end Detector.Signals.Fast_Fourier_Transform;
