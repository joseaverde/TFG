with Detector.Algorithms, Safe_IO, Seizure_Detector_Config.Reals;
procedure FFT_Tests is
   use Detector, Detector.Algorithms, Safe_IO, Seizure_Detector_Config.Reals;
   subtype Real is Detector.Real;
   procedure Put (Item : in Complex) is
   begin
      Put (Item.Re'Image);
      if Item.Im >= 0.0 then
         Put ("+");
      end if;
      Put (Item.Im'Image);
      Put ("i");
   end Put;
   procedure Put (Item : in Complex_Array) is
   begin
      Put ("[");
      if Item'Length > 0 then
         Put (Item (Item'First));
      end if;
      for I in Item'First + 1 .. Item'Last loop
         New_Line;
         Put (",");
         Put (Item (I));
      end loop;
      Put_Line ("]");
   end Put;

   Count  : constant := 1280;
   Input  : Sample_Array (1 .. Count);
   Output : Complex_Array (1 .. Count);
begin
   Input := [for I in Input'Range => Real (I - 1)];
   FFT (Input, Output);
   Put (Output);
end FFT_Tests;
