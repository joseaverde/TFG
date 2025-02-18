package body Float_Algorithms with SPARK_Mode => On is

   procedure To_Real (
      Image : in     String;
      Value :    out Real;
      Valid :    out Boolean) is
   begin
      Value := Real'Value (Image);
      Valid := True;
   exception
      when others =>
         Value := 0.0;
         Valid := False;
   end To_Real;

   procedure To_Sample (
      Image : in     String;
      Value :    out Sample;
      Valid :    out Boolean) is
   begin
      Value := Sample'Value (Image);
      Valid := True;
   exception
      when others =>
         Value := 0.0;
         Valid := False;
   end To_Sample;

   function Mean (
      Signal : in Sample_Array)
      return Sample is
      Result : Real := Signal (Signal'First);
   begin
      for I in Signal'First + 1 .. Signal'Last loop
         Result := Result + Signal (I);
      end loop;
      return Result / Real (Signal'Length);
   end Mean;

   function Energy (
      Signal : in Sample_Array)
      return Real is
      Mean   : constant Sample := Float_Algorithms.Mean (Signal);
      Result : Real := 0.0;
   begin
      for I in Signal'Range loop
         Result := Result + (Signal (I) - Mean) ** 2;
      end loop;
      return Result / Real (Signal'Length);
   end Energy;

   function Max_Distance (
      Signal : in Sample_Array)
      return Real is
      Min : Sample := Signal (Signal'First);
      Max : Sample := Signal (Signal'First);
   begin
      pragma Assert (Min <= Max);
      for I in Signal'First + 1 .. Signal'Last loop
         Min := Sample'Min (Min, Signal (I));
         Max := Sample'Max (Max, Signal (I));
         pragma Loop_Invariant (Min <= Max);
      end loop;
      return Real (Max) - Real (Min);
   end Max_Distance;

   function Power_Spectral_Density (
      Signal             : in Sample_Array;
      Sampling_Frequency : in Sample;
      Low                : in Sample;
      High               : in Sample)
      return Real is (0.0);

   function Dynamic_Time_Warping (
      Signal  : in Sample_Array;
      Pattern : in Sample_Array;
      Maximum : in Real)
      return Real is (0.0);

end Float_Algorithms;
