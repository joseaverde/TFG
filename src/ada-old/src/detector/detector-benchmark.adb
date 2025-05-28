with Safe_IO, Safe_Time, Detector.Algorithms, Seizure_Detector_Config;
use Safe_Time, Detector.Algorithms, Seizure_Detector_Config;

procedure Detector.Benchmark is
   Stride_Count  : constant := 100;
   Pattern_Count : constant := 3;
   The_Batch     : constant Batch_Type := (
      Count    => Pattern_Count,
      PSD_1    => (Real'First, Real'Last),
      PSD_2    => (Real'First, Real'Last),
      PSD_3    => (Real'First, Real'Last),
      Energy   => (Real'First, Real'Last),
      Max_Dist => (Real'First, Real'Last),
      d_max_c  => Real'First,
      Patterns => [for I in Pattern_Index range 1 .. Pattern_Count =>
                     [for J in Count_Type range 1 .. Epoch_Size =>
                        Real (Count_Type (I - 1) * Epoch_Size + J)]]);
   Epoch      : Epoch_Array;
   Detections : Count_Type := 0;

   procedure Read_Stride (
      Part : in     Count_Type;
      Item :    out Stride_Array) is
   begin
      for I in Item'Range loop
         Item (I) := Real (I + Part mod 256);
      end loop;
   end Read_Stride;

   procedure Put (Item : in Duration) is
   begin
      Safe_IO.Put (Item'Image);
   end Put;

   Start, Stop : Time;

begin

   Start := Clock;
   for Stride in Count_Type range 1 .. Strides_Per_Epoch - 1 loop
      Read_Stride (Stride,
         Epoch (1 + (Stride - 1) * Stride_Size .. Stride * Stride_Size));
   end loop;
   for Stride in Count_Type range Strides_Per_Epoch .. Stride_Count loop
      Safe_IO.Put ("Second"); Safe_IO.Put (Stride'Image);
      Epoch (Epoch'First + Stride_Size .. Epoch'Last) :=
         Epoch (Epoch'First .. Epoch'Last - Stride_Size);
      Read_Stride (Stride,
         Epoch (Epoch'First .. Epoch'First + Stride_Size - 1));
      if Is_Seizure (Epoch, The_Batch) then
         Safe_IO.Put (Character'Val (27) & "[31;1m[SEIZURE]");
         Detections := @ + 1;
      end if;
      Safe_IO.New_Line;
   end loop;
   Stop := Clock;

   Safe_IO.Put ("Elapsed ");
   Put (Stop - Start);
   Safe_IO.Put_Line (" s");

   Safe_IO.Put (Duration'Image (Duration (
      Float (Stride_Count - Strides_Per_Epoch + 1) / Float (Stop - Start))));
   Safe_IO.Put_Line (" epochs/second");

   Safe_IO.Put (Duration'Image (Duration (
      Float (Stop - Start) / Float (Stride_Count - Strides_Per_Epoch + 1))));
   Safe_IO.Put_Line (" seconds/epoch");

   Safe_IO.Put_Line (Detections'Image);

end Detector.Benchmark;
