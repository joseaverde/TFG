with Ada.Calendar, Detector, Detector.Algorithms, Seizure_Detector_Config;
use Ada.Calendar, Detector, Detector.Algorithms, Seizure_Detector_Config;
with Safe_IO;

procedure Seizure_Detector_Benchmark is
   Stride_Count  : constant := 10_000;
   Pattern_Count : constant := 3;
   The_Batch     : constant Batch_Type := (
      Count    => Pattern_Count,
      PSD_1    => (Real'First, Real'Last),
      PSD_2    => (Real'First, Real'Last),
      PSD_3    => (Real'First, Real'Last),
      Energy   => (Real'First, Real'Last),
      Max_Dist => (Real'First, Real'Last),
      d_max_c  => Real'Last,
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
      Epoch (Epoch'First + Stride_Size .. Epoch'Last) :=
         Epoch (Epoch'First .. Epoch'Last - Stride_Size);
      Read_Stride (Stride,
         Epoch (Epoch'First .. Epoch'First + Stride_Size - 1));
      if Is_Seizure (Epoch, The_Batch) then
         Detections := @ + 1;
      end if;
   end loop;
   Stop := Clock;

   Safe_IO.Put ("Elapsed ");
   Put (Stop - Start);
   Safe_IO.Put_Line (" s");

   Safe_IO.Put (Count_Type'Image (
      Count_Type (Float (Stride_Count) / Float (Stop - Start))));
   Safe_IO.Put_Line (" epochs/second");

   Safe_IO.Put_Line (Detections'Image);

end Seizure_Detector_Benchmark;
