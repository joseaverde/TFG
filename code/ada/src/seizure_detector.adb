with Ada.Unchecked_Deallocation;
with Detector.Algorithms, Detector.Load_Batch, Safe_IO;

procedure Seizure_Detector is
   use all type Detector.Count_Type;
   type Sample_Array_Access is access Detector.Sample_Array;
   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Detector.Sample_Array,
      Name   => Sample_Array_Access);
   procedure Get_Count is
      new Safe_IO.Generic_Get (
      Object_Type => Detector.Count_Type,
      From_String => Detector.Count_Type'Value);
   procedure Get_Sample is
      new Safe_IO.Generic_Get (
      Object_Type => Detector.Sample,
      From_String => Detector.Sample'Value);
   Batch  : Detector.Batch_Type;
   Valid  : Boolean := True;
   Signal : Sample_Array_Access;
   Count  : Detector.Count_Type;
   Index  : Detector.Count_Type;
begin

   Safe_IO.Put_Line ("Loading batch");
   Detector.Load_Batch (Batch, Valid);
   if not Valid then
      Safe_IO.Put_Line ("Invalid batch file!");
      return;
   end if;

   Safe_IO.Put_Line ("Loading signal");
   Get_Count (Count, Valid);
   if not Valid then
      Safe_IO.Put_Line ("Expected signal length");
      return;
   end if;
   Signal := new Detector.Sample_Array (1 .. Count);
   for I in 1 .. Count loop
      Get_Sample (Signal (I), Valid);
   end loop;

   Index := Signal.all'First;
   Safe_IO.Put_Line ("Begin of detection");
   while Index <= Count - Detector.Epoch_Size loop
      if Detector.Algorithms.Is_Seizure (
            Signal (Index .. Index + Detector.Epoch_Size - 1), Batch)
      then
         Safe_IO.Put (Detector.Count_Type'Image (
                        Index / Detector.Stride_Size));
         Safe_IO.Put_Line ("s alert!");
      end if;
      Index := Index + Detector.Stride_Size;
   end loop;
   Safe_IO.Put_Line ("End of detection");

   Free (Signal);

end Seizure_Detector;
