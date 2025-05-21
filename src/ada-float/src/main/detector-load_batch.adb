with Safe_IO;

procedure Detector.Load_Batch (
   Result :    out Batch_Type;
   Valid  : in out Boolean) is
   pragma SPARK_Mode (On);
   procedure Get_Count is
      new Safe_IO.Generic_Get (
      Object_Type => Pattern_Index,
      From_String => Pattern_Index'Value);
   procedure Get is new Safe_IO.Generic_Get (Real, Real'Value);
   Count : Pattern_Index;
begin
   Get_Count (Count, Valid);
   Result := Batch_Type'(Count => Count, others => <>);
   Get (Result.PSD_1.Low, Valid);    Get (Result.PSD_1.High, Valid);
   Get (Result.PSD_2.Low, Valid);    Get (Result.PSD_2.High, Valid);
   Get (Result.PSD_3.Low, Valid);    Get (Result.PSD_3.High, Valid);
   Get (Result.Energy.Low, Valid);   Get (Result.Energy.High, Valid);
   Get (Result.Max_Dist.Low, Valid); Get (Result.Max_Dist.High, Valid);
   Get (Result.d_max_c, Valid);
   if Valid then
      for P in 1 .. Result.Count loop
         for I in Pattern_Type'Range loop
            Get (Result.Patterns (P) (I), Valid);
            exit when not Valid;
         end loop;
         exit when not Valid;
      end loop;
   end if;
end Detector.Load_Batch;
