package body Detector.Details.Simpson with SPARK_Mode => Off is

   function Simpson (
      Signal : in Feature_Array;
      dx     : in Feature_Type)
      return Feature_Type is
      Result : Feature_Type := 0.0;
      I      : Count_Type := Signal'First + 2;
   begin
      while I in Signal'Range loop
         Result := @ + Signal (I - 2) + 4.0 * Signal (I - 1) + Signal (I);
         I := I + 2;
      end loop;
      Result := Feature_Type (Result * dx) / 3.0;
      if Signal'Length > 2 and then Signal'Length mod 2 = 0 then
         Result := Result + Feature_Type (dx * (
                  5.0 * Signal (Signal'Last)
               + 8.0 * Signal (Signal'Last - 1)
               -       Signal (Signal'Last - 2))) / 12.0;
      end if;
      return Result;
   end Simpson;

end Detector.Details.Simpson;
