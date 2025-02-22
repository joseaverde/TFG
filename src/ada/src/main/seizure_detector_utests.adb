with Detector, Detector.Algorithms, Safe_IO, Seizure_Detector_Config;
use Detector, Detector.Algorithms, Safe_IO, Seizure_Detector_Config;
procedure Seizure_Detector_UTests is
   procedure Get_Count is
      new Safe_IO.Generic_Get (
      Object_Type => Count_Type,
      From_String => Count_Type'Value);
   procedure Get_Sample is
      new Safe_IO.Generic_Get (
      Object_Type => Detector.Sample,
      From_String => Detector.Sample'Value);
   procedure Get_Real is
      new Safe_IO.Generic_Get (
      Object_Type => Detector.Real,
      From_String => Detector.Real'Value);
   procedure Get_Samples (Result : out Sample_Array; Valid : out Boolean) is
   begin
      Valid := True;
      for I in Result'Range loop
         Get_Sample (Result (I), Valid);
         exit when not Valid;
      end loop;
   end Get_Samples;
   procedure Get_Reals (Result : out Real_Array; Valid : out Boolean) is
   begin
      Valid := True;
      for I in Result'Range loop
         Get_Real (Result (I), Valid);
         exit when not Valid;
      end loop;
   end Get_Reals;

   procedure Put (Item : in Real_Array) is
   begin
      for I of Item loop
         Put (' ');
         Put (I'Image);
      end loop;
   end Put;

   Max_Length : constant := 48;
   Command    : String (1 .. Max_Length);
   Last       : Natural;
   Valid      : Boolean := True;
   Length     : Count_Type;
begin
   Put_Line ("Ada utests");
   Put ("stride_size          ="); Put_Line (Stride_Size'Image);
   Put ("epoch_size           ="); Put_Line (Epoch_Size'Image);
   Put ("welch_window_size    ="); Put_Line (Welch_Window_Size'Image);
   Put ("welch_window_overlap ="); Put_Line (Welch_Window_Overlap'Image);
   New_Line;
   Interactive_Loop : loop
      Get_Word (Command, Last, Valid);
      if not Valid then
         Put_Error ("INVALID COMMAND: ");
         Put_Line_Error (Command (1 .. Last));
         exit Interactive_Loop;
      end if;

      exit Interactive_Loop when Command (1 .. Last) = "Stop";

      if Command (1 .. Last) = "Simpson" then
         Get_Count (Length, Valid);
         if not Valid then
            Put_Error ("Invalid count!");
            exit Interactive_Loop;
         end if;
         declare
            Data : Real_Array (1 .. Length);
            Dx   : Sample;
         begin
            Get_Reals (Data, Valid);
            if not Valid then
               Put_Line_Error ("Invalid sample array in simpson!");
               exit Interactive_Loop;
            end if;
            Get_Sample (Dx, Valid);
            if not Valid then
               Put_Line_Error ("Invalid dx in simpson!");
               exit Interactive_Loop;
            end if;
            Put_Line (Simpson (Data, Dx)'Image);
         end;

      elsif Command (1 .. Last) = "Welch" then
         Get_Count (Length, Valid);
         if not Valid then
            Put_Error ("Invalid count!");
            exit Interactive_Loop;
         end if;
         declare
            Data : Sample_Array (1 .. Length);
            Pxx  : Welch_Array;
            Freq : Sample;
         begin
            Get_Samples (Data, Valid);
            if not Valid then
               Put_Line_Error ("Invalid sample array in welch!");
               exit Interactive_Loop;
            end if;
            Get_Sample (Freq, Valid);
            if not Valid then
               Put_Line_Error ("Invalid frequency in welch!");
               exit Interactive_Loop;
            end if;
            Welch (Data, Pxx, Welch_Window_Size / 2, Freq);
            Put (Pxx);
            New_Line;
         end;

      else
         Put_Error ("Unknown command ");
         Put_Line_Error (Command (1 .. Last));
         exit Interactive_Loop;
      end if;

   end loop Interactive_Loop;
end Seizure_Detector_UTests;
