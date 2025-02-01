with Ada.Text_IO;
with CLI;
with Generic_Detector, Generic_Batchs, Generic_Loader, Generic_Signals;
with Types;
with Generic_Real_Sum;
procedure Detect with SPARK_Mode => On is

   subtype Real is Long_Float;

   package T_IO renames Ada.Text_IO;
   package Signals is new Generic_Signals (Real);
   package Batchs is new Generic_Batchs (Signals);
   package Detector is new Generic_Detector (Signals, Batchs);
   procedure Loader is new Generic_Loader (Signals, Batchs);

   subtype Mini_Real is Real range -100_000.0 .. 100_000.0;
   type Real_Array is array (Positive range <>) of Real;
   package Sum is new Generic_Real_Sum (
      Size       => 1280,
      Index_Type => Positive,
      Real       => Real,
      Real_Array => Real_Array,
      First      => Mini_Real'First,
      Last       => Mini_Real'Last);

   use type Types.Count_Type, Signals.Signal_Access, Batchs.Batch_Access;

   Signal : Signals.Signal_Access;
   Batch  : Batchs.Batch_Access;
   Stride : Signals.Stride_Span := (1, Signals.Stride_Size);

begin
   if CLI.Count /= 1 then
      T_IO.Put ("USAGE: `");
      T_IO.Put (CLI.Command_Name);
      T_IO.Put (" signal batch'");
      T_IO.New_Line;
      return;
   end if;
   Loader (CLI.Argument (1), Signal, Batch);
   Detector.Reset;
   pragma Assert (Detector.Invariant);
   if Signal /= null and then Batch /= null then
      Detection_Loop : while Signal.Is_Valid_Span (Stride) loop
         pragma Loop_Invariant (Signal.Is_Valid_Span (Stride));
         pragma Loop_Invariant (Detector.Invariant);
         Detector.Write (Signal.all, Stride);
         if Detector.Is_Seizure (Batch.all) then
            T_IO.Put ("Seizure at");
            T_IO.Put (Types.Count_Type'Image (
               Stride.First / Signals.Stride_Size));
            T_IO.Put_Line (" seconds!");
         end if;
         -- Increment if no overflow
         if Stride.Last <= Types.Index_Type'Last - Signals.Stride_Size then
            Stride := (@.Last + 1, @.Last + Signals.Stride_Size);
         else
            exit Detection_Loop;
         end if;
      end loop Detection_Loop;
   else
      T_IO.Put_Line ("Couldn't load the signal!!!");
   end if;
   Signals.Free (Signal);
   Batchs.Free (Batch);
end Detect;
