with Ada.Text_IO, Ada.Real_Time, Detector, Interfaces.C;
package body Emul with SPARK_Mode => On is

   pragma Warnings (GNATProve, Off,
      "analyzing unreferenced procedure ""eeg_read_sample""",
      Reason => "It is exported it should be implemented in C");
   pragma Warnings (GNATProve, Off,
      "analyzing unreferenced procedure ""eeg_putchar""",
      Reason => "It is exported it should be implemented in C");

   use type Interfaces.C.int;

   procedure eeg_read_sample (
      Numerator   : out Interfaces.C.int;
      Denominator : out Interfaces.C.int) with
      Export        => True,
      Convention    => C,
      External_Name => "eeg_read_sample";

   procedure eeg_putchar (
      Item : in Character) with
      Export        => True,
      Convention    => C,
      External_Name => "eeg_putchar";

   -->> Implementation <<--

   Epochs : constant := 1000;
   type Counter is mod 256 * Epochs;

   Old    : Interfaces.C.int := 10;
   Start  : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   Count  : Counter := 0;

   procedure Put_Elapsed (Item : in Duration) is
   begin
      if Item = 0.0 then
         Ada.Text_IO.Put (" | Infinite ");
      elsif Item < 0.0 then
         Ada.Text_IO.Put (" | ¡Negative time! ");
      elsif Item < Duration (2 * Epochs) / Duration'Last then
         Ada.Text_IO.Put (" | Too many ");
      else
         Ada.Text_IO.Put (" |");
         Ada.Text_IO.Put (Duration'Image (Duration (Epochs) / Item));
      end if;
      Ada.Text_IO.Put_Line (" epochs/second");
   end Put_Elapsed;

   procedure eeg_read_sample (
      Numerator   : out Interfaces.C.int;
      Denominator : out Interfaces.C.int) is
      use Ada.Real_Time;
      Stop    : Time;
      Elapsed : Time_Span;
   begin
      Denominator := 2 ** Detector.Sample_Mantissa;
      Numerator := Old;
      if Old > 10_000 then
         Old := -10_000;
      else
         Old := Old + 1;
      end if;
      Count := Count + 1;
      if Count = 0 then
         Stop := Ada.Real_Time.Clock;
         Elapsed := Stop - Start;
         Put_Elapsed (To_Duration (Elapsed));
         Start := Stop;
      end if;
   end eeg_read_sample;

   procedure eeg_putchar (
      Item : in Character) is
   begin
      Ada.Text_IO.Put (Item);
   end eeg_putchar;

end Emul;
