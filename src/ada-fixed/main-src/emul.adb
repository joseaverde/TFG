with Ada.Text_IO, Detector, Interfaces.C;
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

   Old : Interfaces.C.int := 10;

   procedure eeg_read_sample (
      Numerator   : out Interfaces.C.int;
      Denominator : out Interfaces.C.int) is
   begin
      Denominator := 2 ** Detector.Sample_Mantissa;
      Numerator := Old;
      if Old > 10_000 then
         Old := -10_000;
      else
         Old := Old + 1;
      end if;
   end eeg_read_sample;

   procedure eeg_putchar (
      Item : in Character) is
   begin
      Ada.Text_IO.Put (Item);
   end eeg_putchar;

end Emul;
