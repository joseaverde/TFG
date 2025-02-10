with Signals;
private with FFTW;

package Welch with
   SPARK_Mode     => On,
   Abstract_State => The_Window,
   Initializes    => The_Window
is

   pragma Elaborate_Body;

   subtype Freq_Array is array Sample_Array (1 .. Epoch_Size / 2 + 1);

   type Welch_Type is limited private;

private

   type Welch_Type is limited record
      Plan : FFTW.Plan_Type;
   end record;

end Welch;
