with Common, Formal_Algorithms, Reals;

package Float_Algorithms with Preelaborate, SPARK_Mode => On is

   subtype Real is Reals.Real;
   subtype Sample is Real range -10_000.0 .. 10_000.0;
   subtype Index_Type is Positive;
   type Sample_Array is array (Index_Type range <>) of Sample;

   procedure To_Real (
      Image : in     String;
      Value :    out Real;
      Valid :    out Boolean);

   procedure To_Sample (
      Image : in     String;
      Value :    out Sample;
      Valid :    out Boolean);

   function Energy (
      Signal : in Sample_Array)
      return Real with
      Pre => Signal'Length = Common.Epoch_Size;

   function Max_Distance (
      Signal : in Sample_Array)
      return Real with
      Pre => Signal'Length = Common.Epoch_Size;

   function Power_Spectral_Density (
      Signal             : in Sample_Array;
      Sampling_Frequency : in Sample;
      Low                : in Sample;
      High               : in Sample)
      return Real;

   function Dynamic_Time_Warping (
      Signal  : in Sample_Array;
      Pattern : in Sample_Array;
      Maximum : in Real)
      return Real;

   package Formal is
      new Formal_Algorithms (Real, Sample, Index_Type, Sample_Array);

end Float_Algorithms;
