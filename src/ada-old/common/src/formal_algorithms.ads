pragma Warnings (Off, "procedure ""To_Real"" is not referenced");
pragma Warnings (Off, "procedure ""To_Sample"" is not referenced");
pragma Warnings (Off, "function ""Energy"" is not referenced");
pragma Warnings (Off, "function ""Max_Distance"" is not referenced");
pragma Warnings (Off, "function ""Power_Spectral_Density"" is not referenced");
pragma Warnings (Off, "function ""Dynamic_Time_Warping"" is not referenced");
pragma Warnings (Off, "function ""<"" is not referenced");
pragma Warnings (Off, "function ""="" is not referenced");
generic
   type Real is private;
   type Sample is private;
   type Index_Type is range <>;
   type Sample_Array is array (Index_Type range <>) of Sample;

   with procedure To_Real (
      Image : in     String;
      Value :    out Real;
      Valid :    out Boolean) is <>;

   with procedure To_Sample (
      Image : in     String;
      Value :    out Sample;
      Valid :    out Boolean) is <>;

   with function Energy (
      Signal : in Sample_Array)
      return Real is <>;

   with function Max_Distance (
      Signal : in Sample_Array)
      return Real is <>;

   with function Power_Spectral_Density (
      Signal             : in Sample_Array;
      Sampling_Frequency : in Sample;
      Low                : in Sample;
      High               : in Sample)
      return Real is <>;

   with function Dynamic_Time_Warping (
      Signal  : in Sample_Array;
      Pattern : in Sample_Array;
      Maximum : in Real)
      return Real is <>;

   with function "<" (Left, Right : in Real) return Boolean is <>;
   with function "=" (Left, Right : in Real) return Boolean is <>;

package Formal_Algorithms with Pure, SPARK_Mode => On is

end Formal_Algorithms;
