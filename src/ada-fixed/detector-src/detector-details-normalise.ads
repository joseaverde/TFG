with Detector.Details.Square_Root;
with Detector.Details.Variance;

package Detector.Details.Normalise with SPARK_Mode => On is

   use Variance, Square_Root;

   subtype Normalised_Sample is Detector.Temp.Normalised_Sample;

   pragma Assert (Normalised_Sample'Last > 0.0);
   pragma Assert (Normalised_Sample'First < 0.0);

   NL : constant Normalised_Sample := Normalised_Sample'Last;
   NF : constant Normalised_Sample := Normalised_Sample'First;

   pragma Warnings (GNATProve, Off, "postcondition does not check the outcome of calling ""Lemma_Positive_Safe_Division""",
      Reason => "It is a lemma");
   pragma Warnings (GNATProve, Off, "postcondition does not check the outcome of calling ""Lemma_Negative_Safe_Division""",
      Reason => "It is a lemma");

   procedure Lemma_Positive_Safe_Division (
      Left  : in Real;
      Right : in Sqrt_Result) with
      Ghost    => True,
      Global   => null,
      Pre      => Left > 0.0 and then Right > 0.0
         and then Left <= Real (Sample_Type'Last) - Real (Sample_Type'First)
         and then Right >= Left / NL,
      Post     => Normalised_Sample'(Left / Right) <= NL;

   procedure Lemma_Negative_Safe_Division (
      Left  : in Real;
      Right : in Sqrt_Result) with
      Ghost    => True,
      Global   => null,
      Pre      => Left < 0.0 and then Right > 0.0
         and then Left >= Real (Sample_Type'First) - Real (Sample_Type'Last)
         and then Right >= Left / NF,
      Post     => Normalised_Sample'(Left / Right) >= NF;

   function Normalise (
      Item : in Sample_Epoch)
      return Normalised_Epoch;

end Detector.Details.Normalise;
