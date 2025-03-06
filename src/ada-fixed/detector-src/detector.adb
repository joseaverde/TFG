with Detector.Details.Dynamic_Time_Warping;
with Detector.Details.Energy;
with Detector.Details.Max_Distance;
with Detector.Details.Mean;
with Detector.Details.Simpson;
with Detector.Details.Welch;

use Detector.Details.Dynamic_Time_Warping;
use Detector.Details.Energy;
use Detector.Details.Max_Distance;
use Detector.Details.Mean;
use Detector.Details.Simpson;
use Detector.Details.Welch;

package body Detector with SPARK_Mode => On is

   pragma Warnings (Off, "postcondition does not check the outcome of calling");
   pragma Warnings (Off, "static fixed-point value is not a multiple of Small");

   -->> Max distance <<--

   function Max_Distance (
      Item : in Sample_Epoch)
      return Feature_Type is
      Max : Sample_Type := Item (Item'First);
      Min : Sample_Type := Item (Item'First);
   begin
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Max = Acc_Maximum (Item) (Index - 1));
         pragma Loop_Invariant (Min = Acc_Minimum (Item) (Index - 1));
         Max := Sample_Type'Max (Item (Index), Max);
         Min := Sample_Type'Min (Item (Index), Min);
      end loop;
      pragma Assert (Max = Maximum (Item));
      pragma Assert (Min = Minimum (Item));
      return Feature_Type (Max) - Feature_Type (Min);
   end Max_Distance;

   -->> Energy <<--

   function Energy (
      Item : in Sample_Epoch)
      return Feature_Type is
      subtype Real is Details.Energy.Real;
      subtype Real_Epoch is Details.Energy.Real_Epoch;
      μ      : constant Real := Real (Mean (Item));
      Value  : Real;
      Result : Real := 0.0;
      As_Arr : constant Real_Epoch := Scale_Array (Item, μ) with Ghost;
   begin

      for I in Item'Range loop
         Value := Real (Item (I)) - μ;
         pragma Assert (Value in S_First - S_Last .. S_Last - S_First);
         Value := Value * Value;
         pragma Assert (Value in 0.0 .. Details.Energy.Max_Sq);
         pragma Assert (Value = (Real (Item (I)) - μ) * (Real (Item (I)) - μ));
         pragma Assert (Value = As_Arr (I));
         Result := Result + Value;
         pragma Loop_Invariant (Result = Acc_Energy_Sum (As_Arr) (I));
      end loop;

      pragma Assert (Result >= 0.0);
      Result := Result / Real (Item'Length);
      pragma Assert (Result >= 0.0);
      return Feature_Type ((if Result > F_Last then F_Last else Result));
   end Energy;

   -->> Dynamic Time Warping <<--

   function Dynamic_Time_Warping (
      Signal  : in Sample_Epoch;
      Pattern : in Sample_Epoch;
      Maximum : in Feature_Type)
      return Feature_Type is
   begin
      return Single_Dynamic_Time_Warping (
         Signal  => Normalise (Signal),
         Pattern => Normalise (Pattern),
         Max     => Maximum);
   end Dynamic_Time_Warping;

   -->> Power Spectral Density

   procedure Power_Spectral_Density (
      Signal              : in     Sample_Epoch;
      Sampling_Frequency  : in     Feature_Type;
      PSD_1, PSD_2, PSD_3 :    out Feature_Type) is
      Fq_Res : constant Feature_Type := Sampling_Frequency
                                       / Sample_Type (Welch_Size);
      First  : Count_Type;
      Last   : Count_Type;
      Pxx    : Welch_Array;
   begin
      Welch (Signal, Pxx, Welch_Overlap, Sampling_Frequency);
      -- PSD_1
      First := Count_Type (Feature_Type (2.0)  / Fq_Res);
      Last  := Count_Type (Feature_Type (12.0) / Fq_Res);
      PSD_1 := Simpson (
         Pxx (First + Signal'First .. Last + Signal'First - 1), Fq_Res);
      -- PSD_2
      First := Count_Type (Feature_Type (12.0) / Fq_Res);
      Last  := Count_Type (Feature_Type (18.0) / Fq_Res);
      PSD_2 := Simpson (
         Pxx (First + Signal'First .. Last + Signal'First - 1), Fq_Res);
      -- PSD_3
      First := Count_Type (Feature_Type (18.0) / Fq_Res);
      Last  := Count_Type (Feature_Type (35.0) / Fq_Res);
      PSD_3 := Simpson (
         Pxx (First + Signal'First .. Last + Signal'First - 1), Fq_Res);
   end Power_Spectral_Density;

   -->> Is_Seizure <<--

   function Within (
      Item : in Feature_Type;
      Span : in Real_Span)
      return Boolean is (
      Span.Low <= Item and then Item <= Span.High);

   function Is_Seizure (
      Item  : in Sample_Epoch;
      Batch : in Batch_Type)
      return Boolean is
      PSD_1, PSD_2, PSD_3 : Feature_Type;
   begin
      if not Within (Max_Distance (Item), Batch.Max_Dist)
         or else not Within (Energy (Item), Batch.Energy)
      then
         return False;
      end if;
      Power_Spectral_Density (Item, Feature_Type (Stride_Size),
         PSD_1, PSD_2, PSD_3);
      if not Within (PSD_1, Batch.PSD_1)
         or else not Within (PSD_2, Batch.PSD_2)
         or else not Within (PSD_3, Batch.PSD_3)
      then
         return False;
      end if;

      return
         (for some I in 1 .. Batch.Count =>
            Dynamic_Time_Warping (Item, Batch.Patterns (I), Batch.d_max_c)
            <= Batch.d_max_c);
   end Is_Seizure;

end Detector;
