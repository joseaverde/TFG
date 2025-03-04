with Detector.Details.Max_Distance; use Detector.Details.Max_Distance;
with Detector.Details.Trigonometric; use Detector.Details.Trigonometric;
with Detector.Details.Fourier_Transform;
use Detector.Details.Fourier_Transform;
with Detector.Details.Mean; use Detector.Details.Mean;

package body Detector with SPARK_Mode => On is

   pragma Warnings (Off, "postcondition does not check the outcome of calling");
   pragma Warnings (Off, "static fixed-point value is not a multiple of Small");

   -->> Windows <<--

   type Trigonometric_Output_Array is
      array (Count_Type range 0 .. Welch_Size - 1)
      of Trigonometric_Output_Type;

   subtype TOT is Trigonometric_Output_Type;
   subtype TIT is Trigonometric_Input_Type;

   Hann_Window : constant Trigonometric_Output_Array :=
      [for I in Trigonometric_Output_Array'Range =>
         TOT (0.5) - TOT (0.5)
         * Cos (TIT (TIT (TIT (2.0) * TIT (I)) * TIT (π)) / TIT (Welch_Size))];
   pragma Assert (
      (for all I in Hann_Window'Range =>
         Hann_Window (I) in 0.0 .. 1.0));

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

   --

   package Unproved is

      -- TODO: Prove :)

      type Feature_Array is
         array (Positive_Count_Type range <>) of Feature_Type;
      subtype Welch_Array is Feature_Array (1 .. Welch_Size);
      Warping_Window : constant := 16;

      function Energy (
         Signal : in Sample_Epoch)
         return Feature_Type;

      procedure Power_Spectral_Density (
         Signal              : in     Sample_Epoch;
         Sampling_Frequency  : in     Feature_Type;
         PSD_1, PSD_2, PSD_3 :    out Feature_Type) with
         Always_Terminates;

      function Dynamic_Time_Warping (
         Signal  : in Sample_Array;
         Pattern : in Sample_Array;
         Maximum : in Feature_Type)
         return Feature_Type'Base;

   end Unproved;

   package body Unproved is

      pragma SPARK_Mode (Off);

      function Squared (Item : in Feature_Type)
         return Feature_Type is (Item * Item);

      function Energy (
         Signal : in Sample_Epoch)
         return Feature_Type is
         μ      : constant Sample_Type := Mean (Signal);
         Result : Feature_Type := 0.0;
      begin
         for I in Signal'Range loop
            Result := Result + Squared (Feature_Type (Signal (I) - μ));
         end loop;
         return Result / Feature_Type (Signal'Length);
      end Energy;

      Normalisation_Factor : constant Feature_Type :=
         [for C of Hann_Window =>
            Squared (Feature_Type (C))]'Reduce ("+", Feature_Type (0.0));

      function Norm_Squared (Item : in Complex) return Feature_Type is (
         Squared (Feature_Type (Item.Re)) + Squared (Feature_Type (Item.Im)));

      procedure Welch (
         Signal    : in     Sample_Array;
         Pxx       :    out Welch_Array;
         Overlap   : in     Positive_Count_Type;
         Frequency : in     Feature_Type) is
         Steps  : constant Feature_Type := Feature_Type (
            (Signal'Length - Welch_Size) / Overlap + 1);
         Factor : constant Feature_Type := Feature_Type (2.0)
                        / Feature_Type (Normalisation_Factor * Frequency);
         Input  : Fourier_Transform_Real_Array;
         Output : Complex_Array;
         Index  : Count_Type := Signal'First;
      begin
         Pxx := [others => 0.0];
         while Index <= Signal'Last - Welch_Size + 1 loop
            Input := [for I in Input'Range =>
                        Hann_Window (Hann_Window'First + I - Input'First) *
                        Signal (I - Input'First + Index)];
            Fourier_Transform (Input, Output);
            for I in Pxx'Range loop
               Pxx (I) := @ + Norm_Squared (Output (I)) * Factor;
            end loop;
            Index := Index + (Welch_Size - Overlap);
         end loop;
         Pxx := [for I in Pxx'Range => Pxx (I) / Steps];
      end Welch;

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
         First := Count_Type (2.0 / Fq_Res);
         Last  := Count_Type (12.0 / Fq_Res);
         PSD_1 := Simpson (
            Pxx (First + Signal'First .. Last + Signal'First - 1), Fq_Res);
         -- PSD_2
         First := Count_Type (12.0 / Fq_Res);
         Last  := Count_Type (18.0 / Fq_Res);
         PSD_2 := Simpson (
            Pxx (First + Signal'First .. Last + Signal'First - 1), Fq_Res);
         -- PSD_3
         First := Count_Type (18.0 / Fq_Res);
         Last  := Count_Type (35.0 / Fq_Res);
         PSD_3 := Simpson (
            Pxx (First + Signal'First .. Last + Signal'First - 1), Fq_Res);
      end Power_Spectral_Density;

      function Distance (Left, Right : in Sample_Base_Type)
         return Feature_Type'Base is (
         Squared (Feature_Type (Left) - Feature_Type (Right)));

      function Single_Dynamic_Time_Warping (
         Signal  : in Sample_Epoch;
         Pattern : in Sample_Epoch;
         Max     : in Feature_Type'Base)
         return Feature_Type'Base is
         pragma Unreferenced (Max);
         Diag_Cost : constant := 1.0;
         Band_Size : constant := 2 * Warping_Window + 3;
         Infinity  : constant Feature_Type'Base := Feature_Type'Base'Last;
         function Saturated_Addition (Left, Right : in Feature_Type'Base)
            return Feature_Type'Base is (
            (if Right >= Feature_Type'Base'Last - Left
               then Feature_Type'Base'Last else Right + Left));
         type Band_Pair is
            array (
               Boolean,
               Count_Type range 0 .. Band_Size - 1)
            of Feature_Type'Base with
            Default_Component_Value => Infinity;
         Dist    : Feature_Type'Base;
         Index   : Count_Type := 1;
         Bands   : Band_Pair;
         Cost    : Boolean := False;
         Prev    : Boolean := True;
         X, Y, Z : Feature_Type'Base;
         F, L    : Count_Type;
      begin
         for Row in Count_Type range 1 .. Epoch_Size loop
            Index := Count_Type'Max (0, Warping_Window - Row + 1) + 1;
            F := Count_Type'Max (0, Row - 1 - Warping_Window) + 1;
            L := Count_Type'Min (Epoch_Size - 1, Row - 1 + Warping_Window) + 1;
            for Col in F .. L loop
               Dist := Distance (Signal (Row), Pattern (Col));
               if Row = 1 and Col = 1 then
                  Bands (Cost, Index) := Dist;
               else
                  Y := Saturated_Addition (Bands (Cost, Index - 1), Dist);
                  X := Saturated_Addition (Bands (Prev, Index + 1), Dist);
                  Z := Saturated_Addition (
                     Bands (Prev, Index), Diag_Cost * Dist);
                  Bands (Cost, Index) :=
                     Feature_Type'Base'Min (Feature_Type'Base'Min (X, Y), Z);
               end if;
               Index := Index + 1;
            end loop;
            Prev := not Prev;
            Cost := not Cost;
         end loop;
         return Bands (Prev, Index - 1);
      end Single_Dynamic_Time_Warping;

      function Sum_Squares (
         Item : in Sample_Array)
         return Feature_Type'Base is
         Result : Feature_Type'Base := 0.0;
      begin
         for I in Item'Range loop
            Result := @ + Squared (Feature_Type (Item (I)));
         end loop;
         return Result;
      end Sum_Squares;

      function Sqrt (Item : in Feature_Type'Base) return Feature_Type'Base is
         function powf (a, b : Float) return Float with
            Import => True, Convention => C, External_Name => "powf";
      begin
         return Feature_Type'Base (powf (Float (Item), 0.5));
      end Sqrt;

      function Normalise (
         Item : in Sample_Array)
         return Sample_Epoch is
         μ    : constant Sample_Type := Mean (Item);
         Sum2 : constant Feature_Type'Base := Sum_Squares (Item);
         Inv_Dev : constant Feature_Type'Base :=
            1.0 / Sqrt ((Sum2 / Feature_Type'Base (Epoch_Size))
            - Squared (Feature_Type (μ)));
      begin
         return [for I in Count_Type range 1 .. Epoch_Size =>
                  Feature_Type (Item (I - 1 + Item'First) - μ) * Inv_Dev];
      end Normalise;

      function Dynamic_Time_Warping (
         Signal  : in Sample_Array;
         Pattern : in Sample_Array;
         Maximum : in Feature_Type)
         return Feature_Type'Base is
      begin
         return abs (Single_Dynamic_Time_Warping (
            Signal  => Normalise (Signal),
            Pattern => Normalise (Pattern),
            Max     => Maximum) + 0.1);
      end Dynamic_Time_Warping;

   end Unproved;

   function Within (
      Item : in Feature_Type;
      Span : in Real_Span)
      return Boolean is (
      Span.Low <= Item and then Item <= Span.High);

   function Is_Seizure (
      Item  : in Sample_Epoch;
      Batch : in Batch_Type)
      return Boolean is
      use Unproved;
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
         or else not Within(PSD_2, Batch.PSD_2)
         or else not Within(PSD_3, Batch.PSD_3)
      then
         return False;
      end if;

      return
         (for some I in 1 .. Batch.Count =>
            Dynamic_Time_Warping (Item, Batch.Patterns (I), Batch.d_max_c)
            <= Batch.d_max_c);
   end Is_Seizure;

end Detector;
