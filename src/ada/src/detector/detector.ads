with Seizure_Detector_Config; use Seizure_Detector_Config;
with Seizure_Detector_Config.Reals;

package Detector with Pure, SPARK_Mode => On is

   Stride_Size  : constant := Samples_Per_Stride;
   Epoch_Size   : constant := Stride_Size * Strides_Per_Epoch;
   Sample_First : constant := -10_000.0;
   Sample_Last  : constant := +10_000.0;

   -->> Count Types <<--

   type Count_Type is range 0 .. 1_000_000_000 with Size => 32;
   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;
   subtype Extended_Index is Count_Type range 0 .. Count_Type'Last - 1;
   subtype Index_Type is Extended_Index range 1 .. Extended_Index'Last;

   -->> Reals & Samples <<--

   subtype Real is Reals.Real;
   subtype Sample is Real range Sample_First .. Sample_Last;
   type Complex is record Re, Im : Real; end record;
   type Real_Array is array (Index_Type range <>) of Real;
   type Sample_Array is array (Index_Type range <>) of Sample;
   type Complex_Array is array (Index_Type range <>) of Complex;

   -->> Batchs <<--

   type Pattern_Index is range 1 .. 5;
   subtype Pattern_Type is Sample_Array (1 .. Epoch_Size);
   type Pattern_Array is array (Pattern_Index range <>) of Pattern_Type;
   type Real_Span is record Low, High : Real; end record;

   type Batch_Type (Count : Pattern_Index := 1) is record
      PSD_1, PSD_2, PSD_3, Energy, Max_Dist : Real_Span;
      d_max_c                               : Real;
      Patterns                              : Pattern_Array (1 .. Count);
   end record;

end Detector;
