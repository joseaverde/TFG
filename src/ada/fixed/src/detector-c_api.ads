--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-c_api.ads                                             |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Interfaces.C; use Interfaces.C;
with Seizure_Detector_Fixed_Config;
with Default_Detector;

package Detector.C_API with
   SPARK_Mode,
   Abstract_State => State,
   Initializes    => State
is

   Pattern_Count          : constant := 3;
   Default_Warping_Window : constant := 16;

   type Epoch_Type is record
      Value : Default_Detector.Batches.Epoch_Type;
   end record with Convention => C;

   function Batch_Pattern_Count return Count_Type with
      Global  => (Input => State),
      Depends => (Batch_Pattern_Count'Result => State);

   -- Configuration --

   package Config renames Seizure_Detector_Fixed_Config;

   Language  : constant char_array := "Ada" & nul;
   Compiler  : constant char_array := "GNAT" & nul;
   Profile   : constant char_array :=
      (case Config.Build_Profile is
         when Config.release => "Release" & nul,
         when Config.validation => "Validation" & nul,
         when Config.development => "Development" & nul);
   Real_Type : constant char_array := "Fixed" & nul;
   Target    : constant char_array :=
      (case Config.Target is
         when Config.native  => "Native",
         when Config.esp32c3 => "esp32c3",
         when Config.esp32c6 => "esp32c6",
         when Config.rpi4    => "Raspberry Pi 4");

   type const_char_ptr is not null access constant char;

   C_Language  : constant const_char_ptr := Language (Language'First)'Access
      with Export, Convention => C, External_Name => "detector_language";
   C_Compiler  : constant const_char_ptr := Compiler (Compiler'First)'Access
      with Export, Convention => C, External_Name => "detector_compiler";
   C_Profile   : constant const_char_ptr := Profile (Profile'First)'Access
      with Export, Convention => C, External_Name => "detector_profile";
   C_Real_Type : constant const_char_ptr := Real_Type (Real_Type'First)'Access
      with Export, Convention => C, External_Name => "detector_real_type";
   C_Target    : constant const_char_ptr := Target (Target'First)'Access
      with Export, Convention => C, External_Name => "detector_target";

   -- Sample conversion functions --

   subtype Sample_Type is Default_Detector.Sample_Type;

   function To_Sample (Item : in double) return Sample_Type with
      Export, Convention => C,
      External_Name => "detector_convert_double_to_sample";
   function To_Sample (Item : in int) return Sample_Type with
      Export, Convention => C,
      External_Name => "detector_convert_int_to_sample";
   function To_Double (Item : in Sample_Type) return double with
      Export, Convention => C,
      External_Name => "detector_convert_sample_to_double";

   Max_Sample : constant Sample_Type := Sample_Type'Last with
      Export, Convention => C, External_Name => "detector_max_sample";
   Min_Sample : constant Sample_Type := Sample_Type'First with
      Export, Convention => C, External_Name => "detector_min_sample";
   Zero_Sample : constant Sample_Type := Sample_Type'First with
      Export, Convention => C, External_Name => "detector_zero_sample";

   -- Feature conversion functions --

   subtype Feature_Type is Default_Detector.Feature_Type;

   function To_Feature (Item : in double) return Feature_Type with
      Export, Convention => C,
      External_Name => "detector_convert_double_to_feature";
   function To_Double (Item : in Feature_Type) return double with
      Export, Convention => C,
      External_Name => "detector_convert_feature_to_double";

   Max_Feature : constant Feature_Type := Feature_Type'Last with
      Export, Convention => C, External_Name => "detector_max_feature";
   Min_Feature : constant Feature_Type := Feature_Type'First with
      Export, Convention => C, External_Name => "detector_min_feature";
   Zero_Feature : constant Feature_Type := Feature_Type'First with
      Export, Convention => C, External_Name => "detector_zero_feature";

   -- Batch configuration functions --

   procedure Set_Max_Distance (Min, Max : in Feature_Type) with
      Global => (In_Out => State),
      Pre    => Batch_Pattern_Count = Pattern_Count,
      Post   => Batch_Pattern_Count = Pattern_Count,
      Export, Convention => C,
      External_Name => "detector_batch_set_max_distance";
   procedure Set_Energy (Min, Max : in Feature_Type) with
      Global => (In_Out => State),
      Pre    => Batch_Pattern_Count = Pattern_Count,
      Post   => Batch_Pattern_Count = Pattern_Count,
      Export, Convention => C, External_Name => "detector_batch_set_energy";
   procedure Set_PSD_1 (Min, Max : in Feature_Type) with
      Global => (In_Out => State),
      Pre    => Batch_Pattern_Count = Pattern_Count,
      Post   => Batch_Pattern_Count = Pattern_Count,
      Export, Convention => C, External_Name => "detector_batch_set_psd_1";
   procedure Set_PSD_2 (Min, Max : in Feature_Type) with
      Global => (In_Out => State),
      Pre    => Batch_Pattern_Count = Pattern_Count,
      Post   => Batch_Pattern_Count = Pattern_Count,
      Export, Convention => C, External_Name => "detector_batch_set_psd_2";
   procedure Set_PSD_3 (Min, Max : in Feature_Type) with
      Global => (In_Out => State),
      Pre    => Batch_Pattern_Count = Pattern_Count,
      Post   => Batch_Pattern_Count = Pattern_Count,
      Export, Convention => C, External_Name => "detector_batch_set_psd_3";
   procedure Set_d_max_c (Value : in Feature_Type) with
      Global => (In_Out => State),
      Pre    => Batch_Pattern_Count = Pattern_Count,
      Post   => Batch_Pattern_Count = Pattern_Count,
      Export, Convention => C, External_Name => "detector_batch_set_d_max_c";
   procedure Set_Pattern (Pattern : in Epoch_Type; Index : in int) with
      Global   => (In_Out => State),
      Post     => Batch_Pattern_Count = Pattern_Count,
      Pre      => Batch_Pattern_Count = Pattern_Count
         and then Index in 0 .. Pattern_Count - 1,
      Export, Convention => C, External_Name => "detector_batch_set_pattern";
   procedure Reset with
      Global => (In_Out => State),
      Pre    => Batch_Pattern_Count = Pattern_Count,
      Post   => Batch_Pattern_Count = Pattern_Count,
      Export, Convention => C, External_Name => "detector_reset";

   -- Detection functions

   procedure Is_Seizure (Item : in Epoch_Type; Result : out C_bool) with
      Export, Convention => C, External_Name => "detector_is_seizure",
      Pre    => Batch_Pattern_Count = Pattern_Count,
      Post   => Batch_Pattern_Count = Pattern_Count,
      Global => (In_Out => State);

   -- Feature functions

   function Max_Distance (Item : in Epoch_Type) return Feature_Type with
      Export, Convention => C, External_Name => "detector_max_distance";
   function Energy (Item : in Epoch_Type) return Feature_Type with
      Export, Convention => C, External_Name => "detector_energy";
   function DTW (Item : in Epoch_Type; Index : in int) return Feature_Type with
      Global   => (Input => State),
      Pre      => Index in 0 .. Pattern_Count - 1
         and then Batch_Pattern_Count = Pattern_Count,
      Export, Convention => C, External_Name => "detector_dtw";
   procedure PSDs (
      Item : in Epoch_Type; PSD_1, PSD_2, PSD_3 : out Feature_Type) with
      Export, Convention => C, External_Name => "detector_psds";

end Detector.C_API;
