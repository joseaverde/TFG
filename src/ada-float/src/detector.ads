--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector.ads                                                   |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Seizure_Detector_Float_Config;

package Detector with Pure, SPARK_Mode is

   type Count_Type is new Natural;
   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

   type Float_Type is
      digits   (case Seizure_Detector_Float_Config.Float_Type is
                  when Seizure_Detector_Float_Config.Float_32 => 6,
                  when Seizure_Detector_Float_Config.Float_64 => 15) with
      Size =>  (case Seizure_Detector_Float_Config.Float_Type is
                  when Seizure_Detector_Float_Config.Float_32 => 32,
                  when Seizure_Detector_Float_Config.Float_64 => 64);
   -- The base floating type for the rest of floating types of the library.
   --  - IEEE 754 float 32: 6 digits, size => 32
   --  - IEEE 754 float 64: 15 digits, size => 64
   -- The only restriction we impose is that there is no infinity nor NaN.
   -- Proving their abscense is complicated, though.

end Detector;
