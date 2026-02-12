--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_dynamic_time_warping.ads              |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals.Batch_Normalisation;

generic
   type Result_Type is delta <>;
function Detector.Signals.Generic_Dynamic_Time_Warping (
   Left           : in Batch_Normalisation.Normalised_Signal;
   Right          : in Batch_Normalisation.Normalised_Signal;
   Warping_Window : in Positive_Count_Type)
   return Result_Type with
   Pre      => Left'Length = Right'Length
      and then Left'Length > 0
      and then Warping_Window < Left'Length
      and then Left'Length > Warping_Window
      and then Left'Length - Warping_Window - 1 > Warping_Window,
   Global => null,
   Pure, SPARK_Mode;
