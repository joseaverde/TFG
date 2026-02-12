--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-quarter_variance.ads                          |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

function Detector.Signals.Quarter_Variance (
   Item : in Signal_Type)
   return Big_Sample_Type with
   Global => null,
   Pre  => Item'Length > 0,
   Post => Detector.Signals.Quarter_Variance'Result >= 0.0,
   Pure, SPARK_Mode;
