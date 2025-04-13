--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-mean.ads                                      |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

function Detector.Signals.Mean (
   Item : in Signal_Type)
   return Sample_Type with
   Global => null,
   Pre    => Item'Length > 0,
   Pure, SPARK_Mode;
