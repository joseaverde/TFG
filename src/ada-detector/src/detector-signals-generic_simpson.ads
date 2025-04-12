--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_simpson.ads                           |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

generic
   type Result_Type is delta <>;
function Detector.Signals.Generic_Simpson (
   Signal : in Signal_Type)
   return Result_Type with
   Global => null,
   Pure, SPARK_Mode;
