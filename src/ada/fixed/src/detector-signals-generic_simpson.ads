--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_simpson.ads                           |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

generic
   type Result_Type is delta <>;
function Detector.Signals.Generic_Simpson (
   Signal : in Signal_Type;
   dx     : in Sample_Type)
   return Result_Type with
   Global   => null,
   Pre      => Signal'Length >= 3 and then dx > 0.0,
   Pure, SPARK_Mode;
