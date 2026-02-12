--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_max_distance.ads                      |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

generic
   type Result_Type is delta <>;
   with package Normalisation is new Generic_Normalisation (<>);
function Detector.Signals.Generic_Max_Distance (
   Item : in Signal_Type)
   return Result_Type with
   Global => null,
   Pre    => Item'Length > 0,
   Pure, SPARK_Mode;
-- Max distance is a function that returns the difference between the
-- maximum and the minimum value of a signal.
