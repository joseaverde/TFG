--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-parallel_runners.ads                                  |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Ada.Synchronous_Task_Control;

private generic
   with procedure Compute (Id : in Positive);
   Cores : in Positive;
package Detector.Parallel_Runners with SPARK_Mode is

   procedure Start;
   procedure Wait;

end Detector.Parallel_Runners;
