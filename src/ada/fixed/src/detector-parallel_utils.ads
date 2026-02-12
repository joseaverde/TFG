--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-parallel_utils.ads                                    |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package Detector.Parallel_Utils with SPARK_Mode is

   procedure Partition (
      Cores  : in     Positive;
      Id     : in     Positive;
      Length : in     Positive_Count_Type;
      First  :    out Positive_Count_Type;
      Last   :    out Count_Type) with
      Pre  => Id <= Cores,
      Post => First <= Length and then Last <= Length;

end Detector.Parallel_Utils;
