--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-parallel_utils.adb                                    |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Parallel_Utils with SPARK_Mode is

   procedure Partition (
      Cores  : in     Positive;
      Id     : in     Positive;
      Length : in     Positive_Count_Type;
      First  :    out Positive_Count_Type;
      Last   :    out Count_Type) is
      Chunk     : constant Count_Type := Length / Count_Type (Cores);
      Remainder : constant Count_Type := Length mod Count_Type (Cores);
   begin
      pragma Assert (Chunk * Count_Type (Cores) + Remainder = Length);

      First := Chunk * Count_Type (Id - 1)
             + Count_Type'Min (Count_Type (Id - 1), Remainder) + 1;
      Last  := Chunk * Count_Type (Id)
             + Count_Type'Min (Count_Type (Id), Remainder);

   end Partition;

end Detector.Parallel_Utils;
