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
      First  :    out Count_Type;
      Last   :    out Count_Type) is
      Chunk     : constant Count_Type := Length / Count_Type (Cores);
      Remainder : constant Count_Type := Length mod Count_Type (Cores);
   begin
      pragma Assert (Chunk + Remainder = Length);

      First := Count_Type (Id - 1) * Chunk;
      Last := Count_Type (Id) * Chunk - 1;

      if Count_Type (Id - 1) < Remainder then
         First := First + 1;
      end if;

      if Count_Type (Id) < Remainder then
         pragma Assert (Id /= Cores);
         Last := Last + 1;
      end if;

   end Partition;

end Detector.Parallel_Utils;
