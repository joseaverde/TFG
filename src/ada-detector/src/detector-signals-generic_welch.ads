--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_welch.ads                             |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals.Windows;
generic
   with function Window (
      Index : in Count_Type;
      Size  : in Positive_Count_Type)
      return Windows.Window_Output_Type;
procedure Detector.Signals.Generic_Welch (
   Signal  : in     Signal_Type;
   Pxx     :    out Signal_Type;
   Period  : in     Sample_Type;
   Size    : in     Positive_Count_Type;
   Overlap : in     Count_Type) with
   Pre               => Size <= Signal'Length
               and then Overlap < Size
               and then Pxx'Length = Size / 2 + 1,
   Global            => null,
   Always_Terminates => True,
   Preelaborate, SPARK_Mode;
-- This function computes Welch:
-- @param Period
-- The inverse of the frequency
