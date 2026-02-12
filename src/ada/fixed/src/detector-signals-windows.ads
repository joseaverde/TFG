--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-windows.ads                                   |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Numerics.Elementary_Functions;

package Detector.Signals.Windows with Pure, SPARK_Mode is

   subtype Window_Output_Type is
      Detector.Numerics.Elementary_Functions.Trigonometric_Output_Type
      range 0.0 .. 1.0;

   function Hann (
      Index : in Count_Type;
      Size  : in Positive_Count_Type)
      return Window_Output_Type with
      Global => null,
      Inline => True,
      Pre    => Index < Size;

end Detector.Signals.Windows;
