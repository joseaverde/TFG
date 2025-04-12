--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-windows.adb                                   |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Signals.Windows is

   use Detector.Numerics.Elementary_Functions;

   function Hann (
      Index : in Count_Type;
      Size  : in Positive_Count_Type)
      return Window_Output_Type is ((
      declare
         θ : constant Trigonometric_Input_Type :=
            Fixed_Integer (2 * Index) / Fixed_Integer (Size);
      begin
         0.5 - Cospi (θ) / 2));

end Detector.Signals.Windows;
