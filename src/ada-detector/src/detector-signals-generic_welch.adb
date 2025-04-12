--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_welch.ads                             |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals.Fast_Fourier_Transform;

procedure Detector.Signals.Generic_Welch (
   Signal  : in     Signal_Type;
   Pxx     :    out Signal_Type;
   Period  : in     Sample_Type;
   Size    : in     Positive_Count_Type;
   Overlap : in     Count_Type) is
   use Complex_Types;
   Normalisation_Factor : constant := 0.5;
   Factor : constant Sample_Type := 2 * Period / Normalisation_Factor;
   Power  : constant Natural := Log_2 (Size);
   Steps  : constant Positive :=
      Positive ((Signal'Length - Size) / Overlap + 1);
   Scale  : Natural;
   Input  : Signal_Type (1 .. Size);
   Output : Complex_Signal (1 .. Size);
   Index  : Count_Type := Signal'First;
begin
   Pxx := [others => 0.0];
   while Index <= Signal'Last - Size + 1 loop
      Input := [for I in Input'Range =>
                  Window (I - Input'First, Size) *
                  Signal (I - Input'First + Index)];
      Fast_Fourier_Transform (Input, Output, Power, Scale);
      for I in Pxx'Range loop
         Pxx (I) := @ + Norm_Squared (Output (I)) * Factor;
      end loop;
      Index := Index + (Size - Overlap);
   end loop;
   Pxx := [for I in Pxx'Range => Pxx (I) / Steps];
end Detector.Signals.Generic_Welch;
