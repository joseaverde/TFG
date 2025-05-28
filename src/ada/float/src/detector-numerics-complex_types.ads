--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-complex_types.ads                            |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Ada.Numerics.Generic_Complex_Types;
package Detector.Numerics.Complex_Types is
   new Ada.Numerics.Generic_Complex_Types (
   Real => Float_Type) with Pure;
