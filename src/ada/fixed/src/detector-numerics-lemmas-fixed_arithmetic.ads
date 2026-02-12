--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-lemmas-fixed_arithmetic.ads                  |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

generic
   type Fixed_Type is delta <>;
package Detector.Numerics.Lemmas.Fixed_Arithmetic with Pure, SPARK_Mode is

   subtype Base_Fixed_Type is Fixed_Type'Base;
   subtype Small_Base_Fixed_Type is Base_Fixed_Type range 

   generic
      type Other_Fixed_Type is delta <>;
   package Fixed_Multiplication_Lemmas is
      subtype Base_Other_Fixed_Type is Other_Fixed_Type'Base;
   end Fixed_Multiplication_Lemmas;

end Detector.Numerics.Lemmas.Fixed_Arithmetic;
