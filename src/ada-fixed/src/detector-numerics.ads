--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics.ads                                          |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package Detector.Numerics with Pure, SPARK_Mode is

   -- This package's child packages declare math-related functions, procedures
   -- and lemmas. It is similar to `Ada.Numerics' package. However it differs
   -- in that everything is implemented using binary fixed point numbers and
   -- integers.

   π : constant := 3.14159_26535_89793_23846_26433_83279_50288_41971_69399;
   -- The famous `π' constant.

end Detector.Numerics;
