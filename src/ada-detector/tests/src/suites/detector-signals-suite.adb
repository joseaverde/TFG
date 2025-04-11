--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-suite.adb                                     |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Signals.Fast_Fourier_Transform_Tests;

package body Detector.Signals.Suite is

   Result                      : aliased Test_Suite;
   Fast_Fourier_Transform_Test : aliased Fast_Fourier_Transform_Tests.Test;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Fast_Fourier_Transform_Test'Access);
      return Result'Access;
   end Suite;

end Detector.Signals.Suite;
