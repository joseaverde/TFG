--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-numerics-elementary_functions-suite.adb               |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Numerics.Elementary_Functions.Tests;
package body Detector.Numerics.Elementary_Functions.Suite is

   Result : aliased Test_Suite;
   Test   : aliased Tests.Test;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test'Access);
      return Result'Access;
   end Suite;

end Detector.Numerics.Elementary_Functions.Suite;
