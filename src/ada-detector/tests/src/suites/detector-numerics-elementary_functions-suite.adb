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
