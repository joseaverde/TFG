with Detector.Numerics.Elementary_Functions.Suite;

package body Detector.Numerics.Suite is

   Result : aliased Test_Suite;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Elementary_Functions.Suite.Suite);
      return Result'Access;
   end Suite;

end Detector.Numerics.Suite;
