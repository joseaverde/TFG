with Detector.Numerics.Suite;
with Detector.Signals.Suite;

package body Detector.Suite is

   Result : aliased Test_Suite;

   function Suite return Access_Test_Suite is
   begin
      Result.Add_Test (Numerics.Suite.Suite);
      Result.Add_Test (Signals.Suite.Suite);
      return Result'Access;
   end Suite;

end Detector.Suite;
