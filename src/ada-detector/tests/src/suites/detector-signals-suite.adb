package body Detector.Signals.Suite is

   Result : aliased Test_Suite;

   function Suite return Access_Test_Suite is
   begin
      return Result'Access;
   end Suite;

end Detector.Signals.Suite;
