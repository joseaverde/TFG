package body Detector.Tests_Helper is

   procedure Register (
      Test     : in out Test_Case'Class;
      Routines : in     Routine_Array) is
      use AUnit.Test_Cases.Registration;
   begin
      for Routine of Routines loop
         Register_Routine (Test, Routine.Item, Routine.Name);
      end loop;
   end Register;

end Detector.Tests_Helper;
