with AUnit.Run, AUnit.Reporter.Text, Detector.Suite;
procedure Seizure_Detector_Fixed_Tests is
   -- TODO: Use custom Test_Runner Result'Class type
   procedure Run is new AUnit.Run.Test_Runner (Detector.Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Reporter.Set_Use_ANSI_Colors (True);
   Run (Reporter);
end Seizure_Detector_Fixed_Tests;
