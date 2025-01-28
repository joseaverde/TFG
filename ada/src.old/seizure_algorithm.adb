with Ada.Text_IO;
with Seizure;
with Seizure.Signals;
procedure Seizure_Algorithm with SPARK_Mode => On is
   type Real is new Float;
   package Real_IO is new Ada.Text_IO.Float_IO (Real);
   package Real_Seizure is new Seizure (Real);
   package Signals is new Real_Seizure.Signals;
   X : constant Real_Seizure.Signal_Type := (
      Size => 10,
      Data => [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0]);
begin
   -- Real_IO.Put (Signals.Simpson (X, Real_Seizure.Make_Span (X), 1.0));
   Real_IO.Put (Signals.Sum (X, Real_Seizure.Make_Span (X)));
   Ada.Text_IO.New_Line;
exception
   when others => null;
end Seizure_Algorithm;
