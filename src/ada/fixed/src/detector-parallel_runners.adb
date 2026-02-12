--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-parallel_runners.adb                                  |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Parallel_Runners with SPARK_Mode is

   use Ada.Synchronous_Task_Control;

   task type Worker is
   end Worker;

   protected Manager is
      procedure Claim_Id (Id : out Positive);
      procedure Finish;
   private
      Last_Id    : Natural := 0;
      Stop_Count : Natural := 0;
   end Manager;

   Suspender_Pool : array (1 .. Cores) of Suspension_Object;
   Worker_Pool    : array (1 .. Cores) of Worker;
   Is_Finished    : Suspension_Object;

   -->> Implementation <<--

   procedure Start is
   begin
      for Suspender of Suspender_Pool loop
         Set_True (Suspender);
      end loop;
   end Start;

   protected body Manager is
      procedure Claim_Id (Id : out Positive) is
      begin
         Last_Id := @ + 1;
         Id := Last_Id;
      end Claim_Id;

      procedure Finish is
      begin
         Stop_Count := @ + 1;
         if Stop_Count = Last_Id then
            Set_True (Is_Finished);
         end if;
      end Finish;
   end Manager;

   task body Worker is
      Id : Positive;
   begin
      Manager.Claim_Id (Id);
      Suspend_Until_True (Suspender_Pool (Id));
      Compute (Id);
      Manager.Finish;
   end Worker;

   procedure Wait is
   begin
      Suspend_Until_True (Is_Finished);
   end Wait;

end Detector.Parallel_Runners;
