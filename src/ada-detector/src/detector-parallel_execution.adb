--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-parallel_execution.adb                                |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Ada.Synchronous_Task_Control;
with Ada.Unchecked_Deallocation;

package body Detector.Parallel_Execution with
   SPARK_Mode => On,
   Refined_State => (Normal_State      => (Worker_Pool),
                     Synchronous_State => (Running))
is

   use Ada.Synchronous_Task_Control;

   task type Worker;

   Worker_Pool : array (1 .. Worker_Count) of Worker;
   Running : Boolean := True with Atomic, Async_Readers, Async_Writers;

   -- Implementation

   task body Worker is
   begin
      null;
   end Worker;

end Detector.Parallel_Execution;
