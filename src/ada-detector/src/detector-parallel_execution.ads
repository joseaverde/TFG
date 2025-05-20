--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-parallel_execution.ads                                |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

private package Detector.Parallel_Execution with
   SPARK_Mode        => On,
   Abstract_State    => (Normal_State, (Synchronous_State with External)),
   Initializes       => (Normal_State, Synchronous_State)
is

   pragma Elaborate_Body;

   Worker_Count : constant := 10;

   type Executor_Interface is interface;

   procedure Execute (
      Object : in out Executor_Interface;
      Id     : in     Positive;
      Count  : in     Positive) is
      abstract with
      Pre'Class => Id <= Count;

end Detector.Parallel_Execution;
