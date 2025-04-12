--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-containers-vectors.adb                                |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Containers.Vectors with SPARK_Mode is

   function Create (
      Capacity : in Index_Type := Default_Capacity)
      return Vector is (
      new Vector_Record'(
         Capacity => Capacity,
         Last     => 0,
         others   => <>));

   procedure Reserve (
      Container    : in out Vector;
      New_Capacity : in     Index_Type) is
      Capacity : constant Index_Type :=
         Index_Type'Max (New_Capacity, Container.Capacity);
      Result : constant Vector := Create (Capacity);
      Temp   : Nullable_Vector := Container;
   begin
      pragma Assert (Result.Capacity = Capacity);
      Result.Last := Container.Last;
      Result.Data (1 .. Result.Last) := Container.Data (1 .. Container.Last);
      Temp := Container;
      Container := Result;
      Free (Temp);
   end Reserve;

end Detector.Containers.Vectors;
