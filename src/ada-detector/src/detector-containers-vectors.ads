--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-containers-vectors.ads                                |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Ada.Unchecked_Deallocation;

generic
   type Element_Type is delta <>;
package Detector.Containers.Vectors with Preelaborate, SPARK_Mode is

   pragma Unevaluated_Use_Of_Old (Allow);

   -- This package contains a simple implementation of Vectors. It is not for
   -- the detector itself. However, if we want to apply parallelisation or run
   -- over very big signals (such as for training) we need to access the
   -- internal array for the Vector (as normal vectors don't allow to access
   -- the underlying array and the algorithms are written for just arrays).

   subtype Index_Type is Positive_Count_Type
      range 1 .. Positive_Count_Type'Last - 1;
   type Element_Array is array (Index_Type range <>) of Element_Type;
   type Vector_Record (Capacity : Index_Type) is record
      Data : Element_Array (1 .. Capacity);
      Last : Count_Type := 0;
   end record with
   Dynamic_Predicate => Last <= Capacity;
   -- This is the internal Vector's structure. For performance it has the
   -- capacity in the discriminant. It shouldn't be used as it is. You should
   -- use the `Vector' type instead (which is an access type). As the sole
   -- predicate, the vector's size must be between 0 and the Capacity.

   type Nullable_Vector is access Vector_Record;
   subtype Vector is not null Nullable_Vector;
   -- This is the Vector type. It is an access type and should be use with
   -- care and SPARK's borrow checker. The usage is as follows:
   --
   --    declare
   --       V : Nullable_Vector := Create (Capacity => 100);
   --    begin
   --       for I in 1 .. 1000 loop
   --          Append (V, I)
   --       end loop;
   --       Free (V);      -- SPARK complains if this is missing
   --    end;
   --
   -- Note that allocation may fail. In this implementation if the allocation
   -- fails it terminates. There is no way to recover from it. And if there
   -- was a way it would be too cubersome.

   Default_Capacity : constant := 8;

   function Create (
      Capacity : in Index_Type := Default_Capacity)
      return Vector with
      Global   => null,
      Inline   => True,
      Post     => Create'Result.Capacity = Capacity
         and then Create'Result.Last = 0;

   procedure Reserve (
      Container    : in out Vector;
      New_Capacity : in     Index_Type) with
      Post     => Container.Capacity
                  = Index_Type'Max (Container.Capacity'Old, New_Capacity)
         and then Container.Last = Container.Last'Old
         and then Container.Data'Old (1 .. Container.Last)
                  = Container.Data (1 .. Container.Last);

   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Vector_Record,
      Name   => Nullable_Vector);

end Detector.Containers.Vectors;
