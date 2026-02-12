--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2026 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-containers-vectors.adb                                |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package body Detector.Containers.Vectors with SPARK_Mode is

   function Create (
      Capacity : in Index_Type := Default_Capacity;
      Fill     : in Element_Type)
      return Vector is (
      new Vector_Record'(
         Capacity => Capacity,
         Last     => 0,
         Data     => [for I in 1 .. Capacity => Fill]));

   procedure Reserve (
      Container    : in out Vector;
      New_Capacity : in     Index_Type) is
      Capacity : constant Index_Type :=
         Index_Type'Max (Default_Capacity,
            Index_Type'Max (New_Capacity, Container.Capacity));
      Result : constant Vector := Create (Capacity, Container.Data (1));
      Temp   : Nullable_Vector := Container;
   begin
      pragma Assert (Result.Capacity = Capacity);
      Result.Last := Temp.Last;
      Result.Data (1 .. Result.Last) := Temp.Data (1 .. Temp.Last);
      Free (Temp);
      Container := Result;
   end Reserve;

   function Next_Capacity (
      Old_Capacity : in Index_Type;
      Increment    : in Positive_Count_Type)
      return Index_Type with
      Pre  => Old_Capacity <= Index_Type'Last - Increment,
      Post => Next_Capacity'Result >= Old_Capacity + Increment;

   function Next_Capacity (
      Old_Capacity : in Index_Type;
      Increment    : in Positive_Count_Type)
      return Index_Type is ((
      declare
         Next : constant Index_Type := Old_Capacity + Increment;
      begin
         (if Old_Capacity <= Index_Type'Last / 3 * 2
            then Index_Type'Max (Next, Old_Capacity / 3 * 2)
            else Index_Type'Last)));

   procedure Append (
      Container : in out Vector;
      Element   : in     Element_Type;
      Count     : in     Positive_Count_Type := 1) is
      pragma Assert (Container.Last <= Container.Capacity);
      Old_Last : constant Count_Type := Container.Last;
      Old_Data : constant Element_Array (1 .. Old_Last) :=
         Container.Data (1 .. Container.Last) with
         Ghost => True;
   begin
      if Container.Last + Count > Container.Capacity then
         pragma Assert (Container.Last <= Index_Type'Last - Count);
         Reserve (Container, Next_Capacity (
            Old_Capacity => Container.Capacity,
            Increment    => Count - (Container.Capacity - Container.Last)));
         pragma Assert (Container.Data (1 .. Container.Last) = Old_Data);
      else
         pragma Assert (Container.Data (1 .. Container.Last) = Old_Data);
      end if;
      for I in 1 .. Count loop
         Container.Last := Container.Last + 1;
         Container.Data (Container.Last) := Element;
         pragma Loop_Invariant (
            Container.Last = Container.Last'Loop_Entry + I);
         pragma Loop_Invariant (
            Container.Data (1 .. Old_Last) = Old_Data);
         pragma Loop_Invariant (
            (for all J in 1 .. I =>
               Container.Data (Old_Last + J) = Element));
      end loop;
   end Append;

end Detector.Containers.Vectors;
