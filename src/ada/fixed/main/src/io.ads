package IO is

   type Count_Type is new Natural;
   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

   procedure Put (Item : in Character);
   procedure Put (Item : in String);
   procedure Put_Line (Item : in String);
   procedure New_Line (Count : in Positive_Count_Type := 1);

-- generic
--    type Num is range <>;
-- package Integer_IO is
-- end Integer_IO;

-- generic
--    type Num is delta <>;
-- package Fixed_IO is
-- end Fixed_IO;

end IO;
