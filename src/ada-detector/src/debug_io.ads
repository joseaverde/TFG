package Debug_IO with Pure, SPARK_Mode is

   procedure Put (Item : in Character);
   procedure Put (Item : in String);
   procedure Put_Line (Item : in String);
   procedure New_Line (Count : in Positive := 1);

end Debug_IO;
