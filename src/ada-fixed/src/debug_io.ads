--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    debug_io.ads                                                   |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

package Debug_IO with Pure, SPARK_Mode is

   procedure Put (Item : in Character);
   procedure Put (Item : in String);
   procedure Put_Line (Item : in String);
   procedure New_Line (Count : in Positive := 1);

end Debug_IO;
