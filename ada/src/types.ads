package Types with Pure, SPARK_Mode => On is

   type Count_Type is range 0 .. 1_000_000_000 with Size => 32;
   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;
   subtype Extended_Index is Count_Type range 0 .. Count_Type'Last - 1;
   subtype Index_Type is Extended_Index range 1 .. Extended_Index'Last;

end Types;
