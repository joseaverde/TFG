with Common, Formal_Algorithms;

generic
   with package Algorithms is new Formal_Algorithms (<>);
package Generic_Batchs with Pure, SPARK_Mode => On is

   subtype Pattern_Type is Algorithms.Sample_Array (1 .. Common.Epoch_Size);
   type Pattern_Count is range 1 .. 5;
   type Pattern_Array is array (Pattern_Count range <>) of Pattern_Type;
   type Bounds is record Low, High : Algorithms.Real; end record;

   type Batch_Type (Count : Pattern_Count) is record
      PSD_1, PSD_2, PSD_3, Energy, Max_Dist : Bounds;
      D_max_c                               : Algorithms.Real;
      Patterns                              : Pattern_Array (1 .. Count);
   end record;

end Generic_Batchs;
