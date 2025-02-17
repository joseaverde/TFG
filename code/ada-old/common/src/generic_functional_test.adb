with Ada.Unchecked_Deallocation;
with Ada.Text_IO, Get_Word, Generic_Batchs;

procedure Generic_Functional_Test is
   pragma SPARK_Mode (On);
   package Batchs is new Generic_Batchs (Algorithms);
   type Batch_Access is access Batchs.Batch;
   type Sample_Array_Access is access Algorithms.Sample_Array;
   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Batchs.Batch,
      Name   => Batch_Access);
   procedure Free is
      new Ada.Unchecked_Deallocation (
      Object => Algorithms.Sample_Array,
      Name   => Sample_Array_Access);
   -->> Variables <<--
   Signal_Length : Count_Type;
   Pattern_Count : Batchs.Pattern_Count;
   Batch         : Batch_Access;
   Signal        : Signal_Access;
   Index         : Index_Type;
   PSD_1, PSD_2,
   PSD_3, Energy,
   Max_Dist, DTW : Real;
begin
   while Index <= Signal.all'Last - Epoch_Size loop
      PSD_1 := Algorithms.Power_Spectral_Density (
         Signal             => Signal (Index .. Index + Epoch_Size - 1),
         Sampling_Frequency => Sampling_Frequency,
         Low                => PSD_1_Low,
         High               => PSD_1_High);
      PSD_2 := Algorithms.Power_Spectral_Density (
         Signal             => Signal (Index .. Index + Epoch_Size - 1),
         Sampling_Frequency => Sampling_Frequency,
         Low                => PSD_2_Low,
         High               => PSD_2_High);
      PSD_3 := Algorithms.Power_Spectral_Density (
         Signal             => Signal (Index .. Index + Epoch_Size - 1),
         Sampling_Frequency => Sampling_Frequency,
         Low                => PSD_3_Low,
         High               => PSD_3_High);
      Energy := Algorithms.Energy (Signal (Index .. Index + Epoch_Size - 1));
      Max_Dist := Algorithms.Max_Distance (Signal (Index .. Index + Epoch_Size - 1));
      for Pattern of Batch.Patterns loop
         DTW := Algorithms.Dynamic_Time_Warping (
            Signal (Index .. Index + Epoch_Size - 1),
            Pattern,
            Maximum);
      end loop;
   end loop;
end Generic_Functional_Test;
