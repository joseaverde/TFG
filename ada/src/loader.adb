with Generic_Input_File, Reals;

procedure Loader (
   Name   : in     String;
   Batch  :    out Batchs.Batch_Access;
   Signal :    out Signals.Signal_Access) is
   pragma SPARK_Mode (On);
   use Signals, Reals;
   package File is new Generic_Input_File (Name);
   procedure Read is new File.Read (Count_Type'Base);
   procedure Read is new File.Read (Positive'Base);
   procedure Read is new File.Read (Real'Base);
   procedure Read (
      Item    : out Signals.Sample_Array;
      Success : out Boolean) is
      Value : Real'Base;
   begin
      Success := True;
      for I in Item'Range loop
         Read (Value);
         Success := Success and Value in Sample;
         Item (I) := (if Success then Value else 0.0);
      end loop;
   end Read;
   Size    : Count_Type'Base;
   Count   : Positive'Base;
   Success : Boolean;
begin
   Signal := null;
   Batch := null;
   -- Read the batch
   Read (Count);
   Read (Size);
   if Count not in Positive or else Size not in Index_Type then
      return;
   end if;
   Batch := new Batchs.Batch_Type'(Count, others => <>);
   Read (Batch.PSD_1.Low);    Read (Batch.PSD_1.High);
   Read (Batch.PSD_2.Low);    Read (Batch.PSD_2.High);
   Read (Batch.PSD_3.Low);    Read (Batch.PSD_3.High);
   Read (Batch.Energy.Low);   Read (Batch.Energy.High);
   Read (Batch.Max_Dist.Low); Read (Batch.Max_Dist.High);
   Read (Batch.d_max_c);
   for I in 1 .. Count loop
      Read (Batch.Pj (I).Samples, Success);
   end loop;
   -- Read the signal
   Signal := new Signals.Signal'(Last => Size, others => <>);
   Read (Signal.Samples, Success);
end Loader;
