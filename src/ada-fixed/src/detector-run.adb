with Interfaces.C;

procedure Detector.Run is

   pragma SPARK_Mode (On);

   subtype Sample_Stride is Sample_Array (1 .. Stride_Size);

   C_State : Interfaces.C.int with Import => True, Convention => C;

   procedure Read_Sample (
      Numerator   : out Interfaces.C.int;
      Denominator : out Interfaces.C.int) with
      Global            => C_State,
      Pure_Function     => False,
      Always_Terminates => True,
      Import            => True,
      Convention        => C,
      External_Name     => "eeg_read_sample";

   procedure Read_Sample (
      Value     : out Sample_Type;
      Truncated : out Boolean) is
      use type Interfaces.C.int;
      type Big_Int is range -2 ** 63 .. 2 ** 63 - 1 with Size => 64;
      Fixed_Den : constant := 2 ** Sample_Mantissa;
      Num, Den  : Interfaces.C.int;
      Temp, M   : Big_Int;
   begin
      -- We have the value:
      --    Value := Num / Den
      -- We want it to be:
      --    Value := Num' / Fixed_Den
      -- So it can be safetly converted into fixed point.
      --    Value := (Num * Fixed_Den) / (Den * Fixed_Den)
      --    Value := (Num * Fixed_Den / Den) / Fixed_Den
      --    Num' := (Num * Fixed_Den / Den)
      -- We want to do this multiplication without an overflow. First we can
      -- remove the sign from the denominator:
      Read_Sample (Num, Den);
      if Den = 0 then
         Truncated := True;
         Value := (if Num < 0 then Sample_Type'First else Sample_Type'Last);
      else
         Temp := Big_Int (Num) * Fixed_Den / Big_Int (Den);
         if Temp > Big_Int (Sample_Type'Last) * Fixed_Den then
            Truncated := True;
            Value := Sample_Type'Last;
         elsif Temp < Big_Int (Sample_Type'First) * Fixed_Den then
            Truncated := True;
            Value := Sample_Type'First;
         else
            M := Temp mod Fixed_Den;
            Temp := (Temp - M) / Fixed_Den;
            Value := Sample_Base_Type (Temp) + Sample_Base_Type (M)
                     * Sample_Base_Type (Sample_Type'Small);
            Truncated := False;
         end if;
      end if;
   end Read_Sample;

   procedure Read_Stride (
      Item      : out Sample_Stride;
      Truncated : out Natural) is
      Invalid : Boolean;
   begin
      Truncated := 0;
      for I in Item'Range loop
         pragma Loop_Invariant (Truncated in 0 .. Natural (I) - 1);
         Read_Sample (Item (I), Invalid);
         if Invalid then
            Truncated := Truncated + 1;
         end if;
      end loop;
   end Read_Stride;

   Signal    : Sample_Epoch with Relaxed_Initialization;
   Truncated : Natural;
   Patterns  : constant := 3;
   Batch     : Batch_Type := (
      Count    => Patterns,
      PSD_1    => (Feature_Type'First, Feature_Type'Last),
      PSD_2    => (Feature_Type'First, Feature_Type'Last),
      PSD_3    => (Feature_Type'First, Feature_Type'Last),
      Energy   => (Feature_Type'First, Feature_Type'Last),
      Max_Dist => (Feature_Type'First, Feature_Type'Last),
      d_max_c  => 0.0,
      Patterns => [for I in 1 .. Patterns =>
                     [for J in Sample_Epoch'Range =>
                        (Sample_Type (I) * Sample_Type (J))]]);

begin
   for I in Count_Type range 1 .. Strides_Per_Epoch loop
      Read_Stride (
         Item      => Signal (1 + (I - 1) * Stride_Size .. I * Stride_Size),
         Truncated => Truncated);
   end loop;
   Detection_Loop : loop
      Signal (1 + Stride_Size .. Signal'Last) :=
         Signal (1 .. Signal'Last - Stride_Size);
      Read_Stride (
         Item      => Signal (1 .. Stride_Size),
         Truncated => Truncated);
   end loop Detection_Loop;
end Detector.Run;
