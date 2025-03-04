with Detector.Details.Lemmas; use Detector.Details.Lemmas;

package body Detector.Details.Fourier_Transform with SPARK_Mode => On is

   pragma Warnings (Off);

   type Trigonometric_Output_Array is
      array (Count_Type range 0 .. Welch_Size - 1)
      of Trigonometric_Output_Type;

   subtype TIT is Trigonometric_Input_Type;

   Sin_ω_Table : constant Trigonometric_Output_Array :=
      [for I in Trigonometric_Output_Array'Range =>
         Sin (TIT (TIT (TIT (-2.0) * TIT (I)) * TIT (π)) / TIT (Welch_Size))];

   Cos_ω_Table : constant Trigonometric_Output_Array :=
      [for I in Trigonometric_Output_Array'Range =>
         Cos (TIT (TIT (TIT (-2.0) * TIT (I)) * TIT (π)) / TIT (Welch_Size))];

   -->> Implementation <<--

   function Sin_ω (K, N : Count_Type'Base)
      return Trigonometric_Output_Type is (
      Sin_ω_Table ((K * (Welch_Size / N)) mod Welch_Size)) with
      Pre      => N in 1 .. Welch_Size
         and then K in 0 .. Welch_Size * Welch_Size;

   function Cos_ω (K, N : Count_Type'Base)
      return Trigonometric_Output_Type is (
      Cos_ω_Table ((K * (Welch_Size / N)) mod Welch_Size)) with
      Pre      => N in 1 .. Welch_Size
         and then K in 0 .. Welch_Size * Welch_Size;

   function Product_By_ω (
      Factor : in Complex;
      K, N   : in Count_Type'Base;
      Bound  : in Complex_Part)
      return Complex is
      Re : Complex_Part;
      Im : Complex_Part;
   begin
      Lemma_Product_By_Trigonometric_Output_Keeps_Range (
         Factor.Re, Cos_ω (K, N), Bound);
      Re := Factor.Re * Cos_ω (K, N);
      Lemma_Product_By_Trigonometric_Output_Keeps_Range (
         Factor.Im, Sin_ω (K, N), Bound);
      Re := Re - Factor.Im * Sin_ω (K, N);
      pragma Assert (Re in -2 * Bound .. 2 * Bound);
      Lemma_Product_By_Trigonometric_Output_Keeps_Range (
         Factor.Re, Sin_ω (K, N), Bound);
      Im := Factor.Re * Sin_ω (K, N);
      pragma Assert (Im in -Bound .. Bound);
      Lemma_Product_By_Trigonometric_Output_Keeps_Range (
         Factor.Im, Cos_ω (K, N), Bound);
      Im := Im + Factor.Im * Cos_ω (K, N);
      pragma Assert (Im in -2 * Bound .. 2 * Bound);
      return (Re, Im);
   end Product_By_ω;

   procedure Fourier_Transform_Conquer_Operation (
      Left_Input   : in     Complex;
      Right_Input  : in     Complex;
      K, N         : in     Count_Type'Base;
      Left_Output  :    out Complex;
      Right_Output :    out Complex;
      Bound        : in     Complex_Part) is
      Offset : constant Complex := Product_By_ω (Right_Input, K, N, Bound);
   begin
      pragma Assert (Left_Input.Re in -Bound .. Bound);
      pragma Assert (Offset.Re in -2 * Bound .. 2 * Bound);
      pragma Assert (Left_Input.Re + Offset.Re in -3 * Bound .. 3 * Bound);
      Left_Output := (Re => Left_Input.Re + Offset.Re,
                      Im => Left_Input.Im + Offset.Im);
      Right_Output := (Re => Left_Input.Re - Offset.Re,
                       Im => Left_Input.Im - Offset.Im);
   end Fourier_Transform_Conquer_Operation;

   procedure Fourier_Transform_Conquer (
      Input      : in     Complex_Array;
      Output     :    out Complex_Array;
      Chunk_Size : in     Positive_Count_Type;
      Bound      : in     Complex_Part) is
      Count     : constant Count_Type := Input'Length / Chunk_Size;
      In_Left   : Count_Type;
      In_Right  : Count_Type;
      Out_Left  : Count_Type;
      Out_Right : Count_Type;
   begin
      pragma Assert (Input'Length = Output'Length);
      pragma Assert (Count * Chunk_Size = Input'Length);
      for Chunk in 0 .. Count / 2 - 1 loop      -- Count * Size = Length
         pragma Warnings (GNATProve, Off, "unused assignment",
            Reason => "It is used as the index offset in the loop");
         In_Left := Input'First + Chunk * Chunk_Size;
         pragma Warnings (GNATProve, On, "unused assignment");
         pragma Assert (Chunk < Count / 2);
         pragma Assert (Chunk + Count / 2 < Count);
         pragma Assert (Chunk + Count / 2 <= Count - 1);
         pragma Assert (Count * Chunk_Size = Input'Length);
         pragma Assert ((Count - 1) * Chunk_Size = Input'Length - Chunk_Size);
         pragma Assert (Chunk_Size > 0);
         pragma Assert (Chunk + Count / 2 <= Count - 1);
         Lemma_Mult_Is_Monotonic (Chunk + Count / 2, Count - 1, Chunk_Size);
         pragma Assert (Chunk_Size * (Chunk + Count / 2)
                        <= Chunk_Size * (Count - 1));
         pragma Assert ((Chunk + Count / 2) * Chunk_Size
                           <= Input'Length - Chunk_Size);
         pragma Assert (Input'Last - Input'First + 1 = Input'Length);
         pragma Warnings (GNATProve, Off, "unused assignment",
            Reason => "It is used as the index offset in the loop");
         In_Right := Input'First + (Chunk + Count / 2) * Chunk_Size;
         pragma Warnings (GNATProve, On, "unused assignment");
         Out_Left := Output'First + 2 * Chunk;
         Out_Right := Output'First + (2 * Chunk + 1);
         for Index in 0 .. Chunk_Size - 1 loop
            -- Split the buffer
            pragma Loop_Invariant (Out_Left + Index /= Out_Right + Index);
            pragma Warnings (GNATProve, Off, "statement has no effect",
               Reason => "False, it modifies `Left_Output' and `Right_Output' params.");
            pragma Warnings (GNATProve, Off, """Output"" is set by ""Fourier_Transform_Conquer_Operation"" but not used after the call",
               Reason => "It is not used as it is because it is the output parameter");
            Fourier_Transform_Conquer_Operation (
               Left_Input   => Input (In_Left + Index),
               Right_Input  => Input (In_Right + Index),
               K            => Index,
               N            => Chunk_Size * 2,
               Left_Output  => Output (Out_Left + Index),
               Right_Output => Output (Out_Right + Index),
               Bound        => Bound);
            pragma Annotate (
               GNATprove,
               False_Positive,
               "formal parameters ""Left_Output"" and ""Right_Output"" might be aliased",
               """Input"" and ""Output"" are not aliased Result /= not Result");
            pragma Warnings (GNATProve, On, "statement has no effect");
            pragma Warnings (GNATProve, On, """Output"" is set by ""Fourier_Transform_Conquer_Operation"" but not used after the call");
         end loop;
      end loop;
      Output := Input;
   end Fourier_Transform_Conquer;

   -- Obtain the maximum chunk size per iteration

   function Fourier_Transform_Chunk_Sizes
      return Fourier_Transform_Chunk_Size_Array is
      Result : Fourier_Transform_Chunk_Size_Array := [others => 1];
   begin
      for Index in 2 .. Fourier_Transform_Recursion_Depth loop
         pragma Loop_Invariant (Result (1) = 1);
         pragma Loop_Invariant (Result (1) = 2 ** (1 - 1));
         pragma Loop_Invariant (
            (for all I in 2 .. Index - 1 =>
               Result (I) = Result (I - 1) * 2));
         Result (Index) := Result (Index - 1) * 2;
      end loop;
      pragma Assert (Result (1) = 1);
      pragma Assert (
         (for all I in 2 .. Result'Last =>
            Result (I) = Result (I - 1) * 2));
      pragma Assert (Result (1) = 2 ** (1 - 1));
      pragma Assert (
         (for all I in 2 .. Result'Last =>
            (if Result (I - 1) = 2 ** (I - 2)
               and then Result (I) = Result (I - 1) * 2
               and then 2 ** (I - 2) * 2 = 2 ** (I - 1)
               then Result (I) = 2 ** (I - 1))));
      return Result;
   end Fourier_Transform_Chunk_Sizes;

   -- Obtain the result bounds per iteration

   function Fourier_Transform_Bounds
      return Fourier_Transform_Bound_Array is
      Result : Fourier_Transform_Bound_Array := [others => 0.0];
   begin
      Result (1) := Fourier_Transform_Initial_Bound;
      for Index in 2 .. Fourier_Transform_Recursion_Depth loop
         pragma Loop_Invariant (Result (1) = Fourier_Transform_Initial_Bound);
         pragma Loop_Invariant (
            (for all I in 2 .. Index - 1 =>
               Result (I) = 3 * Result (I - 1)));
         Result (Index) := 3 * Result (Index - 1);
      end loop;
      pragma Assume (for all Bound of Result => Bound <= Complex_Part'Last / 3);
      return Result;
   end Fourier_Transform_Bounds;

   procedure Lemma_Product_By_Trigonometric_Output_Keeps_Range (
      Left  : in Complex_Part;
      Right : in Trigonometric_Output_Type;
      Bound : in Complex_Part) is
      Min : constant Trigonometric_Output_Type :=
         Trigonometric_Output_Type'First;
      Max : constant Trigonometric_Output_Type :=
         Trigonometric_Output_Type'Last;
   begin
      -- TODO: Maybe it needs more assertions in the future.
      pragma Assert (Min = -1.0);
      pragma Assert (Max = 1.0);
      pragma Assert (Right in Min .. Max);
      if Right = 0.0 then
         pragma Assert (Complex_Part (Left * Right) in -Bound .. Bound);
      elsif Right > 0.0 then
         if Left = 0.0 then
            pragma Assert (Complex_Part (Left * Right) in -Bound .. Bound);
         elsif Left > 0.0 then
            pragma Assert (Left > 0.0);
            pragma Assert (Left <= Bound);
            pragma Assert (Complex_Part (Bound * Max) = Bound);
            pragma Assert (Complex_Part (Left * Max) = Left);
            pragma Assert (Complex_Part (Left * Right) in 0.0 .. Bound);
         elsif Left < 0.0 then
            pragma Assert (Left < 0.0);
            pragma Assert (Left >= -Bound);
            pragma Assert (Complex_Part ((-Bound) * Max) = -Bound);
            pragma Assert (Complex_Part (Left * Max) = Left);
            pragma Assert (Complex_Part (Left * Right) in -Bound .. 0.0);
         end if;
      end if;
      pragma Assert (Complex_Part (Left * Right) in -Bound .. Bound);
   end Lemma_Product_By_Trigonometric_Output_Keeps_Range;

   procedure Fourier_Transform (
      Input  : in     Fourier_Transform_Real_Array;
      Output :    out Complex_Array) is
      Result : Boolean := True;
      Buffer : array (Boolean) of Complex_Array;
      Chunk  : Positive_Count_Type := 1;
      Bound  : Complex_Part := Fourier_Transform_Initial_Bound;
   begin
      Buffer := [
         True  => [for I in Output'Range => (Input (I), 0.0)],
         False => [for I in Output'Range => (0.0, 0.0)]];
      pragma Assert (
         (for all I in Output'Range =>
            Buffer (True) (I).Re in -Bound .. Bound
            and then Buffer (True) (I).Im in -Bound .. Bound));
      pragma Assert (
         (for all I in Output'Range =>
            Buffer (False) (I).Re in -Bound .. Bound
            and then Buffer (False) (I).Im in -Bound .. Bound));
      pragma Assert (Bound > 0.0);
      for Layer in 1 .. Fourier_Transform_Recursion_Depth loop
         pragma Loop_Invariant (
            Chunk = Fourier_Transform_Chunk_Sizes (Layer));
         pragma Loop_Invariant (
            Bound = Fourier_Transform_Bounds (Layer));
         pragma Loop_Invariant (
            (for all I in Output'Range =>
               Buffer (True) (I).Re in -Bound .. Bound
               and then Buffer (True) (I).Im  in -Bound .. Bound
               and then Buffer (False) (I).Re in -Bound .. Bound
               and then Buffer (False) (I).Im in -Bound .. Bound));
         pragma Assert (Bound > 0.0);
         Fourier_Transform_Conquer (
            Input      => Buffer (Result),
            Output     => Buffer (not Result),
            Chunk_Size => Chunk,
            Bound      => Bound);
         pragma Annotate (
            GNATprove,
            False_Positive,
            "formal parameters ""Input"" and ""Output"" might be aliased",
            """Input"" and ""Output"" are not aliased Result /= not Result");
         Bound := Bound * 3;
         Chunk := Chunk * 2;
         Result := not Result;
      end loop;
      Output := Buffer (Result);
   end Fourier_Transform;

end Detector.Details.Fourier_Transform;
