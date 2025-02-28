package body Detector with SPARK_Mode => On is

   pragma Warnings (Off, "postcondition does not check the outcome of calling");
   pragma Warnings (Off, "static fixed-point value is not a multiple of Small");

   -->> Trigonometric Functions <<--

   function Cos (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type is (
      1.0);

   function Sin (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type is (
      Trigonometric_Output_Type (
         Trigonometric_Input_Type'Min (1.0,
         Trigonometric_Input_Type'Max (Item, -1.0))));

   -->> Windows <<--

   type Trigonometric_Output_Array is
      array (Count_Type range 0 .. Welch_Size - 1)
      of Trigonometric_Output_Type;

   subtype TOT is Trigonometric_Output_Type;
   subtype TIT is Trigonometric_Input_Type;

   Hann_Window : constant Trigonometric_Output_Array :=
      [for I in Trigonometric_Output_Array'Range =>
         TOT (0.5) - TOT (0.5)
         * Cos (TIT (TIT (TIT (2.0) * TIT (I)) * TIT (π)) / TIT (Welch_Size))];
   pragma Assert (
      (for all I in Hann_Window'Range =>
         Hann_Window (I) in 0.0 .. 1.0));

   Sin_ω_Table : constant Trigonometric_Output_Array :=
      [for I in Trigonometric_Output_Array'Range =>
         Sin (TIT (TIT (TIT (-2.0) * TIT (I)) * TIT (π)) / TIT (Welch_Size))];

   Cos_ω_Table : constant Trigonometric_Output_Array :=
      [for I in Trigonometric_Output_Array'Range =>
         Cos (TIT (TIT (TIT (-2.0) * TIT (I)) * TIT (π)) / TIT (Welch_Size))];

   -->> Max distance <<--

   function Acc_Maximum (
      Item : in Sample_Epoch)
      return Sample_Epoch is
      Result : Sample_Epoch := [others => Sample_Type'First];
   begin
      Result (Item'First) := Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (
            Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I) = Sample_Type'Max (Item (I), Result (I - 1))));
         Result (Index) := Sample_Type'Max (Item (Index), Result (Index - 1));
      end loop;
      return Result;
   end Acc_Maximum;

   function Acc_Minimum (
      Item : in Sample_Epoch)
      return Sample_Epoch is
      Result : Sample_Epoch := [others => Sample_Type'First];
   begin
      Result (Item'First) := Item (Item'First);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (
            Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1 =>
               Result (I) = Sample_Type'Min (Item (I), Result (I - 1))));
         Result (Index) := Sample_Type'Min (Item (Index), Result (Index - 1));
      end loop;
      return Result;
   end Acc_Minimum;

   function Maximum (
      Item : in Sample_Epoch)
      return Sample_Type is
      Result : Sample_Type := Item (Item'First);
   begin
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (
            (for all I in Item'First .. Index - 1 =>
               Result >= Item (I)));
         pragma Loop_Invariant (
            (for some I in Item'First .. Index - 1 =>
               Item (I) = Result));
         pragma Loop_Invariant (Result = Acc_Maximum (Item) (Index - 1));
         Result := Sample_Type'Max (Result, Item (Index));
      end loop;
      return Result;
   end Maximum;

   function Minimum (
      Item : in Sample_Epoch)
      return Sample_Type is
      Result : Sample_Type := Item (Item'First);
   begin
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (
            (for all I in Item'First .. Index - 1 =>
               Result <= Item (I)));
         pragma Loop_Invariant (
            (for some I in Item'First .. Index - 1 =>
               Item (I) = Result));
         pragma Loop_Invariant (Result = Acc_Minimum (Item) (Index - 1));
         Result := Sample_Type'Min (Result, Item (Index));
      end loop;
      return Result;
   end Minimum;

   function Max_Distance (
      Item : in Sample_Epoch)
      return Feature_Type is
      Max : Sample_Type := Item (Item'First);
      Min : Sample_Type := Item (Item'First);
   begin
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Max = Acc_Maximum (Item) (Index - 1));
         pragma Loop_Invariant (Min = Acc_Minimum (Item) (Index - 1));
         Max := Sample_Type'Max (Item (Index), Max);
         Min := Sample_Type'Min (Item (Index), Min);
      end loop;
      pragma Assert (Max = Maximum (Item));
      pragma Assert (Min = Minimum (Item));
      return Feature_Type (Max) - Feature_Type (Min);
   end Max_Distance;

   -->> Mean <<--

   function Acc_Sum (
      Item : in Sample_Epoch)
      return Sample_Base_Epoch is
      Result : Sample_Base_Epoch := [others => 0.0];

      function Lemma_Sample_Increment (
         Left  : in Sample_Base_Type;
         Right : in Sample_Base_Type;
         Index : in Count_Type)
         return Boolean with
         Pre      => Index in 1 .. Epoch_Size - 1
            and then Left in Sample_Base_Type (Index) * Sample_Type'First
                          .. Sample_Base_Type (Index) * Sample_Type'Last
            and then Right in Sample_Type,
         Post     => Lemma_Sample_Increment'Result = True
            and then Left + Right
                        in Sample_Base_Type (Index + 1) * Sample_Type'First
                        .. Sample_Base_Type (Index + 1) * Sample_Type'Last;

      function Lemma_Sample_Increment (
         Left  : in Sample_Base_Type;
         Right : in Sample_Base_Type;
         Index : in Count_Type)
         return Boolean is (True);

   begin
      Result (Item'First) := Item (Item'First);
      pragma Assert (Result (Item'First) in Sample_Type);
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result (Item'First) = Item (Item'First));
         pragma Loop_Invariant (
            (for all I in Item'First + 1 .. Index - 1
               =>       Result (I - 1)
                           in Sample_Base_Type (I - 1) * Sample_Type'First
                           .. Sample_Base_Type (I - 1) * Sample_Type'Last
               and then Lemma_Sample_Increment (Result (I - 1), Item (I),
                                                I - 1)
               and then Result (I) = Result (I - 1) + Item (I)
               and then Result (I)
                           in Sample_Base_Type (I) * Sample_Type'First
                           .. Sample_Base_Type (I) * Sample_Type'Last));
         Result (Index) := Result (Index - 1) + Item (Index);
      end loop;
      return Result;
   end Acc_Sum;

   function Sum (
      Item : in Sample_Epoch)
      return Sample_Base_Type is
      Result : Sample_Base_Type := Item (Item'First);
   begin
      for Index in Item'First + 1 .. Item'Last loop
         pragma Loop_Invariant (Result = Acc_Sum (Item) (Index - 1));
         Result := Result + Item (Index);
      end loop;
      return Result;
   end Sum;

   function Mean (
      Item : in Sample_Epoch)
      return Sample_Type is
      Result : Sample_Base_Type;
   begin
      Result := Sum (Item);
      pragma Assert (Result
                        in Sample_Base_Type (Epoch_Size) * Sample_Type'First
                        .. Sample_Base_Type (Epoch_Size) * Sample_Type'Last);
      pragma Assert (Result / Sample_Base_Type (Epoch_Size) in Sample_Type);
      return Result / Sample_Base_Type (Epoch_Size);
   end Mean;

   -->> Fourier Transform <<--
   -- We use the Cooley–Tukey to make it O(Nlog(N)) when the signal is a
   -- multiple of 2**M. If it is an odd number, then we use a Discrete Fourier
   -- Transform which is O(N²). Then when: N = 2^E * O then the complexity will
   -- be: O(EO²) which is approximately.
   --
   -- The idea is start from the bottom up. Let's see the example of an array
   -- of 20 elements:
   --
   --    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   --    |X|Y|Z|W|X|Y|Z|W|X|Y|Z|W|X|Y|Z|W|X|Y|Z|W|    'Length = 20
   --    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   --                                                 Divide in even and odd
   --    +-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+-+-+-+-+-+
   --    |X|Z|X|Z|X|Z|X|Z|X|Z| |Y|W|Y|W|Y|W|Y|W|Y|W|  'Length = 10, 10
   --    +-+-+-+-+-+-+-+-+-+-+ +-+-+-+-+-+-+-+-+-+-+
   --                                                    Divide in even and odd
   --    +-+-+-+-+-+ +-+-+-+-+-+ +-+-+-+-+-+ +-+-+-+-+-+
   --    |X|X|X|X|X| |Z|Z|Z|Z|Z| |Y|Y|Y|Y|Y| |W|W|W|W|W| 'Length = 5, 5, 5, 5
   --    +-+-+-+-+-+ +-+-+-+-+-+ +-+-+-+-+-+ +-+-+-+-+-+
   --        DFT         DFT         DFT         DFT
   --
   -- Then the solutions are combied with Cooley-Tukey. There should be minimum
   -- amount of copies in the algorithm.
   --
   --             (input offset, output offset)             stride,length
   --                       (0,0)                          }   1     n
   --                    (0,0) (1,n/2)                     }   2    n/2
   --             (0,0) (2,n/4) (1,2n/4) (3,3n/4)          }   4    n/4
   --            (0,0)   (4,n/8) (2,2n/8) (6,3n/8)         \   8    n/8
   --          (1,4n/8) (5,5n/8) (3,6n/8) (7,7n/8)         /
   --
   -- And the pattern continues: the output offset is always:
   --
   --       i * stride
   --       ----------, i = 0, 1, 2, 3, ...
   --         length
   --
   -- The problem is the input offset, in the tree, the left node has the same
   -- value as the parent, and the right node equals the left node plus half
   -- the stride:
   --
   --                 (input offset in hexadecimal)
   --
   --                                  0
   --                          0               1
   --                      0       2       1       3
   --                    0   4   2   6   1   5   3   7
   --                   0 8 4 C 2 A 6 E 1 9 5 D 3 B 7 F
   --
   -- Which is equivalent to inverting the bits, this poses a problem. As there
   -- is no instruction in RISC-V which is able to invert the bits. Therefore
   -- we have to move the objects in memory. We will be using a double buffer
   -- for this.

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
      return Complex with
      Pre      => N in 1 .. Welch_Size
         and then K in 0 .. Welch_Size * Welch_Size
         and then Bound >= 0.0
         and then -Bound >= Complex_Part'First / 2
         and then Bound <= Complex_Part'Last / 2
         and then Factor.Re in -Bound .. Bound
         and then Factor.Im in -Bound .. Bound,
      Post     => Product_By_ω'Result.Re in -2 * Bound .. 2 * Bound
         and then Product_By_ω'Result.Im in -2 * Bound .. 2 * Bound;

   procedure Lemma_Product_By_Trigonometric_Output_Keeps_Range (
      Left  : in Complex_Part;
      Right : in Trigonometric_Output_Type;
      Bound : in Complex_Part) with
      Ghost,
      Pre  => Bound >= 0.0 and then Left in -Bound .. Bound,
      Post => Complex_Part (Left * Right) in -Bound .. Bound;

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
      Bound        : in     Complex_Part) with
      Inline   => True,
      Pre      => Bound >= 0.0
         and then Bound <= Complex_Part'Last / 3
         and then Left_Input.Re in -Bound .. Bound
         and then Left_Input.Im in -Bound .. Bound
         and then Right_Input.Re in -Bound .. Bound
         and then Right_Input.Im in -Bound .. Bound
         and then K in 0 .. Welch_Size * Welch_Size
         and then N in 1 .. Welch_Size,
      Post     => Left_Input.Re in -3 * Bound .. 3 * Bound
         and then Left_Input.Im in -3 * Bound .. 3 * Bound
         and then Right_Input.Re in -3 * Bound .. 3 * Bound
         and then Right_Input.Im in -3 * Bound .. 3 * Bound;

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
      Bound      : in     Complex_Part) with
      Pre      => Bound >= 0.0
         and then Bound <= Complex_Part'Last / 3
         and then Chunk_Size < Input'Length
         and then Input'Length mod Chunk_Size = 0
         and then (for all Item of Input =>
                     Item.Re in -Bound .. Bound
                     and then Item.Im in -Bound .. Bound),
      Post     => (for all Item of Output =>
                     Item.Re in -3 * Bound .. 3 * Bound
                     and then Item.Im in -3 * Bound .. Bound);

   procedure Lemma_Mult_Is_Monotonic (
      Left   : in Count_Type;
      Right  : in Count_Type;
      Factor : in Count_Type) with
      Global => null,
      Pre      => Left <= Epoch_Size
         and then Right <= Epoch_Size
         and then Factor <= Epoch_Size
         and then Left <= Right,
      Post     => Left * Factor <= Right * Factor;

   procedure Lemma_Mult_Is_Monotonic (
      Left   : in Count_Type;
      Right  : in Count_Type;
      Factor : in Count_Type) is
   begin
      null;
   end Lemma_Mult_Is_Monotonic;

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

   Fourier_Transform_Recursion_Depth : constant := Log_2 (Welch_Size);

   -- Obtain the maximum chunk size per iteration

   procedure Lemma_Modulo_By_Definition (
      A : in Positive_Multiplication_Safe_Count;
      B : in Positive_Multiplication_Safe_Count;
      N : in Positive_Multiplication_Safe_Count) with
      Pre  => A = B * N,
      Post => A mod B = 0;

   procedure Lemma_Modulo_Divisor_Is_Also_Modulo (
      A : in Positive_Multiplication_Safe_Count;
      K : in Positive_Multiplication_Safe_Count;
      B : in Positive_Multiplication_Safe_Count) with
      Pre  => A mod (K * B) = 0,
      Post => A mod B = 0;

   procedure
      Lemma_If_B_Divides_A_And_A_Div_B_Mod_2_Is_0_Then_A_Mod_2_Times_B_Is_0 (
      A : in Multiplication_Safe_Count;
      B : in Positive_Multiplication_Safe_Count) with
      Ghost    => True,
      Global   => null,
      Pre      => A mod B = 0 and then (A / B) mod 2 = 0,
      Post     => A mod (B * 2) = 0;

   procedure Lemma_Modulo_By_Definition (
      A : in Positive_Multiplication_Safe_Count;
      B : in Positive_Multiplication_Safe_Count;
      N : in Positive_Multiplication_Safe_Count) is
   begin
      null;
   end Lemma_Modulo_By_Definition;

   procedure Lemma_Modulo_Divisor_Is_Also_Modulo (
      A : in Positive_Multiplication_Safe_Count;
      K : in Positive_Multiplication_Safe_Count;
      B : in Positive_Multiplication_Safe_Count) is
      N : Positive_Multiplication_Safe_Count;
   begin
      -- A ≡ 0 (mód K * B)
      -- ∃N: A = K * B * N
      -- A mod B = 0
      -- A mod K = 0
      -- A mod N = 0
      pragma Assert (A >= K * B);
      pragma Assert (A mod (K * B) = 0);
      N := A / (K * B);
      pragma Assert (A = N * (K * B));
      Lemma_Modulo_By_Definition (A, B, N * K);
   end Lemma_Modulo_Divisor_Is_Also_Modulo;

   procedure
      Lemma_If_B_Divides_A_And_A_Div_B_Mod_2_Is_0_Then_A_Mod_2_Times_B_Is_0 (
      A : in Multiplication_Safe_Count;
      B : in Positive_Multiplication_Safe_Count) is
      K : Positive_Multiplication_Safe_Count;
   begin
      if A = 0 then
         pragma Assert (A mod (B * 2) = 0);
      else
         pragma Assert (A >= B);
         pragma Assert ((A / B) mod 2 = 0);
         -- ∃K : A / B = 2 * K => K = A / B / 2;
         K := A / B / 2;
         pragma Assert (K <= A);
         pragma Assert (K * 2 = (A / B));
         pragma Assert (K * 2 * B = A);
         pragma Assert (A mod (K * 2 * B) = 0);
         Lemma_Modulo_Divisor_Is_Also_Modulo (A, K, 2 * B);
         pragma Assert (A mod (2 * B) = 0);
      end if;
   end Lemma_If_B_Divides_A_And_A_Div_B_Mod_2_Is_0_Then_A_Mod_2_Times_B_Is_0;

   type Fourier_Transform_Chunk_Size_Array is
      array (1 .. Fourier_Transform_Recursion_Depth)
      of Positive_Count_Type;
   function Fourier_Transform_Chunk_Sizes
      return Fourier_Transform_Chunk_Size_Array with
      Ghost    => True,
      Global   => null,
      Post     => Fourier_Transform_Chunk_Sizes'Result (1) = 1
         and then (for all I in 2 .. Fourier_Transform_Recursion_Depth =>
                     Fourier_Transform_Chunk_Sizes'Result (I) =
                        Fourier_Transform_Chunk_Sizes'Result (I - 1) * 2)
         and then Fourier_Transform_Chunk_Sizes'Result (1) = 2 ** (1 - 1)
         and then (for all I in 2 .. Fourier_Transform_Recursion_Depth =>
                     Fourier_Transform_Chunk_Sizes'Result (I) = 2 ** (I - 1));

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

   Fourier_Transform_Initial_Bound : constant Complex_Part :=
      Complex_Part'Max (Complex_Part (abs Sample_Type'First),
                        Complex_Part (abs Sample_Type'Last));

   type Fourier_Transform_Bound_Array is
      array (1 .. Fourier_Transform_Recursion_Depth)
      of Complex_Part;
   function Fourier_Transform_Bounds
      return Fourier_Transform_Bound_Array with
      Ghost    => True,
      Global   => null,
      Post     => Fourier_Transform_Bounds'Result (1)
                  = Fourier_Transform_Initial_Bound
         and then (for all I in 2 .. Fourier_Transform_Recursion_Depth =>
                     Fourier_Transform_Bounds'Result (I)
                     = 3 * Fourier_Transform_Bounds'Result (I - 1))
         and then (for all Bound of Fourier_Transform_Bounds'Result =>
                     Bound > 0.0)
         and then (for all Bound of Fourier_Transform_Bounds'Result =>
                     Bound <= Complex_Part'Last / 3);
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

   -->> Energy <<--

-- function Energy (
--    Item : in Sample_Epoch)
--    return Feature_Type is
--    μ      : constant Sample_Type := Mean (Item);
--    Value  : Feature_Type;
--    Result : Feature_Type := 0.0;
-- begin
--    for I in Item'Range loop
--       Value := Feature_Type (Signal (I)) - Feature_Type (μ);
--       Result := Result + Value * Value;
--    end loop;
--    return Result / Feature_Type (Signal'Length);
-- end Energy;

end Detector;
