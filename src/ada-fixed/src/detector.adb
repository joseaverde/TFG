package body Detector with SPARK_Mode => On is

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

      pragma Warnings (Off);
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
      pragma Warnings (On);

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

   procedure Fourier_Transform (
      Input  : in     Fourier_Transform_Real_Array;
      Output :    out Complex_Array) is
   begin
      null;
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
