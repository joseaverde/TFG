with Detector.Details.Trigonometric;

package Detector.Details.Fast_Fourier_Transform with SPARK_Mode => On is

   -- Cooley-Tuckey
   -- A Fixed-point Fast Fourier Transform Error Analysis: PETER D. WELCH
   --
   -- We are going to use the technique «Testing for an Overflow». We first
   -- need to scale the input `Item' so that:
   --
   --    |Item (I)| < 1, ∀i = 0, 1, 2, ..., Welch_Size
   --    Sqrt (Item (I).Re ** 2 + Item (I).Im ** 2) < 1
   --    Item (I).Re ** 2 + Item (I).Im ** 2 < 1
   --
   -- We know that the input array for the Fourier Transform is formed out of
   -- only real numbers, therefore:
   --
   --    Item (I).Re ** 2 < 1
   --
   -- Which means that:
   --
   --    abs Item (I).Re < 1
   --
   -- The scaling factor we need is just the maximum number of the array. If
   -- that number is already lower than 1, the array is already scaled.
   -- Otherwise we take the maximum number. For this implementation we are
   -- going to be using <= instead of <. Because it simplifies a lot the
   -- calculations.
   --
   -- After applying the scaling factor, we start the algorithm. The algorithm
   -- is a modified Cooley-Tuckey algorithm that doesn't use recursion. Due to
   -- hardware constraints and precission concerns we cannot afford to use the
   -- slow O(N²) Discrete Fourier Transform. Thus we use Cooley Tuckey which is
   -- O(Nlog(N)).
   --
   -- We are going to assume the array's length is a power of 2. Otherwise we
   -- would have to do the Discrete Fourier Transform over the chunks of the
   -- non-multiple-of-2 part. The base case if it's a power of 2, is just
   -- copying the array.
   --
   -- The idea is to start from the bottom up. Let's see the example of an
   -- array of 20 elements:
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
   --
   -- In the conquer step, when we compute:
   --
   --    New_Output (I) := Output (I) + Output (J) * ω
   --    New_Output (J) := Output (I) - Output (J) * ω
   --
   -- We need to check for an overflow. This proves to be difficult since in
   -- SPARK we can't allow the overflow to happen. If we are able to check for
   -- an overflow. If at least one of the elements of the array overflows, then
   -- we divide everything by 2 and multiply the scaling factor by 2. That way
   -- we can keep the valid range.
   --
   -- The result is then rescaled by multiplying by the value it was divided
   -- by 1 or the maximum of the array; and by 2 to the power of the number of
   -- times it was divided by two during the conquer part (that number is
   -- bounded and is as much as the `Welch_Size').

   Fourier_Transform_Recursion_Depth : constant := Log_2 (Welch_Size);

   type Normalised_Sample is new Trigonometric.Trigonometric_Output_Type;
   subtype Scaling_Factor is Sample_Type;
   type Normalised_Sample_Epoch is
      array (Count_Type range 0 .. Welch_Size - 1)
      of Normalised_Sample;

   Max_Shift : constant := Log_2 (Welch_Size);
   subtype Shift_Count is Natural range 0 .. Max_Shift;
   Max_Factor_By_Shifting : constant := 2 ** Max_Shift;

   subtype Fast_Fourier_Transform_Input is Sample_Array (1 ..  Welch_Size);
   subtype Output_Sample is Sample_Base_Type
      range Sample_Type'First * Max_Factor_By_Shifting
         .. Sample_Type'Last * Max_Factor_By_Shifting;
   type Fast_Fourier_Transform_Output is
      array (Count_Type range 0 .. Welch_Size - 1)
      of Output_Sample;
   pragma Assert (abs Output_Sample'First = Output_Sample'Last);

   function Acc_Scaling_Factor (
      Item : in Fast_Fourier_Transform_Input)
      return Fast_Fourier_Transform_Input with
      Ghost    => True,
      Global   => null,
      Post     => Acc_Scaling_Factor'Result (Item'First) =
                     abs Item (Item'First)
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Acc_Scaling_Factor'Result (I) =
                        Sample_Type'Max (Acc_Scaling_Factor'Result (I - 1),
                                         abs Item (I)));

   function Get_Scaling_Factor (
      Item : in Fast_Fourier_Transform_Input)
      return Scaling_Factor with
      Global   => null,
      Post     => Get_Scaling_Factor'Result
                  = Acc_Scaling_Factor (Item) (Item'Last)
         and then (for some Value of Item =>
                     Get_Scaling_Factor'Result = abs Value
                     and then
                        (for all X of Item => abs Value >= abs X));

   function Scale (
      Item   : in Sample_Type;
      Factor : in Scaling_Factor)
      return Normalised_Sample with
      Inline => True,
      Pre    => Factor > 1.0 and then Factor >= abs Item;

   procedure Scale (
      Item   : in     Fast_Fourier_Transform_Input;
      Result :    out Normalised_Sample_Epoch;
      Factor :    out Scaling_Factor) with
      Global => null;

   function Rescale_Factor (
      Factor : in Scaling_Factor;
      Shifts : in Shift_Count)
      return Output_Sample with
      Global => null,
      Inline => True,
      Pre    => Factor >= 1.0;

   procedure Rescale (
      Item   : in     Normalised_Sample_Epoch;
      Result :    out Fast_Fourier_Transform_Output;
      Factor : in     Scaling_Factor;
      Shifts : in     Shift_Count) with
      Global => null,
      Pre    => Factor >= 1.0;

-- -->> Public <<--

-- procedure Fast_Fourier_Transform (
--    Input  : in     Fast_Fourier_Transform_Input;
--    Output :    out Fast_Fourier_Transform_Output);

end Detector.Details.Fast_Fourier_Transform;
