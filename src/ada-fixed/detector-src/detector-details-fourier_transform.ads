with Detector.Details.Trigonometric; use Detector.Details.Trigonometric;

package Detector.Details.Fourier_Transform with SPARK_Mode => On is

   pragma Warnings (Off);

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

   Fourier_Transform_Recursion_Depth : constant := Log_2 (Welch_Size);

   subtype Complex_Part is Sample_Base_Type;
   type Complex is
      record
         Re : Complex_Part;
         Im : Complex_Part;
      end record;
   subtype Fourier_Transform_Real_Array is Sample_Array (1 .. Welch_Size);
   type Complex_Array is array (Index_Type range 1 .. Welch_Size) of Complex;

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

   pragma Warnings (Off, "postcondition does not check the outcome of calling");
   procedure Lemma_Product_By_Trigonometric_Output_Keeps_Range (
      Left  : in Complex_Part;
      Right : in Trigonometric_Output_Type;
      Bound : in Complex_Part) with
      Ghost,
      Pre  => Bound >= 0.0 and then Left in -Bound .. Bound,
      Post => Complex_Part (Left * Right) in -Bound .. Bound;
   pragma Warnings (On, "postcondition does not check the outcome of calling");

   procedure Fourier_Transform (
      Input  : in     Fourier_Transform_Real_Array;
      Output :    out Complex_Array);

end Detector.Details.Fourier_Transform;
