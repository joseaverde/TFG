package Detector.Details.Lemmas with SPARK_Mode => On is

   pragma Warnings (Off, "postcondition does not check the outcome of calling");
   pragma Warnings (GNATProve, Off, "subprogram ""Lemma_Modulo_By_Definition"" has no effect",
      Reason => "We don't use the proof, but is kept for the future.");
   pragma Warnings (GNATProve, Off, "subprogram ""Lemma_Modulo_Divisor_Is_Also_Modulo"" has no effect",
      Reason => "We don't use the proof, but is kept for the future.");

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

   pragma Warnings (On, "postcondition does not check the outcome of calling");

end Detector.Details.Lemmas;
