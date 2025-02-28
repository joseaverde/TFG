package body Detector.Lemmas with SPARK_Mode => On is

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

   function Lemma_Sample_Increment (
      Left  : in Sample_Base_Type;
      Right : in Sample_Base_Type;
      Index : in Count_Type)
      return Boolean is (True);

   procedure Lemma_Mult_Is_Monotonic (
      Left   : in Count_Type;
      Right  : in Count_Type;
      Factor : in Count_Type) is
   begin
      null;
   end Lemma_Mult_Is_Monotonic;

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

end Detector.Lemmas;
