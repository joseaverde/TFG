with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
procedure Test_Fft is

   subtype Real is Float;
   package EL is new Ada.Numerics.Generic_Elementary_Functions (Real);
   use EL;
   subtype Positive_Count_Type is Positive;
   subtype Count_Type is Natural;
   type Real_Array is array (Positive_Count_Type range <>) of Real;
   type Complex is record Re, Im : Real; end record;
   type Complex_Array is array (Positive_Count_Type range <>) of Complex;

   function "+" (Left, Right : in Complex) return Complex is (
      Re => Left.Re + Right.Re,
      Im => Left.Im + Right.Im);

   function "-" (Left, Right : in Complex) return Complex is (
      Re => Left.Re - Right.Re,
      Im => Left.Im - Right.Im);

   function Cos_Omega (K, N : Count_Type'Base) return Real is (
      Cos (Real (-2.0 * Ada.Numerics.Pi * Real (K)) / Real (N)));
   function Sin_Omega (K, N : Count_Type'Base) return Real is (
      Sin (Real (-2.0 * Ada.Numerics.Pi * Real (K)) / Real (N)));

   function Exponent_Product (
      Factor : in Complex;
      K, N   : in Count_Type)
      return Complex is (
      Re => Factor.Re * Cos_Omega (K, N) - Factor.Im * Sin_Omega (K, N),
      Im => Factor.Re * Sin_Omega (K, N) + Factor.Im * Cos_Omega (K, N));

   procedure FFT_Old (
      Input  : in     Real_Array;
      Output :    out Complex_Array;
      Size   : in     Count_Type;
      Stride : in     Positive_Count_Type) is
      Half : constant Count_Type := Size / 2;
   begin
      if Size = 1 then
         Output (Output'First) := (Re => Input (Input'First), Im => 0.0);
      elsif Size mod 2 = 0 then
         FFT_Old (
            Input  => Input (Input'First .. Input'Last),
            Output => Output (Output'First .. Output'First + Half - 1),
            Size   => Half,
            Stride => Stride * 2);
         FFT_Old (
            Input  => Input (Input'First + Stride .. Input'Last),
            Output => Output (Output'First + Half .. Output'Last),
            Size   => Half,
            Stride => Stride * 2);
         for K in 0 .. Half - 1 loop
            declare
               Fst : constant Count_Type := Output'First + K;
               Snd : constant Count_Type := Output'First + K + Half;
               P   : constant Complex := Output (Fst);
               Q   : constant Complex := Exponent_Product (Output (Snd),
                                                           K, Size);
            begin
               Output (Fst) := P + Q;
               Output (Snd) := P - Q;
            end;
         end loop;
      else
         for K in 0 .. Size - 1 loop
            declare
               Result : Complex := (0.0, 0.0);
            begin
               for N in 0 .. Size - 1 loop
                  Result.Re := @ + Input (Input'First + N * Stride) *
                                 Cos_Omega (K * N, Size);
                  Result.Im := @ + Input (Input'First + N * Stride) *
                                 Sin_Omega (K * N, Size);
               end loop;
               Output (Output'First + K) := Result;
            end;
         end loop;
      end if;
   end FFT_Old;

   procedure FFT (
      Input  : in     Real_Array;
      Output :    out Complex_Array) is
      Odd_Part  : Positive_Count_Type := Input'Length;
      Even_Part : Count_Type;
      Depth     : Count_Type := 0;
      Chunk     : Positive_Count_Type;
      Count     : Positive_Count_Type;
      subtype Output_Array is Complex_Array (Output'Range);
      Buffer    : array (Boolean) of Output_Array;
      O         : Boolean := True;
      T         : Boolean := False;
   begin

      while Odd_Part mod 2 = 0 loop
         Odd_Part := Odd_Part / 2;
         Depth := Depth + 1;
      end loop;
      Even_Part := Input'Length / Odd_Part;

      -- Even_Part = 2 ** Depth
      -- Input'Length = Odd_Part * 2 ** Depth

      -- Discrete Fourier transform:

      if Odd_Part = 1 then
         Buffer (O) := [for I in Output'Range =>
                          (Input (I - Output'First + Input'First), 0.0)];
      else
         for Offset in 0 .. Even_Part - 1 loop
            for Index in 0 .. Odd_Part - 1 loop
               declare
                  Result : Complex := (0.0, 0.0);
               begin
                  for J in 0 .. Odd_Part - 1 loop
                     Result := (
                        @.Re + Cos_Omega (Index * J, Odd_Part) *
                           Input (Input'First + Offset + Even_Part * J),
                        @.Im + Sin_Omega (Index * J, Odd_Part) *
                           Input (Input'First + Offset + Even_Part * J));
                  end loop;
                  Buffer (O) (Output'First + Offset * Odd_Part + Index) :=
                     Result;
               end;
            end loop;
         end loop;
      end if;

      -- Recursive Step, for powers of 2:

      Chunk := Odd_Part;
      Count := Even_Part;
      for Layer in 1 .. Depth loop           -- Depth = log2(Length)
         for C in 0 .. Count / 2 - 1 loop    -- Count * Chunk = Length
            for K in 0 .. Chunk - 1 loop
               declare
                  Fst : constant Count_Type := Output'First + C * Chunk + K;
                  Snd : constant Count_Type :=
                     Output'First + (C + (Count / 2)) * Chunk + K;
                  P   : constant Complex := Buffer (O) (Fst);
                  Q   : constant Complex := Exponent_Product (Buffer (O) (Snd),
                                                              K, Chunk * 2);
               begin
                  Buffer (T) (Output'First + 2 * C * Chunk + K) := P + Q;
                  Buffer (T) (Output'First + (2 * C + 1) * Chunk + K) := P - Q;
               end;
            end loop;
         end loop;

         T := not T;
         O := not O;
         Chunk := Chunk * 2;
         Count := Count / 2;
      end loop;

      Output := Buffer (O);
   end FFT;

   -->> <<--

   Size   : constant := 100_000;
   Period : constant := 4;
   Input  : Real_Array (1 .. Size) := [others => 0.0];
   Output : Complex_Array (1 .. Size) := [others => (0.0, 0.0)];
   Start  : Time;
   Stop   : Time;

begin

   for I in 1 .. Size loop
      Input (I) := Sin (
         Ada.Numerics.Pi * 2.0 * Real (I - 1) / Real (Size) * Real (Period));
   end loop;
   Start := Clock;
   FFT (Input, Output);
   Stop := Clock;
   Put_Line (Duration'Image (Stop - Start));

   Start := Clock;
   Output := [others => (0.0, 0.0)];
   FFT_Old (Input, Output, Size, 1);
   Stop := Clock;
   Put_Line (Duration'Image (Stop - Start));

end Test_Fft;
