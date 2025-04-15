--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_welch.ads                             |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

with Detector.Numerics.Complex_Types_Operations;
with Detector.Signals.Fast_Fourier_Transform;

procedure Detector.Signals.Generic_Welch (
   Signal    : in     Signal_Type;
   Pxx       :    out Signal_Type;
   Frequency : in     Positive_Count_Type;
   Size      : in     Positive_Count_Type;
   Overlap   : in     Count_Type;
   Rescale   :    out Result_Type) with SPARK_Mode => On is

   use Complex_Types;

   -- The usual algorithm is as follows:
   --
   --    Pxx := [0, 0, ..., 0];
   --    for each Window of Signal loop
   --       Temp := FFT (Window * Hann);
   --       Pxx (I) := Pxx (I) + |Temp|² / Factor
   --    end loop;
   --    Pxx (I) := Pxx (I) / Steps
   --
   -- However we have several problems:
   --
   --  * The output of the Fast Fourier Transform (FFT) is scaled.
   --  * The norm squared should be done with care and uniform integers <= 1.
   --  * Appling the factor at each iteration worsens performance and precision
   --
   -- In the implementation floating point implementation there was point on
   -- scaling on each step. However if we keep the invariant that every value
   -- is in 0.0 .. 1.0 (uniform). We can use other strategies.

   Input  : Signal_Type (1 .. Size);
   Output : Complex_Signal (1 .. Size);
   Power  : constant Natural := Log_2 (Size);

   -- First of all we are going to be using the Big_Sample_Type for internal
   -- computations:
   --
   --  * It stores perfectly the square of the norm of the FFT.
   --  * It is in range 0.0 .. 1.0 - Δ
   --
   -- We are going to be keeping a variable called `Scale' which stores the
   -- number of times we need to multiply by 2 to restore the result. Which
   -- is common for every element of the array.

   T_Pxx : Big_Signal_Type (1 .. Size / 2 + 1) := [others => 0.0];

   function Base_Norm is
      new Detector.Numerics.Complex_Types_Operations.Norm_Squared (
      Input_Complex => Complex_Types,
      Result_Type   => Big_Sample_Type);

   function Quarter_Norm (Item : in Complex) return Big_Sample_Type is (
      Base_Norm ((Re => Item.Re / 2, Im => Item.Im / 2))) with
      Post => Quarter_Norm'Result >= 0.0 and then Quarter_Norm'Result < 0.5;

   -- When we do the FFT it returns a scaling factor of the output. Furthermore
   -- we multiply the Re and Im numbers between themselves. Which means the
   -- scaling factor is doubled. Let's call this `Scaling' (double the output
   -- scaling factor of the FFT), then:
   --
   --    T_Pxx (I) := (2 ** Scale) * Pxx (I)
   --    Temp (I)  := (2 ** Scaling) * |FFT (Window) (I)|²
   --
   -- We have to them both together, we want to keep them in the same range.
   -- Therefore we divide the smaler one by the difference:
   --
   --    2 ** |Scaling - Scale|
   --
   -- That way both values will be in a similar range. Then before adding,
   -- consider if there is an overflow. If there was an overflow divide both
   -- by two and increase the final scaling factor.
   --
   -- Note: For the time being always rescale dividing by two. Then consider
   -- using a similar strategy to the FFT to avoid losing precision. However
   -- losing 3 or 4 bits or precision won't matter a lot, since even with using
   -- a double precision IEEE 754 floating point type you only get up to 52
   -- bits of precision.

   Scale     : Natural := 0;
   Scaling   : Natural;

   -- We are going to use the maximum value of the T_Pxx and the Norms. And get
   -- the scaling factor from them. That way we can always make sure to be
   -- using the maximum number of bits available.

   Exp_Left  : Natural;
   Exp_Right : Natural;
   Max_Left  : Big_Sample_Type;
   Max_Right : Big_Sample_Type;
   Norms     : Big_Signal_Type (T_Pxx'Range);

   -- Once we reach the end of the loop. All the values will be within 0.0 and
   -- 1.0. We have to rescale back. We have to rescale by:
   --
   --    Multiplying: 2 ** Scale * 2
   --    Dividing by: Steps * Frequency * Normalisation_Factor
   --
   -- Which can be done directly to the target Result_Type. It is worth noting
   -- that the Frequency might be a power of 2.
   --
   -- We can obtain the final value of `Scale' very easily. In the worst case.
   -- Every FFT rescales `Power' times (as per Postcondition). The loop
   -- iterates Steps times. Then the maximum Scale factor will be:
   --
   --    Scale <= (Power * 2) + Steps              [To be proven]
   --
   -- For instance for an epoch (1280) with stride (256), welch size of (512)
   -- and overlap of (256). The number of steps will be 4. In which case:
   --
   --    Power = Log_2 (512) = 9
   --    Steps = 4
   --    Scale <= 18 + 4 = 22
   --
   -- Which is a feasable value. It is around 64 million. Then it would be
   -- divided by:
   --
   --    Steps                 = 4
   --    Normalisation_Factor ~= 0.5
   --    Frequency             = 256      (stride size)
   --    Denominator ~= 512 = 2 ** 9
   --
   -- This value depends primarely on the Frequency. We cannot provide a good
   -- estimate. This can be solved by putting another output parameter with
   -- the real value to scale so that the user can use it to rescale it later.
   -- That way Pxx can sill be a `Signal_Type'.

   Index  : Count_Type := Signal'First;
   Steps  : Count_Type := 0;

begin

   T_Pxx := [others => 0.0];

   while Index <= Signal'Last - Size + 1 loop

      -- Compute the fast fourier transform over the window product.

      Input := [for I in Input'Range =>
                  Window (I - Input'First, Size) *
                  Signal (I - Input'First + Index)];
      Fast_Fourier_Transform (Input, Output, Power, Scaling);

      -- Compute the norms squared and their scaling factor

      -- Note that the norms before squaring where divided by 2 both their
      -- imaginary and real parts, and the result multiplied by 2. Which means
      -- the result was divided by 4. Also they where scaled by a given scaling
      -- factor. That means they scaling factor is doubled and we need to add 2
      -- from the division by 4.

      Scaling := Scaling * 2 + 2;
      Max_Right := 0.0;
      for I in Norms'Range loop
         Norms (I) := Quarter_Norm (Output (I));
         Max_Right := Big_Sample_Type'Max (Norms (I), Max_Right);
         pragma Loop_Invariant (
            (for all J in Norms'First .. I
               =>       Norms (J) >= 0.0
               and then Norms (J) < 0.5
               and then Max_Right >= Norms (J)));
      end loop;
      pragma Assert (
         (for all J in Norms'Range =>
            Max_Right >= Norms (J)));
      pragma Assert (Max_Right >= 0.0 and then Max_Right < 0.5);

   -- if Max_Right = 0.0 then
   --    -- This is never going to happen unless all the vales in the fast
   --    -- fourier transform are 0.0. In which case, let's set the scaling
   --    -- factor to 0. The result is not scaled.
   --    Scaling := 0;
   -- else
   --    -- The values are all in range [0.0, 0.5). The same goes for the
   --    -- maximum number. What we have to do is multiply by two until it is
   --    -- greater than 0.5. Each time we multiply. We reduce the scaling one
   --    -- by one. That way we now the maximum is as near to 0.5 as it can.
   --    while Scaling > 0 and then 2 * Max_Right < 0.5 loop
   --       Max_Right := 2 * @;
   --       Scaling   := @ - 1;
   --    end loop;
   -- end if;

      -- Now we now that:
      --
      --    Norm * (2 ** (Scaling'Old * 2 + 2))
      --
      -- was the real result. Which meant that the solution was divided by
      --
      --    2 ** (Scaling'Old * 2 + 2)
      --
      -- In order to fit the uniform type. However multiplied the result by 2
      -- until the maximum value was as near to 0.5 as possible. That means
      -- that the number is divided by:
      --
      --    2 ** Scaling
      --
      -- Where
      --
      --    Scaling := Scaling'Old * 2 + 2 - <Amount of times to reach 0.5>
      --
      -- Because we multiplied <Amount of times to reach 0.5> the numerator
      -- to reach to 0.5.
      --
      -- Similarly we had the Scale factor for the T_Pxx variable. Where we
      -- know that the values are scaed.

      if Scale = Scaling then
         Exp_Left := 1;
         Exp_Right := 0;
         Scale := Scale + 1;
      elsif Scale < Scaling then
         Exp_Left := Scaling - Scale + 1;
         Exp_Right := 0;
         Scale := Scaling + 1;
      else
         Exp_Left := 1;
         Exp_Right := Scale - Scaling;
         Scale := Scale + 1;
      end if;

      -- Add the values

      Max_Left := 0.0;
      for I in T_Pxx'Range loop
         T_Pxx (I) := @ / (2 ** Exp_Left) + Norms (I) / (2 ** Exp_Right);
         Max_Left := Big_Sample_Type'Max (@, T_Pxx (I));
         pragma Loop_Invariant (
            (for all J in T_Pxx'First .. I =>
               Max_Left >= T_Pxx (J)));
      end loop;
      pragma Assert (
         (for all J in T_Pxx'Range =>
            Max_Left >= T_Pxx (J)));

      -- Increment counters and repeat

      Steps := Steps + 1;
      Index := Index + (Size - Overlap);
      pragma Loop_Variant (Increases => Scale);
      pragma Loop_Variant (Increases => Index);
      pragma Loop_Variant (Increases => Steps);
      pragma Loop_Invariant ((for all X of T_Pxx => X >= 0.0));

   end loop;

   pragma Assert (Steps >= 0);
   Pxx := [for I in Pxx'Range =>
             Sample_Type (T_Pxx (I - Pxx'First + T_Pxx'First))];
   -- TODO: Add real normalisation factor. Here it is one half which divides
   --       the Steps variable in the denominator :)
   Rescale := Fixed_Long_Integer (2 ** Scale)
            / Fixed_Long_Integer (Steps * Frequency * 192);

end Detector.Signals.Generic_Welch;
