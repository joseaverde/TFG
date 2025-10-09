package body Detector.Numerics.Elementary_Functions with SPARK_Mode => Off is

   -->> Trigonometric Functions <<--

   -- ==== Bhāskara I's sine approximation formula ====
   --
   -- https://en.wikipedia.org/wiki/
   --    Bh%C4%81skara_I%27s_sine_approximation_formula
   --
   --              π² - 4x²
   --    cos y ~= ----------
   --              π² + x²
   --
   -- As the angle is multiplied by π, we have:
   --
   --              π² - 4(xπ)²       1 - 4x²
   --    cos y ~= ------------ = π² ---------
   --              π² + (xπ)²        1 + x²
   --
   -- It approximates very accurately to the first quadrant, for the other
   -- quadrants we can use the first one (thanks Luisda for your math notes).
   --
   --
   --      +sin     |      +sin
   --      -cos     |      +cos
   --               |
   --        π-α    |   α
   --               |
   --     ----------+----------
   --               |
   --        π+α    |  2π-α
   --               |
   --      -sin     |      -sin
   --      -cos     |      +cos
   --
   --             1 - 4x²
   --    cos x = ---------,   0 <= x < 0.5
   --             1 + x²
   --
   --    cos x = -cos(1 - x), 0.5 <= x < 1
   --
   --    cos x = -cos(x - 1), 1 <= x < 1.5
   --
   --    cos x = cos(2 - x),  1.5 <= x < 2
   --          = cos(0.5 - (x - 1.5))
   --
   -- Luisda que Dios te lo pague, que yo no lo voy a hacer.
   -- ¿Dónde puse el libro de mates de 4º de la ESO?

   function Cospi_First_Quadrant (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type with
      Pre  => Item <= 0.5,
      Post => Cospi_First_Quadrant'Result >= 0.0;

   function Cospi_First_Quadrant (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type is
      Item2 : constant Trigonometric_Input_Type := Item * Item;
   begin
      return (1.0 - (4 * Item2)) / (1.0 + Item2);
   end Cospi_First_Quadrant;

   function Cospi (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type is (
      (if    Item < 0.5 then +Cospi_First_Quadrant (Item)
       elsif Item < 1.0 then -Cospi_First_Quadrant (1.0 - Item)
       elsif Item < 1.5 then -Cospi_First_Quadrant (Item - 1.0)
       else                  +Cospi_First_Quadrant (0.5 - (Item - 1.5))));

   function Sinpi (Item : in Trigonometric_Input_Type)
      return Trigonometric_Output_Type is (
      Cospi ((if Item < 1.5 then Item + 0.5 else Item - 1.5)));

   -- TODO: Proof: Sin²(x) + Cos²(x) <= 1

   -->> Square Root <<--

   type Fix is delta 1.0
      range -2.0 ** (Max_Bits - 1) .. 2.0 ** (Max_Bits - 1) - 1.0 with
      Size => Max_Bits;

   function Fixed_Sqrt (Item : in Fixed_Type) return Fixed_Type'Base is
      pragma SPARK_Mode (Off);
      Max_Iters : constant := 32;
      subtype Fixed is Fixed_Type'Base;
      subtype Sweet_Range is Fixed range 0.5 .. 2.0 - Fixed'Delta;
      Internal_Bits     : constant := Bits * 2;
      Internal_Whole    : constant := 16;
      Internal_Fraction : constant := Internal_Bits - Internal_Whole - 1;
      Internal_Delta    : constant := 2.0 ** (-Internal_Fraction);
      type Internal_Real is delta Internal_Delta
         range -2.0 ** Internal_Whole .. 2.0 ** Internal_Whole - Internal_Delta
      with Size => Internal_Bits;
      type Natural_64 is range 0 .. 2 ** 63 - 1 with Size => 64;
      X : Natural_64 := 1;
      Y : Natural_64 := 1;
      A : Internal_Real;
      C : Internal_Real;
   begin
      -- TODO: Check whether the divisions and multiplications translate as
      --       shifts.
      if Item <= 0.0 then
         return 0.0;
      end if;

      -- Scaling
      if Item < Sweet_Range'First then
         while Fix (X) * Item < Sweet_Range'First loop
            X := X * 4;
            Y := Y * 2;
         end loop;
         A := Fix (X) * Item;
      elsif Item > Sweet_Range'Last then
         while Item / Fix (X) >= Sweet_Range'Last loop
            X := X * 4;
            Y := Y * 2;
         end loop;
         A := Item / Fix (X);
      else
         pragma Assert (Item in Sweet_Range);
         A := Internal_Real (Item);
      end if;

      -- Algorithm
      pragma Assert (A >= 0.5 and then A < 2.0);
      C := A - 1.0;
      pragma Assert (C >= -0.5 and then C < 1.0);

      -- A ∈ [0.5, 2)
      -- C ∈ [-0.5, 1)
      --
      -- First iteration
      --    A' := A - AC/2
      --    AC     ∈ [-1.0, 2]
      --    AC/2   ∈ [-0.5, 1]
      --    A-AC/2 ∈ [-0.5, 2]
      -- TODO: Think it in parts

      for I in 1 .. Max_Iters loop
         pragma Loop_Invariant (A >= 0.5 and then A < 2.0);
         pragma Loop_Invariant (C >= -0.5 and then C < 1.0);
         exit when C = 0.0;
         A := A - (A * C) / 2;
         C := Internal_Real'(C * C) * (C - 3.0) / 4;
      end loop;

      -- Rescaling
      if Item < Sweet_Range'First then
         return A / Fix (Y);
      elsif Item > Sweet_Range'Last then
         return A * Fix (Y);
      else
         return Fixed_Type'Base (A);
      end if;
   end Fixed_Sqrt;

end Detector.Numerics.Elementary_Functions;
