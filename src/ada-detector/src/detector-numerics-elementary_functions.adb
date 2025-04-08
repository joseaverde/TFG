package body Detector.Numerics.Elementary_Functions with SPARK_Mode is

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

   function Integer_Sqrt (Item : in Integer_Type) return Integer_Type'Base is
      pragma SPARK_Mode (Off);
      -- https://en.wikipedia.org/wiki/Methods_of_computing_square_roots
      --    #Binary_numeral_system
      subtype Int is Integer_Type'Base range 0 .. Integer_Type'Base'Last;
      X : Int := Item;
      C : Int := 0;
      D : Int := 1;
   begin

      -- D is the highest power of 4, with D <= Item
      while D * 4 <= Item loop
         D := D * 4;
      end loop;

      while D /= 0 loop
         pragma Loop_Invariant (D >= 0);
         pragma Loop_Invariant (C >= 0);
         pragma Loop_Invariant (X >= 0);
         if X >= C + D then
            X := X - C - D;
            C := C / 2 + D;
         else
            C := C / 2;
         end if;
         D := D / 4;
      end loop;

      return C;

   end Integer_Sqrt;

   type Int is range -2 ** (Max_Bits - 1) .. 2 ** (Max_Bits - 1) - 1 with
      Size => Max_Bits;
   type Fix is delta 1.0
      range -2.0 ** (Max_Bits - 1) .. 2.0 ** (Max_Bits - 1) - 1.0 with
      Size => Max_Bits;
   function Sqrt_Int is new Integer_Sqrt (Int);

   function Fixed_Sqrt (Item : in Fixed_Type) return Fixed_Type'Base is
      subtype Fixed is Fixed_Type'Base range 0.0 .. Fixed_Type'Base'Last;
      Num : constant Int := Int (Fix'(Item / Fixed'(Fixed'Delta)));
      Den : constant Int := Int (Fix'(Fixed (1) / Fixed'(Fixed'Delta)));
   begin
      return Fix (Sqrt_Int (Num)) / Fix (Sqrt_Int (Den));
   end Fixed_Sqrt;

end Detector.Numerics.Elementary_Functions;
