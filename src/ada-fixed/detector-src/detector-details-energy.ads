package Detector.Details.Energy with SPARK_Mode => On is

   -->> Energy <<--
   -- The Energy is computed as:
   --    Σ (Signal (I) - μ)² / Epoch_Size
   -- We now that
   --    μ ∈ First .. Last
   --    Signal (I) ∈ First .. Last, ∀ I
   -- Therefore:
   --    Signal (I) - μ ∈ Last - First .. First - Last
   -- And then
   --    (Signal (I) - μ)² ∈ 0 .. (Last - First)²
   -- Therefore:
   --    Σ (Signal (I) - μ)² ∈ 0 .. Epoch_Size (Last - First)²
   -- And then:
   --    Σ (Signal (I) - μ)² / Epoch_Size ∈ 0 .. (Last - First)²
   -- That gives us the upper limit for the energy computation.

-- function Energy (
--    Item : in Sample_Epoch)
--    return Feature_Type with
--    Global => null,
--    Post   => Energy'Result in 0.0
--                            .. (Feature_Type (Sample_Type'Last)
--                               - Feature_Type (Sample_Type'First))
--                               * (Feature_Type (Sample_Type'Last)
--                                 - Feature_Type (Sample_Type'First));

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

end Detector.Details.Energy;
