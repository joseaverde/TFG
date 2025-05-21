with SPARK.Lemmas.Fixed_Point_Arithmetic;

private package Detector.Signals.Lemmas with Pure, SPARK_Mode is

   package Sample_Lemmas is
      new SPARK.Lemmas.Fixed_Point_Arithmetic (Sample_Type);

   package Big_Sample_Lemmas is
      new SPARK.Lemmas.Fixed_Point_Arithmetic (Big_Sample_Type);

   procedure Lemma_Division_Reduces_Range (
      Num   : in Sample_Type;
      Den   : in Positive;
      First : in Sample_Type;
      Last  : in Sample_Type) with
      Ghost  => True,
      Global => null,
      Pre    => Num in First .. Last,
      Post   => Num / Den in First / Den .. Last / Den;

   Half_Sample_Type_First : constant Sample_Type := Sample_Type'First / 2;
   Half_Sample_Type_Last  : constant Sample_Type := Sample_Type'Last / 2;

   procedure Lemma_Half_Halves (Item : in Sample_Type) with
      Ghost  => True,
      Global => null,
      Post   => Item / 2 in Half_Sample_Type_First .. Half_Sample_Type_Last;

   generic
      type Fixed_Type is delta <>;
      type Index_Type is range <>;
      type Array_Type is array (Index_Type range <>) of Fixed_Type;
      First : in Fixed_Type;
      Last  : in Fixed_Type;
   function Generic_Accumulation (
      Item : in Array_Type)
      return Array_Type with
      Ghost    => True,
      Global   => null,
      Pre      => Item'Length > 0
         and then (for all X of Item => X in First .. Last),
      Post     => (
         declare  Result renames Generic_Accumulation'Result;
         begin    Result'First = Item'First
         and then Result'Length = Item'Length
         and then Result (Item'First) = Item (Item'First)
         and then (for all I in Item'First + 1 .. Item'Last =>
                     Result (I - 1) in Positive (I - Item'First) * First
                                    .. Positive (I - Item'First) * Last
                     and then Result (I) = Result (I - 1) + Item (I))
         and then Result (Item'Last) in Item'Length * First
                                    .. Item'Length * Last);
   -- This is a ghost functions for proofs that require some kind of
   -- accumulation. It returns an array with the intermediate results of the
   -- accumulation:
   --
   --    Item   => [1, 2, 3, 4, 5]
   --    Result => [1, 3, 6, 10, 15]
   --
   -- The values of the input array are withing a given range. This allows the
   -- theorem prover to prove there is no overflow within an accumulation if
   -- the maximum length of the array multiplied by the bounds are within the
   -- fixed type's range.

end Detector.Signals.Lemmas;
