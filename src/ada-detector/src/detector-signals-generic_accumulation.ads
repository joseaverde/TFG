--/-------------------------------------------------------------------------\--
--| Copyright (c) 2023-2025 José Antonio Verde Jiménez  All Rights Reserved |--
--|-------------------------------------------------------------------------|--
--| File:    detector-signals-generic_accumuation.ads                       |--
--| Author:  José Antonio Verde Jiménez  <joseaverde@protonmail.com>        |--
--| License: European Union Public License 1.2                              |--
--\-------------------------------------------------------------------------/--

private generic
   type Fixed_Type is delta <>;
   type Index_Type is range <>;
   type Array_Type is array (Index_Type range <>) of Fixed_Type;
   First : in Fixed_Type;
   Last  : in Fixed_Type;
function Detector.Signals.Generic_Accumulation (
   Item : in Array_Type)
   return Array_Type with
   Ghost    => True,
   Global   => null,
   Pre      => Item'Length > 0
      and then (for all X of Item => X in First .. Last),
   Post     => (
      declare  Result renames Detector.Signals.Generic_Accumulation'Result;
      begin    Result'First = Item'First
      and then Result'Length = Item'Length
      and then Result (Item'First) = Item (Item'First)
      and then (for all I in Item'First + 1 .. Item'Last =>
                  Result (I - 1) in Positive (I - Item'First) * First
                                 .. Positive (I - Item'First) * Last
                  and then Result (I) = Result (I - 1) + Item (I))
      and then Result (Item'Last) in Item'Length * First
                                  .. Item'Length * Last),
   Pure, SPARK_Mode;
-- This is a ghost functions for proofs that require some kind of accumulation.
-- It returns an array with the intermediate results of the accumulation:
--
--    Item   => [1, 2, 3, 4, 5]
--    Result => [1, 3, 6, 10, 15]
--
-- The values of the input array are withing a given range. This allows the
-- theorem prover to prove there is no overflow within an accumulation if the
-- maximum length of the array multiplied by the bounds are within the fixed
-- type's range.
