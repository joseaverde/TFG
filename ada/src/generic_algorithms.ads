with Generic_Signals, Types;

generic
   with package Signals is new Generic_Signals (<>);
package Generic_Algorithms with Preelaborate, SPARK_Mode => On is

   use Signals;

-- function Simpson (
--    y     : in Signal;
--    Epoch : in Epoch_Span;
--    dx    : in Real)
--    return Real;

   function Max_Dist (
      Signal : in Signals.Signal;
      Epoch  : in Epoch_Span)
      return Real with
      Pre => Signal.Is_Valid_Span (Epoch);

   function Mean (
      Signal : in Signals.Signal;
      Epoch  : in Epoch_Span)
      return Sample with
      Pre => Signal.Is_Valid_Span (Epoch);

-- function Energy (
--    Signal : in Signals.Signal;
--    Epoch  : in Epoch_Span)
--    return Real with
--    Pre => Signal.Is_Valid_Span (Epoch);

   type Real_Array is array (Types.Index_Type range <>) of Real;
   generic
      First : in Real;
      Last  : in Real;
      Max   : in Types.Positive_Count_Type;
   package Generic_Accumulation is
      -- https://stackoverflow.com/questions/44808451/
      -- spark-ada-postcondition-for-array-total

      use type Types.Count_Type;

      subtype Input_Real is Real
         range First .. Last;
      subtype Output_Real is Real
         range First * Real (Max) .. First * Real (Max);

      function Partial_Accumulate (Item : in Real_Array)
         return Real_Array with
         Pre  => Item'Length in 1 .. Max
                 and then (for all Value of Item => Value in Input_Real),
         Post => Partial_Accumulate'Result'Length = Item'Length
                 and then Partial_Accumulate'Result'First = Item'First
                 and then (for all Value of Item => Value in Output_Real)
                 and then (for all I in Item'First .. Item'Last =>
                              Partial_Accumulate'Result (I)
                              in Real (I - Item'First + 1) * Input_Real'First
                              .. Real (I - Item'First + 1) * Input_Real'Last)
                 and then Partial_Accumulate'Result (Item'First)
                        = Item (Item'First)
                 and then (for all I in Item'First + 1 .. Item'Last =>
                              Partial_Accumulate'Result (I) =
                                 Partial_Accumulate'Result (I - 1) + Item (I));

   -- function Accumulate (Item : in Real_Array) return Output_Real with
   --    Pre  => Item'Length in 1 .. Max
   --            and then (for all Value of Item => Value in Input_Real),
   --    Post => Accumulate'Result = Partial_Accumulate (Item) (Item'Last);
   end Generic_Accumulation;

end Generic_Algorithms;
