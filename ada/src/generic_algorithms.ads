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

end Generic_Algorithms;
