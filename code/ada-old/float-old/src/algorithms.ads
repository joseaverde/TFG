with Reals, Signals;
use Reals, Signals;

package Algorithms with Preelaborate, SPARK_Mode => On is

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

   function Energy (
      Signal : in Signals.Signal;
      Epoch  : in Epoch_Span)
      return Real with
      Pre => Signal.Is_Valid_Span (Epoch);

end Algorithms;
