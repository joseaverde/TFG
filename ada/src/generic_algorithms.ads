with Generic_Signals;

generic
   package Signals is new Generic_Signals (<>);
package Generic_Algorithms with Preelaborate, SPARK_Mode => On is

   function Simpson (
      y     : in Signals.Signal;
      Epoch : in Signals.Epoch_Span;
      dx    : in Real)
      return Real;

end Generic_Algorithms;
