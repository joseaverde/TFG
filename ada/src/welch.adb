with Ada.Numerics;
with Reals;

package body Welch with
   SPARK_Mode    => On,
   Refined_State => (The_Window => (Window))
is

   use Ada.Numerics, Reals, Reals.Elementary_Functions, Signals;

   subtype Uniformly_Distributed is Real range 0.0 .. 1.0;
   type Uniformly_Distributed_Array is
      array (Index_Type range 1 .. Epoch_Size)
      of Uniformly_Distributed;

   Window : constant Uniformly_Distributed_Array := [
      for I in 1 .. Epoch_Size =>
         0.5 - 0.5 * Cos (2.0 * Pi * Real (I - 1) / Real (Epoch_Size - 1))];

   function Evaluate (
      Welch   : in out Welch_Type;
      x       : in Signal;
      Span    : in Span_Type;
      Freq    : in Real;
      Overlap : in Positive_Count_Type)
      return Freq_Array with
      Pre => Overlap < Signals.Epoch_Size
         and then x.Is_Valid_Span (Span)
         and then Size (Span) >= Epoch_Size is
      Result : Freq_Array;
      Steps  : constant Count_Type := (Size (Span) - Epoch_Size) / Overlap + 1;
      F      : constant Real := Freq / 2.0;
   begin
      for Win in Windows loop
         Temp := [for I in 1 .. Epoch_Size =>
                     Window (I) * Win (Epoch, I)];
         Execute (Welch.Plan, In_Signal, In_Span, W);
         for I in 1 .. Epoch_Size / 2 + 1 loop
            Result (I) := @ + Norm_Squared(W (I) / (Normalisation_Factor * Freq);
         end loop;
      end loop
      for I in 1 .. Epoch_Size / 2 + 1 loop
         Pxx (I) := @ / Steps;
      end loop;
   end Evaluate;

end Welch;
