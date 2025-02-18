with Ada.Unchecked_Deallocation, Signals, Reals;

package Batchs with Preelaborate, SPARK_Mode => On is

   use Reals;

   subtype Pattern_Type is Signals.Signal (Signals.Epoch_Size);
   type Pattern_Array is array (Positive range <>) of Pattern_Type;

   type Bound_Type is record
      Low, High : Real := 0.0;
   end record;

   type Batch_Type (Pattern_Count : Positive) is limited record
      PSD_1    : Bound_Type;
      PSD_2    : Bound_Type;
      PSD_3    : Bound_Type;
      Energy   : Bound_Type;
      Max_Dist : Bound_Type;
      d_max_c  : Real := 0.0;
      Pj       : Pattern_Array (1 .. Pattern_Count);
   end record;

   type Batch_Access is access Batch_Type;
   procedure Free is new Ada.Unchecked_Deallocation (Batch_Type, Batch_Access);

   function Is_In (Item : in Real; Bound : in Bound_Type) return Boolean is (
      Bound.Low < Item and Item < Bound.High);

end Batchs;
