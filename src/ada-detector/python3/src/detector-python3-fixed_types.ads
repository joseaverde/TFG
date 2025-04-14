with Interfaces.C;

package Detector.Python3.Fixed_Types with Pure is

   -- For interfacing with C

   Long_Bits : constant := Interfaces.C.long'Size;
   type Fixed_Long is delta 1.0
      range -2.0 ** (Long_Bits - 1) .. 2.0 ** (Long_Bits - 1) - 1.0 with
   Size => Long_Bits;

end Detector.Python3.Fixed_Types;
