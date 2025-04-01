with Interfaces.C;

package Detector.Module with Pure is

   Bits : constant := Interfaces.C.long'Size;

   type Fixed_Long is delta 1.0
      range -2.0 ** (Bits - 1) .. 2.0 ** (Bits - 1) - 1.0 with
   Size => Bits;

end Detector.Module;
