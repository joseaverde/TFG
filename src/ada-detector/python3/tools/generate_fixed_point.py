#!/usr/bin/env  python3
# *-* encoding=utf8 *-*

from typing import Final
import os

C_PREFIX : Final[str] = "___c__detector__module__"
MODULE   : Final[str] = "detector"

class Fixed:
    def __init__ (self, width : int, shift : int):
        self.width = width
        self.shift = shift

    def sign (self) -> str:
        return ("_" if self.shift == 0 else
                ("_p_" if self.shift > 0 else "_m_"))

    def suffix (self) -> str:
        return f"{self.width}{self.sign()}{abs(self.shift)}"

    def __str__ (self) -> str:
        return "Fixed_" + self.suffix()

    def __repr__ (self) -> str:
        return "Fixed_" + self.suffix()

# Casting, Multiplication, Division

def declare_ada_fixed (fixed, gen):

    # https://rszalski.github.io/magicmethods/

    def mangle (name):
        return f"{C_PREFIX}fixed_{fixed.suffix()}__{name}"

    layout = f"""with Interfaces.C, Interfaces.C.Strings;

package Detector.Module.{fixed} with Preelaborate, SPARK_Mode is

   use all type Interfaces.C.int;

   Fixed_Delta : constant := 2.0 ** ({-fixed.shift});
   Fixed_Bound : constant := 2.0 ** ({fixed.width - fixed.shift - 1});
   type Fixed is delta Fixed_Delta
      range -Fixed_Bound .. Fixed_Bound - Fixed_Delta with
   Size => {fixed.width}, Convention => C;

   -- Construction

   function Zero return Fixed is (0.0) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("zero")}";

   function From_Float (Item : in Float)
      return Fixed is (Fixed (Item)) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("from_float")}";

   -- Binary operations

   function Add (Left, Right : in Fixed)
      return Fixed is (Left + Right) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("add")}";

   function Sub (Left, Right : in Fixed)
      return Fixed is (Left - Right) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("sub")}";

   function Cmp (Left, Right : in Fixed)
      return Interfaces.C.int is (
      (if Left = Right then 0 elsif Left < Right then -1 else 1)) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("cmp")}";

   -- Unary operations

   function Absf (Right : in Fixed)
      return Fixed is (abs Right) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("abs")}";

   function Pos (Right : in Fixed)
      return Fixed is (+Right) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("pos")}";

   function Neg (Right : in Fixed)
      return Fixed is (-Right) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("neg")}";

   -- Attributes

   function Attr_Delta return Fixed is (Fixed'Delta) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("delta")}";

   -- Conversions

   function To_Long (Item : in Fixed)
      return Interfaces.C.long is (
      Interfaces.C.long (Item)) with
      Export => True,
      Convention => C,
      External_Name => "{mangle("to_long")}";

   function To_Str (Item : in Fixed)
      return Interfaces.C.Strings.chars_ptr is (
      Interfaces.C.Strings.New_String (Item'Image)) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("to_str")}";

end Detector.Module.{fixed};"""
    name = os.path.join(gen, f"detector-module-fixed_{fixed.suffix()}.ads")
    with open(name, 'w') as fp:
        fp.write(layout)

def declare_c_fixed (fixed, gen):
    layout = f"""#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct {{ PyObject_HEAD int{fixed.width}_t value; }} Fixed;

static PyTypeObject FixedObject = {{
  .ob_base = PyVarObject_HEAD_INIT(NULL, 0)
  .tp_name = "{MODULE}.{fixed}",
  .tp_doc = PyDoc_STR("Fixed point"),
  .tp_basicsize = sizeof(Fixed),
  .tp_itemsize = 0,
  .tp_flags = Py_TPFLAGS_DEFAULT,
  .tp_new = PyType_GenericNew,
}};

extern bool {C_PREFIX}_fixed_{fixed.suffix()}_register (PyObject * m) {{
  if (PyModule_AddObjectRef(m, "{fixed}", (PyObject*)&FixedObject) < 0) {{
    Py_DECREF(m);
    return false;
  }}
  return true;
}}

extern bool {C_PREFIX}_fixed_{fixed.suffix()}_prepare (void) {{
  return (PyType_Ready(&FixedObject) >= 0);
}}
"""
    name = os.path.join(gen, f"module_fixed_{fixed.suffix()}.c")
    with open(name, 'w') as fp:
        fp.write(layout)

def declare_c_module (fixed_list, gen):
    layout = f"""#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>
#include <stdbool.h>
#include <iso646.h>

static PyMethodDef DetectorMethods[] = {{
  {{NULL, NULL, 0, NULL}}
}};

static struct PyModuleDef DetectorModule = {{
  PyModuleDef_HEAD_INIT,
  "{MODULE}",   /* name of module */
  NULL,         /* module documentation, may be NULL */
  -1,           /* size of per-interpreter state of the module,
                   or -1 if the module keeps state in global variables. */
  DetectorMethods
}};

{"\n".join([f"extern bool {C_PREFIX}_fixed_{fixed.suffix()}_prepare (void);"
            for fixed in fixed_list])}
{"\n".join([f"extern bool {C_PREFIX}_fixed_{fixed.suffix()}_register (PyObject *m);"
            for fixed in fixed_list])}

PyMODINIT_FUNC
PyInit_detector(void) {{
  PyObject *m;

{"\n".join([f"  if (not {C_PREFIX}_fixed_{fixed.suffix()}_prepare()) return NULL;"
            for fixed in fixed_list])}

  m = PyModule_Create(&DetectorModule);
  if (m == NULL) return NULL;

{"\n".join([f"  if (not {C_PREFIX}_fixed_{fixed.suffix()}_register(m)) return NULL;"
            for fixed in fixed_list])}

  return m;
}}
"""
    name = os.path.join(gen, "module.c")
    with open(name, 'w') as fp:
        fp.write(layout)

if __name__ == "__main__":
    fixed_list = [Fixed(width, shift)
                  for width in [64, 32]
                  for shift in range(-29, width - 3 -1)]
    gen = "gen"
    if not os.path.exists(gen):
        os.mkdir(gen)
    for fixed in fixed_list:
        declare_ada_fixed (fixed, gen)
        declare_c_fixed(fixed, gen)
    declare_c_module(fixed_list, gen)
