#!/usr/bin/env  python3
# *-* encoding=utf8 *-*

# https://docs.python.org/3/extending/extending.html
# https://docs.python.org/3/extending/newtypes_tutorial.html
# https://docs.python.org/3/extending/newtypes.html
# https://docs.python.org/3/c-api/typeobj.html

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

    layout = f"""with Interfaces.C.Strings;

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

   function From_Float (Item : in Interfaces.C.double)
      return Fixed is (Fixed (Item)) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("from_double")}";

   function From_Long (Item : in Interfaces.C.long)
      return Fixed is (Fixed (Item)) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("from_long")}";

   function From_Frac (Num, Den : in Interfaces.C.long)
      return Fixed is (Fixed_Long (Num) / Fixed_Long (Den)) with
      Export        => True,
      Convention    => C,
      External_Name => "{mangle("from_frac")}";

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
#include <iso646.h>

typedef int{fixed.width}_t fixed_t;
#define MANGLE(name) {C_PREFIX}fixed_{fixed.suffix()}__ ## name
extern fixed_t MANGLE(zero)         (void);
extern fixed_t MANGLE(from_long)    (long);
extern fixed_t MANGLE(from_double)  (double);
extern fixed_t MANGLE(from_frac)    (long, long);
extern char *  MANGLE(to_str)       (fixed_t);

typedef struct {{ PyObject_HEAD fixed_t value; }} FixedObject;

static PyObject *
Fixed_new (PyTypeObject *type, PyObject *args, PyObject *kwds) {{
  FixedObject *self;
  self = (FixedObject *)type->tp_alloc(type, 0);
  if (self) {{
    self->value = MANGLE(zero)();
  }}
  return (PyObject*)self;
}}

 
static PyObject *
Fixed_str(FixedObject *obj) {{
  char * temp = MANGLE(to_str) (obj->value);
  PyObject * result = PyUnicode_FromString(temp);
  free(temp);
  return result;
}}

static PyObject *
Fixed_repr(FixedObject *obj) {{
  char * temp = MANGLE(to_str) (obj->value);
  PyObject * result = PyUnicode_FromFormat("{fixed}(%s)", temp);
  free(temp);
  return result;
}}

static int
Fixed_init (FixedObject *self, PyObject *args, PyObject *kwds) {{
  Py_ssize_t size = PyTuple_Size(args);
  self->value = MANGLE(zero) ();
  if (size < 0) {{ return -1; }}
  else if (size == 0) {{ }}
  else if (size == 2) {{
    // A fraction of two integers
    long num, den;
    if (not PyArg_ParseTuple(args, "ll", &num, &den)) {{
      return -1;
    }}
    self->value = MANGLE(from_frac) (num, den);
  }} else if (size == 1) {{
    long as_long;
    double as_double;
    if (PyArg_ParseTuple(args, "l", &as_long)) {{
      self->value = MANGLE(from_long) (as_long);
    }} else if (PyArg_ParseTuple(args, "d", &as_double)) {{
      self->value = MANGLE(from_double) (as_double);
    }} else {{
      return -1;
    }}
  }} else {{
    return -1;
  }}
  return 0;
}}

static PyTypeObject FixedType = {{
  .ob_base = PyVarObject_HEAD_INIT(NULL, 0)
  .tp_name = "{MODULE}.{fixed}",
  .tp_doc = PyDoc_STR("Fixed point"),
  .tp_basicsize = sizeof(FixedObject),
  .tp_itemsize = 0,
  .tp_flags = Py_TPFLAGS_DEFAULT,
  .tp_str = (reprfunc)Fixed_str,
  .tp_repr = (reprfunc)Fixed_repr,
  .tp_init = (initproc) Fixed_init,
  .tp_new = Fixed_new,
}};

extern bool {C_PREFIX}_fixed_{fixed.suffix()}_register (PyObject * m) {{
  if (PyModule_AddObjectRef(m, "{fixed}", (PyObject*)&FixedType) < 0) {{
    Py_DECREF(m);
    return false;
  }}
  return true;
}}

extern bool {C_PREFIX}_fixed_{fixed.suffix()}_prepare (void) {{
  return (PyType_Ready(&FixedType) >= 0);
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
                  for width in [32]
                  for shift in range(-2, 7)]
    gen = "gen"
    # fixed_list = [Fixed(width, shift)
    #               for width in [64, 32]
    #               for shift in range(-29, width - 3 -1)]
    gen = "gen"
    if not os.path.exists(gen):
        os.mkdir(gen)
    for fixed in fixed_list:
        declare_ada_fixed (fixed, gen)
        declare_c_fixed(fixed, gen)
    declare_c_module(fixed_list, gen)
