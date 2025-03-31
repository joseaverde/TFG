#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>
#include <iso646.h>

typedef struct { PyObject_HEAD int32_t value; } Fixed_32;
typedef struct { PyObject_HEAD int64_t value; } Fixed_64;

#define define_fixed_class_helper(bits,prefix,shift)                          \
  static PyTypeObject Fixed_##bits##_##prefix##_##shift = {                   \
    .ob_base = PyVarObject_HEAD_INIT(NULL, 0)                                 \
    .tp_name = "spam.Fixed" #bits #prefix #bits,                              \
    .tp_doc = PyDoc_STR("Fixed point"),                                       \
    .tp_basicsize = sizeof(Fixed_##bits),                                     \
    .tp_itemsize = 0,                                                         \
    .tp_flags = Py_TPFLAGS_DEFAULT,                                           \
    .tp_new = PyType_GenericNew,                                              \
  }
#define define_fixed_32(bits) define_fixed_class_helper(32,p,bits)
#define define_fixed_32_neg(bits) define_fixed_class_helper(32,m,bits)
#define define_fixed_64(bits) define_fixed_class_helper(64,p,bits)
#define define_fixed_64_neg(bits) define_fixed_class_helper(64,m,bits)

#define init_fixed_class_helper(m,bits,prefix,shift)                          \
  if (PyModule_AddObjectRef(                                                  \
        m, "Fixed" #bits #prefix #shift,                                      \
        (PyObject *)&Fixed_##bits##_##prefix##_##shift) < 0) {                \
      Py_DECREF(m);                                                           \
      return NULL;                                                            \
  }
#define init_fixed_32(m,bits) init_fixed_class_helper(m,32,p,bits)
#define init_fixed_32_neg(mod,bits) init_fixed_class_helper(mod,32,m,bits)
#define init_fixed_64(m,bits) init_fixed_class_helper(m,64,p,bits)
#define init_fixed_64_neg(mod,bits) init_fixed_class_helper(mod,64,m,bits)

#define declare_fixed_class_helper(bits,prefix,shift)                         \
  if (PyType_Ready(&Fixed_##bits##_##prefix##_##shift) < 0) return NULL;
#define declare_fixed_32(bits) declare_fixed_class_helper(32,p,bits)
#define declare_fixed_32_neg(bits) declare_fixed_class_helper(32,m,bits)
#define declare_fixed_64(bits) declare_fixed_class_helper(32,p,bits)
#define declare_fixed_64_neg(bits) declare_fixed_class_helper(32,m,bits)

define_fixed_32(8);

static PyObject *
spam_system(PyObject *self, PyObject *args) {
  const char *command;
  int sts;

  if (!PyArg_ParseTuple(args, "s", &command)) return NULL;
  sts = system(command);
  return PyLong_FromLong(sts);
}

static PyMethodDef SpamMethods[] = {
  {"system",  spam_system, METH_VARARGS,
   "Execute a shell command."},
  {NULL, NULL, 0, NULL}        /* Sentinel */
};

static struct PyModuleDef spammodule = {
  PyModuleDef_HEAD_INIT,
  "spam",   /* name of module */
  NULL,     /* module documentation, may be NULL */
  -1,       /* size of per-interpreter state of the module,
               or -1 if the module keeps state in global variables. */
  SpamMethods
};

PyMODINIT_FUNC
PyInit_spam(void) {
  PyObject *m;

  declare_fixed_32(8);

  m = PyModule_Create(&spammodule);
  if (m == NULL) return NULL;

  init_fixed_32(m,8);

  return m;
}
