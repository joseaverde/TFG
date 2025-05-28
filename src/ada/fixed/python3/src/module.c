#define PY_SSIZE_T_CLEAN
#define Py_LIMITED_API
#include <Python.h>
#include <iso646.h>

static PyMethodDef DetectorMethods[] = {
  {NULL, NULL, 0, NULL}
};

static struct PyModuleDef DetectorModule = {
  PyModuleDef_HEAD_INIT,
  "detector",
  "Seizure detector utilities (fixed point implementation)",
  -1,
  DetectorMethods
};

PyMODINIT_FUNC
PyInit_detector (void) {
  PyObject *m;
  m = PyModule_Create(&DetectorModule);
  return m;
}
