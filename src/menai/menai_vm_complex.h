/*
 * menai_vm_complex.h — MenaiComplex type definition and API.
 *
 * MenaiComplex stores a C double pair (real, imag).  There are no
 * singletons; each value is allocated on demand.
 */

#ifndef MENAI_VM_COMPLEX_H
#define MENAI_VM_COMPLEX_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    double real;
    double imag;
} MenaiComplex_Object;

extern PyTypeObject MenaiComplex_Type;

/*
 * Return a new Python unicode string describing the complex value.
 * Called directly by the VM for OP_COMPLEX_TO_STRING.
 */
PyObject *MenaiComplex_describe(PyObject *self, PyObject *args);

PyObject *MenaiComplex_to_python(PyObject *self, PyObject *args);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_complex_init(void);

#endif /* MENAI_VM_COMPLEX_H */
