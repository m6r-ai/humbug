/*
 * menai_vm_complex.h — MenaiComplex type definition and API.
 *
 * MenaiComplex wraps a Python complex as its payload.  There are no
 * singletons; each value is allocated on demand.
 */

#ifndef MENAI_VM_COMPLEX_H
#define MENAI_VM_COMPLEX_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    PyObject *value;    /* Python complex */
} MenaiComplex_Object;

extern PyTypeObject MenaiComplex_Type;

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_complex_init(void);

#endif /* MENAI_VM_COMPLEX_H */
