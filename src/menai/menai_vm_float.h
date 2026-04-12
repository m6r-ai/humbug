/*
 * menai_vm_float.h — MenaiFloat type definition and API.
 *
 * MenaiFloat stores a C double.  There are no singletons; each value is
 * allocated on demand.
 */

#ifndef MENAI_VM_FLOAT_H
#define MENAI_VM_FLOAT_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    double value;
} MenaiFloat_Object;

extern PyTypeObject MenaiFloat_Type;

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_float_init(void);

#endif /* MENAI_VM_FLOAT_H */
