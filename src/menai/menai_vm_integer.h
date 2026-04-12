/*
 * menai_vm_integer.h — MenaiInteger type definition and API.
 *
 * MenaiInteger wraps a Python int (arbitrary precision) as its payload.
 * There are no singletons; each value is allocated on demand.
 */

#ifndef MENAI_VM_INTEGER_H
#define MENAI_VM_INTEGER_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    PyObject *value;    /* Python int (arbitrary precision) */
} MenaiInteger_Object;

extern PyTypeObject MenaiInteger_Type;

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_integer_init(void);

#endif /* MENAI_VM_INTEGER_H */
