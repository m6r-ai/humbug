/*
 * menai_vm_none.h — MenaiNone type definition and API.
 *
 * MenaiNone is a singleton type with no payload beyond the PyObject header.
 * It represents the absence of a value in the Menai runtime.
 */

#ifndef MENAI_VM_NONE_H
#define MENAI_VM_NONE_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
} MenaiNone_Object;

extern PyTypeObject MenaiNone_Type;

/*
 * Return the MenaiNone singleton (borrowed reference).
 * Valid only after menai_vm_none_init() has been called.
 */
PyObject *menai_none_singleton(void);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_none_init(void);

#endif /* MENAI_VM_NONE_H */
