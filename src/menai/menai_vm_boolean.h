/*
 * menai_vm_boolean.h — MenaiBoolean type definition and API.
 *
 * MenaiBoolean stores a C int (0 or 1).  Two singletons (#t and #f) are
 * created at init time and returned by menai_boolean_true() and
 * menai_boolean_false().
 */

#ifndef MENAI_VM_BOOLEAN_H
#define MENAI_VM_BOOLEAN_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    int value;          /* 0 or 1 */
} MenaiBoolean_Object;

extern PyTypeObject MenaiBoolean_Type;

/*
 * Return the #t singleton (borrowed reference).
 * Valid only after menai_vm_boolean_init() has been called.
 */
PyObject *menai_boolean_true(void);

/*
 * Return the #f singleton (borrowed reference).
 * Valid only after menai_vm_boolean_init() has been called.
 */
PyObject *menai_boolean_false(void);

PyObject *MenaiBoolean_describe(PyObject *self, PyObject *args);
PyObject *MenaiBoolean_to_python(PyObject *self, PyObject *args);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_boolean_init(void);

#endif /* MENAI_VM_BOOLEAN_H */
