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
 * Small integer cache — covers [-5, 256] inclusive, matching CPython's own
 * small int range.  menai_integer_from_long() returns a borrowed reference
 * to a pre-allocated singleton for values in this range, and allocates
 * fresh otherwise.  Callers must Py_INCREF the result if they need to own it.
 *
 * All allocation sites (make_integer_* in menai_vm_c.c and convert_value in
 * menai_vm_value.c) go through menai_integer_from_long so the cache is
 * always consulted.
 */
#define MENAI_INT_CACHE_MIN (-5)
#define MENAI_INT_CACHE_MAX 256
#define MENAI_INT_CACHE_SIZE (MENAI_INT_CACHE_MAX - MENAI_INT_CACHE_MIN + 1)

/*
 * menai_integer_from_long — return a MenaiInteger for the given value.
 * Returns a borrowed reference for cached values, a new reference otherwise.
 * Returns NULL on MemoryError (Python exception set).
 */
PyObject *menai_integer_from_long(long n);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_integer_init(void);

#endif /* MENAI_VM_INTEGER_H */
