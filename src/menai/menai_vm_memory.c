/*
 * menai_vm_memory.c — Memory management API implementation for the Menai VM.
 *
 * The non-inline functions declared in menai_vm_memory.h are implemented here.
 * All inline functions are defined directly in the header.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_memory.h"

PyObject **
menai_regs_alloc(Py_ssize_t n, PyObject *none_val)
{
    PyObject **regs = (PyObject **)PyMem_Malloc((size_t)n * sizeof(PyObject *));
    if (regs == NULL) {
        PyErr_NoMemory();
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        Py_INCREF(none_val);
        regs[i] = none_val;
    }

    return regs;
}

void
menai_regs_free(PyObject **regs, Py_ssize_t n)
{
    if (regs == NULL) return;
    for (Py_ssize_t i = 0; i < n; i++) Py_DECREF(regs[i]);
    PyMem_Free(regs);
}
