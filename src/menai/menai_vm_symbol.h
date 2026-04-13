/*
 * menai_vm_symbol.h — MenaiSymbol type definition and API.
 *
 * MenaiSymbol wraps an interned Python str as its name.  Interning is applied
 * at construction time so that equality comparisons reduce to a single pointer
 * comparison with no string content inspection.
 */

#ifndef MENAI_VM_SYMBOL_H
#define MENAI_VM_SYMBOL_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    PyObject *name;     /* interned Python str */
} MenaiSymbol_Object;

extern PyTypeObject MenaiSymbol_Type;

/*
 * menai_symbol_alloc — direct C constructor for MenaiSymbol.
 *
 * name must be a Python str (PyUnicode).  It is interned in place and
 * stored as an owned reference.  Returns a new reference, or NULL on
 * failure (Python exception set).
 */
PyObject *menai_symbol_alloc(PyObject *name);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_symbol_init(void);

#endif /* MENAI_VM_SYMBOL_H */
