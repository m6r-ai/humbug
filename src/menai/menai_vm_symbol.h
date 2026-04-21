/*
 * menai_vm_symbol.h — MenaiSymbol type definition and API.
 *
 * MenaiSymbol wraps an interned Python str as its name.  Interning is applied
 * at construction time so that equality comparisons reduce to a single pointer
 * comparison with no string content inspection.
 *
 * The name field remains a PyObject * because symbol names originate from
 * Python source strings and interning is a Python-layer operation.  This is
 * a boundary-layer concern, not part of the object model being migrated.
 */

#ifndef MENAI_VM_SYMBOL_H
#define MENAI_VM_SYMBOL_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_object.h"

typedef struct {
    MenaiObject_HEAD
    PyObject *name;     /* interned Python str */
} MenaiSymbol_Object;

extern MenaiType MenaiSymbol_Type;

/*
 * menai_symbol_alloc — direct C constructor for MenaiSymbol.
 *
 * name must be a Python str (PyUnicode).  It is interned in place and
 * stored as an owned reference.  Returns a new reference, or NULL on
 * failure (Python exception set).
 */
MenaiValue menai_symbol_alloc(PyObject *name);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure.
 */
int menai_vm_symbol_init(void);

#endif /* MENAI_VM_SYMBOL_H */
