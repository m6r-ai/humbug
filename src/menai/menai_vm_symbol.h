/*
 * menai_vm_symbol.h — MenaiSymbol type definition and API.
 *
 * MenaiSymbol wraps a Python str (the symbol name) as its payload.  There
 * are no singletons; each value is allocated on demand.
 */

#ifndef MENAI_VM_SYMBOL_H
#define MENAI_VM_SYMBOL_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    PyObject *name;     /* Python str */
} MenaiSymbol_Object;

extern PyTypeObject MenaiSymbol_Type;

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_symbol_init(void);

#endif /* MENAI_VM_SYMBOL_H */
