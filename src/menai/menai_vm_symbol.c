/*
 * menai_vm_symbol.c — MenaiSymbol type implementation.
 *
 * MenaiSymbol wraps an interned Python str.  Interning is applied at
 * construction time so that symbol equality reduces to a single pointer
 * comparison.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdlib.h>

#include "menai_vm_symbol.h"

static void
MenaiSymbol_dealloc(MenaiValue self)
{
    Py_XDECREF(((MenaiSymbol_Object *)self)->name);  /* name is a Python-owned interned str */
    free(self);
}

PyTypeObject MenaiSymbol_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiSymbol",          /* tp_name */
    sizeof(MenaiSymbol_Object),   /* tp_basicsize */
    0,                            /* tp_itemsize */
    (destructor)MenaiSymbol_dealloc, /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue
menai_symbol_alloc(PyObject *name)
{
    Py_INCREF(name);
    PyUnicode_InternInPlace(&name);

    MenaiSymbol_Object *self = (MenaiSymbol_Object *)malloc(sizeof(MenaiSymbol_Object));
    if (self == NULL) {
        Py_DECREF(name);
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = &MenaiSymbol_Type;
    self->ob_destructor = MenaiSymbol_dealloc;
    self->name = name;

    return (MenaiValue)self;
}

int
menai_vm_symbol_init(void)
{
    return PyType_Ready(&MenaiSymbol_Type);
}
