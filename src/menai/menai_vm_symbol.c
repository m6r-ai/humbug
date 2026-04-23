/*
 * menai_vm_symbol.c — MenaiSymbol type implementation.
 *
 * MenaiSymbol stores its name as an owned MenaiString *.  Equality
 * is determined by menai_string_equal() on the name field.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdlib.h>

#include "menai_vm_alloc.h"
#include "menai_vm_symbol.h"

static void
MenaiSymbol_dealloc(MenaiValue *self)
{
    menai_xrelease(((MenaiSymbol *)self)->name);
    menai_free(self, sizeof(MenaiSymbol));
}

PyTypeObject MenaiSymbol_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiSymbol",          /* tp_name */
    sizeof(MenaiSymbol),   /* tp_basicsize */
    0,                            /* tp_itemsize */
    (destructor)MenaiSymbol_dealloc, /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue *
menai_symbol_alloc(MenaiValue *name)
{
    MenaiSymbol *self = (MenaiSymbol *)menai_alloc(sizeof(MenaiSymbol));
    if (self == NULL) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = &MenaiSymbol_Type;
    self->ob_destructor = MenaiSymbol_dealloc;
    menai_retain(name);
    self->name = name;

    return (MenaiValue *)self;
}

int
menai_vm_symbol_init(void)
{
    return PyType_Ready(&MenaiSymbol_Type);
}
