/*
 * menai_vm_float.c — MenaiFloat type implementation.
 */

#include <stdlib.h>
#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_float.h"

static void
MenaiFloat_dealloc(MenaiValue *self)
{
    free(self);
}

PyTypeObject MenaiFloat_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiFloat",          /* tp_name */
    sizeof(MenaiFloat_Object),   /* tp_basicsize */
    0,                           /* tp_itemsize */
    (destructor)MenaiFloat_dealloc, /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue *
menai_float_alloc(double value)
{
    MenaiFloat_Object *self = (MenaiFloat_Object *)malloc(sizeof(MenaiFloat_Object));
    if (self == NULL) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = &MenaiFloat_Type;
    self->ob_destructor = MenaiFloat_dealloc;
    self->value = value;
    return (MenaiValue *)self;
}

int
menai_vm_float_init(void)
{
    return PyType_Ready(&MenaiFloat_Type);
}
