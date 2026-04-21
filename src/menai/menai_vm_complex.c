/*
 * menai_vm_complex.c — MenaiComplex type implementation.
 */

#include <stdlib.h>
#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_complex.h"

static void
MenaiComplex_dealloc(PyObject *self)
{
    free(self);
}

PyTypeObject MenaiComplex_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiComplex",          /* tp_name */
    sizeof(MenaiComplex_Object),   /* tp_basicsize */
    0,                             /* tp_itemsize */
    MenaiComplex_dealloc,          /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue
menai_complex_alloc(double real, double imag)
{
    MenaiComplex_Object *self = (MenaiComplex_Object *)malloc(sizeof(MenaiComplex_Object));
    if (self == NULL) return NULL;
    self->ob_refcnt = 1;
    self->ob_type = &MenaiComplex_Type;
    self->real = real;
    self->imag = imag;
    return (MenaiValue)self;
}

int
menai_vm_complex_init(void)
{
    return PyType_Ready(&MenaiComplex_Type);
}
