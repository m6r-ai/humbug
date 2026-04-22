/*
 * menai_vm_function.c — MenaiFunction type implementation.
 *
 * MenaiFunction represents a Menai closure.  It holds a retained reference to
 * a MenaiCodeObject (which owns all frame metadata) and an inline C array of
 * captured MenaiValue *s.  No Python objects are referenced after construction.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdlib.h>
#include <stdint.h>

#include "menai_vm_function.h"
#include "menai_vm_memory.h"

static void
MenaiFunction_dealloc(MenaiValue *self)
{
    MenaiFunction *f = (MenaiFunction *)self;
    menai_code_object_release(f->bytecode);
    Py_ssize_t ncap = f->ncap;
    for (Py_ssize_t i = 0; i < ncap; i++) {
        menai_xrelease(f->captures[i]);
    }

    free(self);
}

PyTypeObject MenaiFunction_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiFunction",                        /* tp_name */
    sizeof(MenaiFunction) - sizeof(MenaiValue *), /* tp_basicsize */
    0,                                            /* tp_itemsize */
    (destructor)MenaiFunction_dealloc, /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue *
menai_function_alloc(MenaiCodeObject *co, MenaiValue *none_val)
{
    Py_ssize_t ncap = co->ncap;
    MenaiFunction *self = (MenaiFunction *)malloc(
        sizeof(MenaiFunction) + (size_t)ncap * sizeof(MenaiValue *));
    if (!self) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = &MenaiFunction_Type;
    self->ob_destructor = MenaiFunction_dealloc;
    self->ncap = ncap;
    menai_code_object_retain(co);
    self->bytecode = co;

    for (Py_ssize_t i = 0; i < ncap; i++) {
        menai_retain(none_val);
        self->captures[i] = none_val;
    }

    return (MenaiValue *)self;
}

int
menai_vm_function_init(void)
{
    if (PyType_Ready(&MenaiFunction_Type) < 0) {
        return -1;
    }

    return 0;
}
