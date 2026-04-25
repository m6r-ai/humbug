/*
 * menai_vm_complex.c — MenaiComplex type implementation.
 */
#include <stdlib.h>

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_alloc.h"
#include "menai_vm_value.h"

#include "menai_vm_complex.h"

MenaiValue *
menai_complex_alloc(double real, double imag)
{
    MenaiComplex *self = (MenaiComplex *)menai_alloc(sizeof(MenaiComplex));
    if (!self) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = MENAITYPE_COMPLEX;
    self->real = real;
    self->imag = imag;

    return (MenaiValue *)self;
}
