/*
 * menai_vm_complex.c — MenaiComplex type implementation.
 */
#include <stdlib.h>

#include "menai_vm_alloc.h"
#include "menai_vm_complex.h"

static void
MenaiComplex_dealloc(MenaiValue *self)
{
    menai_free(self, sizeof(MenaiComplex));
}

MenaiValue *
menai_complex_alloc(double real, double imag)
{
    MenaiComplex *self = (MenaiComplex *)menai_alloc(sizeof(MenaiComplex));
    if (self == NULL) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = MENAITYPE_COMPLEX;
    self->ob_destructor = MenaiComplex_dealloc;
    self->real = real;
    self->imag = imag;
    return (MenaiValue *)self;
}
