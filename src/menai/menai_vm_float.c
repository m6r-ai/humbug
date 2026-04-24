/*
 * menai_vm_float.c — MenaiFloat type implementation.
 */
#include <stdlib.h>

#include "menai_vm_alloc.h"
#include "menai_vm_value.h"

#include "menai_vm_float.h"

static void
MenaiFloat_dealloc(MenaiValue *self)
{
    menai_free(self, sizeof(MenaiFloat));
}

MenaiValue *
menai_float_alloc(double value)
{
    MenaiFloat *self = (MenaiFloat *)menai_alloc(sizeof(MenaiFloat));
    if (self == NULL) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = MENAITYPE_FLOAT;
    self->ob_destructor = MenaiFloat_dealloc;
    self->value = value;
    return (MenaiValue *)self;
}
