/*
 * menai_vm_function.c — MenaiFunction type implementation.
 *
 * MenaiFunction represents a Menai closure.  It holds a retained reference to
 * a MenaiCodeObject (which owns all frame metadata) and an inline C array of
 * captured MenaiValue *s.  No Python objects are referenced after construction.
 */
#include <stdlib.h>
#include <stdint.h>

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_alloc.h"
#include "menai_vm_value.h"
#include "menai_vm_hashtable.h"
#include "menai_vm_memory.h"
#include "menai_vm_code.h"

#include "menai_vm_function.h"

static void
MenaiFunction_dealloc(MenaiValue *self)
{
    MenaiFunction *f = (MenaiFunction *)self;
    menai_code_object_release(f->bytecode);
    ssize_t ncap = f->ncap;
    for (ssize_t i = 0; i < ncap; i++) {
        menai_xrelease(f->captures[i]);
    }

    size_t sz = sizeof(MenaiFunction) + (size_t)ncap * sizeof(MenaiValue *);
    menai_free(self, sz);
}

MenaiValue *
menai_function_alloc(MenaiCodeObject *co, MenaiValue *none_val)
{
    ssize_t ncap = co->ncap;
    size_t sz = sizeof(MenaiFunction) + (size_t)ncap * sizeof(MenaiValue *);
    MenaiFunction *self = (MenaiFunction *)menai_alloc(sz);
    if (!self) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = MENAITYPE_FUNCTION;
    self->ob_destructor = MenaiFunction_dealloc;
    self->ncap = ncap;
    menai_code_object_retain(co);
    self->bytecode = co;

    for (ssize_t i = 0; i < ncap; i++) {
        menai_retain(none_val);
        self->captures[i] = none_val;
    }

    return (MenaiValue *)self;
}
