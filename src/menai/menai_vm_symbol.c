/*
 * menai_vm_symbol.c — MenaiSymbol type implementation.
 *
 * MenaiSymbol stores its name as an owned MenaiString *.  Equality
 * is determined by menai_string_equal() on the name field.
 */
#include <stdlib.h>

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_alloc.h"
#include "menai_vm_value.h"

#include "menai_vm_symbol.h"

static void
MenaiSymbol_dealloc(MenaiValue *self)
{
    menai_xrelease(((MenaiSymbol *)self)->name);
    menai_free(self);
}

MenaiValue *
menai_symbol_alloc(MenaiValue *name)
{
    MenaiSymbol *self = (MenaiSymbol *)menai_alloc(sizeof(MenaiSymbol));
    if (self == NULL) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = MENAITYPE_SYMBOL;
    self->ob_destructor = MenaiSymbol_dealloc;
    menai_retain(name);
    self->name = name;

    return (MenaiValue *)self;
}
