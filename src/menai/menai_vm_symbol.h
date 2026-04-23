/*
 * menai_vm_symbol.h — MenaiSymbol type definition and API.
 *
 * MenaiSymbol stores its name as a MenaiString *.  Equality is
 * determined by menai_string_equal() on the name field.
 */

#ifndef MENAI_VM_SYMBOL_H
#define MENAI_VM_SYMBOL_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_object.h"
#include "menai_vm_string.h"

typedef struct {
    MenaiObject_HEAD
    MenaiValue *name;    /* owned MenaiString * */
} MenaiSymbol;

extern MenaiType MenaiSymbol_Type;

MenaiValue *menai_symbol_alloc(MenaiValue *name);
int menai_vm_symbol_init(void);

#endif /* MENAI_VM_SYMBOL_H */
