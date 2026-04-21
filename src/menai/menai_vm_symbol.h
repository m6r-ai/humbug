/*
 * menai_vm_symbol.h — MenaiSymbol type definition and API.
 *
 * MenaiSymbol stores its name as a MenaiString_Object *.  Equality is
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
    MenaiValue name;    /* owned MenaiString_Object * */
} MenaiSymbol_Object;

extern MenaiType MenaiSymbol_Type;

/*
 * menai_symbol_alloc — direct C constructor for MenaiSymbol.
 *
 * name must be a MenaiString_Object * (as MenaiValue).  A retain is taken
 * on name.  Returns a new reference, or NULL on allocation failure.
 */
MenaiValue menai_symbol_alloc(MenaiValue name);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure.
 */
int menai_vm_symbol_init(void);

#endif /* MENAI_VM_SYMBOL_H */
