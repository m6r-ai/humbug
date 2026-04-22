/*
 * menai_vm_float.h — MenaiFloat type definition and API.
 *
 * MenaiFloat stores a C double.  There are no singletons; each value is
 * allocated on demand.
 */

#ifndef MENAI_VM_FLOAT_H
#define MENAI_VM_FLOAT_H

#include "menai_vm_object.h"

typedef struct {
    MenaiObject_HEAD
    double value;
} MenaiFloat_Object;

extern MenaiType MenaiFloat_Type;

/*
 * menai_float_alloc — allocate a new MenaiFloat with the given value.
 * Returns a new reference, or NULL on allocation failure.
 */
MenaiValue *menai_float_alloc(double value);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure.
 */
int menai_vm_float_init(void);

#endif /* MENAI_VM_FLOAT_H */
