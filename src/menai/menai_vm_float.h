/*
 * menai_vm_float.h — MenaiFloat type definition and API.
 *
 * MenaiFloat stores a C double.  There are no singletons; each value is
 * allocated on demand.
 */

#ifndef MENAI_VM_FLOAT_H
#define MENAI_VM_FLOAT_H

#include "menai_vm_value.h"

typedef struct {
    MenaiValue_HEAD
    double value;
} MenaiFloat;

MenaiValue *menai_float_alloc(double value);

#endif /* MENAI_VM_FLOAT_H */
