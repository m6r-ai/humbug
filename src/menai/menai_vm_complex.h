/*
 * menai_vm_complex.h — MenaiComplex type definition and API.
 *
 * MenaiComplex stores a C double pair (real, imag).  There are no
 * singletons; each value is allocated on demand.
 */

#ifndef MENAI_VM_COMPLEX_H
#define MENAI_VM_COMPLEX_H

#include "menai_vm_object.h"

typedef struct {
    MenaiObject_HEAD
    double real;
    double imag;
} MenaiComplex_Object;

extern MenaiType MenaiComplex_Type;

/*
 * menai_complex_alloc — allocate a new MenaiComplex with the given components.
 * Returns a new reference, or NULL on allocation failure.
 */
MenaiValue *menai_complex_alloc(double real, double imag);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure.
 */
int menai_vm_complex_init(void);

#endif /* MENAI_VM_COMPLEX_H */
