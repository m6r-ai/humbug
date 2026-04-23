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
} MenaiComplex;

extern MenaiType MenaiComplex_Type;

MenaiValue *menai_complex_alloc(double real, double imag);
int menai_vm_complex_init(void);

#endif /* MENAI_VM_COMPLEX_H */
