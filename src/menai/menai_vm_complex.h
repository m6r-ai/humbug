/*
 * menai_vm_complex.h — MenaiComplex type definition and API.
 *
 * MenaiComplex stores a C double pair (real, imag).  There are no
 * singletons; each value is allocated on demand.
 */

#ifndef MENAI_VM_COMPLEX_H
#define MENAI_VM_COMPLEX_H

#include "menai_vm_value.h"

typedef struct {
    MenaiValue_HEAD
    double real;
    double imag;
} MenaiComplex;

MenaiValue *menai_complex_alloc(double real, double imag);

#endif /* MENAI_VM_COMPLEX_H */
