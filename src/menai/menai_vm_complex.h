/*
 * menai_vm_complex.h — MenaiComplex type definition and API.
 *
 * MenaiComplex stores a C double pair (real, imag).  There are no
 * singletons; each value is allocated on demand.
 */
#ifndef MENAI_VM_COMPLEX_H
#define MENAI_VM_COMPLEX_H

typedef struct {
    MenaiValue_HEAD
    double real;
    double imag;
} MenaiComplex;

MenaiValue *menai_complex_alloc(double real, double imag);

static inline void
menai_complex_dealloc(MenaiValue *self)
{
    menai_free(self);
}

#endif /* MENAI_VM_COMPLEX_H */
