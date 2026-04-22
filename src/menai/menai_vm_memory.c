/*
 * menai_vm_memory.c — register array allocation and deallocation.
 *
 * The non-inline functions declared in menai_vm_memory.h are implemented here.
 * All inline functions are defined directly in the header.
 */

#include <stdlib.h>

#include "menai_vm_memory.h"

MenaiValue **
menai_regs_alloc(size_t n, MenaiValue *none_val)
{
    MenaiValue **regs = (MenaiValue **)malloc(n * sizeof(MenaiValue *));
    if (regs == NULL) {
        return NULL;
    }

    for (size_t i = 0; i < n; i++) {
        menai_retain(none_val);
        regs[i] = none_val;
    }

    return regs;
}

void
menai_regs_free(MenaiValue **regs, size_t n)
{
    for (size_t i = 0; i < n; i++) {
        menai_release(regs[i]);
    }

    free(regs);
}
