/*
 * menai_vm_code.c — MenaiCodeObject lifetime management.
 */
#define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <string.h>

#include "menai_vm_c.h"

void
menai_code_object_release(MenaiCodeObject *co)
{
    if (--co->ob_refcnt > 0) {
        return;
    }

    for (ssize_t i = 0; i < co->nconst; i++) {
        menai_release(co->constants[i]);
    }

    free(co->constants);

    for (ssize_t i = 0; i < co->nnames; i++) {
        free((char *)co->names[i]);
    }

    free(co->names);
    free(co->name_hashes);

    for (ssize_t i = 0; i < co->nparam_names; i++) {
        free(co->param_names[i]);
    }

    free(co->param_names);

    for (ssize_t i = 0; i < co->nchildren; i++) {
        menai_code_object_release(co->children[i]);
    }

    free(co->children);
    free(co->instrs);
    free(co->name);
    free(co);
}

int
menai_code_object_max_locals(const MenaiCodeObject *co)
{
    int best = co->local_count + co->outgoing_arg_slots;
    for (ssize_t i = 0; i < co->nchildren; i++) {
        int child_best = menai_code_object_max_locals(co->children[i]);
        if (child_best > best) {
            best = child_best;
        }
    }

    return best;
}

