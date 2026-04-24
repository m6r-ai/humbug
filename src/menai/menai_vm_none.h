/*
 * menai_vm_none.h — MenaiNone type definition and API.
 *
 * MenaiNone is a singleton type with no payload beyond the object header.
 * It represents the absence of a value in the Menai runtime.
 */

#ifndef MENAI_VM_NONE_H
#define MENAI_VM_NONE_H

#include "menai_vm_value.h"

typedef struct {
    MenaiValue_HEAD
} MenaiNone;

MenaiValue *menai_none_singleton(void);
void menai_vm_none_init(void);

#endif /* MENAI_VM_NONE_H */
