/*
 * menai_vm_dealloc.h — direct deallocation dispatch for MenaiValue.
 */

#ifndef MENAI_VM_DEALLOC_H
#define MENAI_VM_DEALLOC_H

#include "menai_vm_value.h"

/*
 * menai_dealloc — free a MenaiValue whose reference count has reached zero.
 *
 * Must only be called when val->ob_refcnt == 0.  Defined in menai_vm_dealloc.c.
 */
void menai_dealloc(MenaiValue *val);

#endif /* MENAI_VM_DEALLOC_H */
