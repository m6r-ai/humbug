/*
 * menai_vm_dealloc.h — direct deallocation dispatch for MenaiValue.
 *
 * menai_dealloc() replaces the indirect val->ob_type->tp_dealloc() call in
 * menai_release().  It switches on ob_type pointer identity — all type objects
 * are static singletons — giving the compiler a small integer-like comparison
 * that it can turn into a jump table and, for the simple cases, inline the
 * free() call directly.
 *
 * This eliminates two pointer chases and an unpredictable indirect branch on
 * every object deallocation, which is the dominant cost when freeing lists
 * whose elements span multiple types.
 */

#ifndef MENAI_VM_DEALLOC_H
#define MENAI_VM_DEALLOC_H

#include "menai_vm_object.h"

/*
 * menai_dealloc — free a MenaiValue whose reference count has reached zero.
 *
 * Must only be called when val->ob_refcnt == 0.  Defined in menai_vm_dealloc.c.
 */
void menai_dealloc(MenaiValue *val);

#endif /* MENAI_VM_DEALLOC_H */
