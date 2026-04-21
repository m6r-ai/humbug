/*
 * menai_vm_boolean.h — MenaiBoolean type definition and API.
 *
 * MenaiBoolean stores a C int (0 or 1).  Two singletons (#t and #f) are
 * created at init time and returned by menai_boolean_true() and
 * menai_boolean_false().
 */

#ifndef MENAI_VM_BOOLEAN_H
#define MENAI_VM_BOOLEAN_H

#include "menai_vm_object.h"

typedef struct {
    MenaiObject_HEAD
    int value;          /* 0 or 1 */
} MenaiBoolean_Object;

extern MenaiType MenaiBoolean_Type;

/*
 * Return the #t singleton (borrowed reference).
 * Valid only after menai_vm_boolean_init() has been called.
 */
MenaiValue menai_boolean_true(void);

/*
 * Return the #f singleton (borrowed reference).
 * Valid only after menai_vm_boolean_init() has been called.
 */
MenaiValue menai_boolean_false(void);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure.
 */
int menai_vm_boolean_init(void);

#endif /* MENAI_VM_BOOLEAN_H */
