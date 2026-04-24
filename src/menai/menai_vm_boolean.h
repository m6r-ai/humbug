/*
 * menai_vm_boolean.h — MenaiBoolean type definition and API.
 *
 * MenaiBoolean stores a C int (0 or 1).  Two singletons (#t and #f) are
 * created at init time and returned by menai_boolean_true() and
 * menai_boolean_false().
 */
#ifndef MENAI_VM_BOOLEAN_H
#define MENAI_VM_BOOLEAN_H

typedef struct {
    MenaiValue_HEAD
    int value;          /* 0 or 1 */
} MenaiBoolean;

MenaiValue *menai_boolean_true(void);
MenaiValue *menai_boolean_false(void);
void menai_vm_boolean_init(void);

#endif /* MENAI_VM_BOOLEAN_H */
