/*
 * menai_vm_boolean.c — MenaiBoolean type implementation.
 *
 * MenaiBoolean stores a C int (0 or 1).  Two singletons (_Menai_TRUE and
 * _Menai_FALSE) are created at init time.
 */
#include <stdlib.h>

#include "menai_vm_value.h"

#include "menai_vm_boolean.h"

static MenaiBoolean _true_storage;
static MenaiBoolean _false_storage;
static MenaiValue *_Menai_TRUE = NULL;
static MenaiValue *_Menai_FALSE = NULL;

MenaiValue *
menai_boolean_true(void)
{
    return _Menai_TRUE;
}

MenaiValue *
menai_boolean_false(void)
{
    return _Menai_FALSE;
}

void
menai_vm_boolean_init(void)
{
    _true_storage.ob_refcnt = 1;
    _true_storage.ob_type = MENAITYPE_BOOLEAN;
    _true_storage.value = 1;
    _Menai_TRUE = (MenaiValue *)&_true_storage;

    _false_storage.ob_refcnt = 1;
    _false_storage.ob_type = MENAITYPE_BOOLEAN;
    _false_storage.value = 0;
    _Menai_FALSE = (MenaiValue *)&_false_storage;
}
