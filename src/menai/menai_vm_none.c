/*
 * menai_vm_none.c — MenaiNone type implementation.
 *
 * MenaiNone is a singleton with no payload.  A single instance (_Menai_NONE)
 * is created at init time and returned by menai_none_singleton().
 */
#include <stdlib.h>

#include "menai_vm_value.h"

#include "menai_vm_none.h"

static MenaiNone _none_storage;
static MenaiValue *_Menai_NONE = NULL;

MenaiValue *
menai_none_singleton(void)
{
    return _Menai_NONE;
}

void
menai_vm_none_init(void)
{
    _none_storage.ob_refcnt = 1;
    _none_storage.ob_type = MENAITYPE_NONE;
    _Menai_NONE = (MenaiValue *)&_none_storage;
}
