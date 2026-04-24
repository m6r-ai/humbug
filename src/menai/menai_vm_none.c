/*
 * menai_vm_none.c — MenaiNone type implementation.
 *
 * MenaiNone is a singleton with no payload.  A single instance (_Menai_NONE)
 * is created at init time and returned by menai_none_singleton().
 */

#include <stdlib.h>
#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_none.h"

static MenaiNone _none_storage;
static MenaiValue *_Menai_NONE = NULL;

static void
MenaiNone_dealloc(MenaiValue *self)
{
    /*
     * The singleton is never freed — its refcount should never reach zero.
     * ob_destructor points here so menai_release has a valid target, but
     * this body is intentionally empty.
     */
    (void)self;
}

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
    _none_storage.ob_destructor = MenaiNone_dealloc;
    _Menai_NONE = (MenaiValue *)&_none_storage;
}
