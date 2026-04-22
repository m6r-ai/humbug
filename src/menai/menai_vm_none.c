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

static MenaiNone_Object _none_storage;
static MenaiValue _Menai_NONE = NULL;

static void
MenaiNone_dealloc(MenaiValue self)
{
    /*
     * The singleton is never freed — its refcount should never reach zero.
     * ob_destructor points here so menai_release has a valid target, but
     * this body is intentionally empty.
     */
    (void)self;
}

PyTypeObject MenaiNone_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiNone",          /* tp_name */
    sizeof(MenaiNone_Object),   /* tp_basicsize */
    0,                          /* tp_itemsize */
    (destructor)MenaiNone_dealloc, /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,         /* tp_flags — no Py_TPFLAGS_HAVE_GC */
};

MenaiValue
menai_none_singleton(void)
{
    return _Menai_NONE;
}

int
menai_vm_none_init(void)
{
    if (PyType_Ready(&MenaiNone_Type) < 0) {
        return -1;
    }

    _none_storage.ob_refcnt = 1;
    _none_storage.ob_type = &MenaiNone_Type;
    _none_storage.ob_destructor = MenaiNone_dealloc;
    _Menai_NONE = (MenaiValue)&_none_storage;
    return 0;
}
