/*
 * menai_vm_boolean.c — MenaiBoolean type implementation.
 *
 * MenaiBoolean stores a C int (0 or 1).  Two singletons (_Menai_TRUE and
 * _Menai_FALSE) are created at init time.
 */

#include <stdlib.h>
#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_boolean.h"

static MenaiBoolean_Object _true_storage;
static MenaiBoolean_Object _false_storage;
static MenaiValue _Menai_TRUE = NULL;
static MenaiValue _Menai_FALSE = NULL;

static void
MenaiBoolean_dealloc(MenaiValue self)
{
    /*
     * Singletons are never freed.  ob_destructor points here so
     * menai_release has a valid target, but this body is intentionally empty.
     */
    (void)self;
}

PyTypeObject MenaiBoolean_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiBoolean",          /* tp_name */
    sizeof(MenaiBoolean_Object),   /* tp_basicsize */
    0,                             /* tp_itemsize */
    (destructor)MenaiBoolean_dealloc, /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue
menai_boolean_true(void)
{
    return _Menai_TRUE;
}

MenaiValue
menai_boolean_false(void)
{
    return _Menai_FALSE;
}

int
menai_vm_boolean_init(void)
{
    if (PyType_Ready(&MenaiBoolean_Type) < 0) return -1;

    _true_storage.ob_refcnt = 1;
    _true_storage.ob_type = &MenaiBoolean_Type;
    _true_storage.ob_destructor = MenaiBoolean_dealloc;
    _true_storage.value = 1;
    _Menai_TRUE = (MenaiValue)&_true_storage;

    _false_storage.ob_refcnt = 1;
    _false_storage.ob_type = &MenaiBoolean_Type;
    _false_storage.ob_destructor = MenaiBoolean_dealloc;
    _false_storage.value = 0;
    _Menai_FALSE = (MenaiValue)&_false_storage;

    return 0;
}
