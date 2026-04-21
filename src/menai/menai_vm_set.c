/*
 * menai_vm_set.c — MenaiSet type implementation.
 *
 * MenaiSet stores an ordered, deduplicated sequence of elements as two
 * parallel C arrays (elements, hashes) plus a pure-C MenaiHashTable for O(1)
 * membership testing.  Hash values are computed once at construction time via
 * menai_value_hash() and reused for all subsequent set operations.
 *
 * Primary construction path for VM operations: menai_set_from_arrays_steal.
 * menai_set_new_empty() creates the empty-set singleton.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdlib.h>

#include "menai_vm_set.h"
#include "menai_vm_memory.h"
#include "menai_vm_hashtable.h"

/*
 * _set_free_arrays — release n owned references in elements, then free
 * both arrays.  NULL pointers are safely ignored.
 */
static void
_set_free_arrays(MenaiValue *elements, Py_hash_t *hashes, Py_ssize_t n)
{
    if (elements) {
        for (Py_ssize_t i = 0; i < n; i++) menai_xrelease(elements[i]);
        free(elements);
    }
    free(hashes);
}

static void
MenaiSet_dealloc(PyObject *self)
{
    MenaiSet_Object *s = (MenaiSet_Object *)self;
    _set_free_arrays(s->elements, s->hashes, s->length);
    menai_ht_free(&s->ht);
    free(self);
}

PyTypeObject MenaiSet_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiSet",          /* tp_name */
    sizeof(MenaiSet_Object),   /* tp_basicsize */
    0,                             /* tp_itemsize */
    MenaiSet_dealloc,                  /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue
menai_set_from_arrays_steal(MenaiValue *elements, Py_hash_t *hashes, Py_ssize_t n)
{
    MenaiSet_Object *obj = (MenaiSet_Object *)malloc(sizeof(MenaiSet_Object));
    if (!obj) {
        _set_free_arrays(elements, hashes, n);
        return NULL;
    }

    obj->ob_refcnt = 1;
    obj->ob_type = &MenaiSet_Type;

    if (menai_ht_build(&obj->ht, elements, hashes, n) < 0) {
        _set_free_arrays(elements, hashes, n);
        free(obj);
        return NULL;
    }

    obj->elements = elements;
    obj->hashes = hashes;
    obj->length = n;

    return (MenaiValue)obj;
}

MenaiValue
menai_set_new_empty(void)
{
    MenaiSet_Object *obj = (MenaiSet_Object *)malloc(sizeof(MenaiSet_Object));
    if (!obj) return NULL;

    obj->ob_refcnt = 1;
    obj->ob_type = &MenaiSet_Type;
    obj->elements = NULL;
    obj->hashes = NULL;
    obj->ht.slots = NULL;
    obj->ht.slot_count = 0;
    obj->ht.used = 0;
    obj->length = 0;

    return (MenaiValue)obj;
}

int
menai_vm_set_init(void)
{
    if (PyType_Ready(&MenaiSet_Type) < 0) return -1;
    return 0;
}
