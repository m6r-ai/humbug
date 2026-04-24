/*
 * menai_vm_struct.c — MenaiStructType and MenaiStruct type implementations.
 *
 * MenaiStructType: field names are stored in an inline C array of
 * (MenaiString name, index) pairs.  A MenaiHashTable built at construction
 * time provides O(1) name-to-index lookup; its slots hold borrowed references
 * into fields[].  All string fields are native MenaiString * values
 * managed with menai_retain/menai_release.
 *
 * MenaiStruct: field values are stored in an inline C array (nfields entries),
 * eliminating the Python tuple previously heap-allocated on every struct
 * construction.
 */
#include <stdlib.h>

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_alloc.h"
#include "menai_vm_value.h"
#include "menai_vm_hashtable.h"
#include "menai_vm_memory.h"
#include "menai_vm_symbol.h"
#include "menai_vm_string.h"

#include "menai_vm_struct.h"

static void
MenaiStructType_dealloc(MenaiValue *self)
{
    MenaiStructType *s = (MenaiStructType *)self;
    menai_ht_free(&s->field_ht);
    menai_xrelease(s->name);
    int n = s->nfields;
    for (int i = 0; i < n; i++) {
        menai_xrelease(s->fields[i].name);
    }

    menai_free(self);
}

/*
 * _build_struct_type — shared constructor body for MenaiStructType.
 * name must be a MenaiString * (borrowed).  tag is a C int.
 * fn_tup must be a Python tuple of PyUnicode field name strings (borrowed).
 * Returns a new reference, or NULL on error.
 */
static MenaiValue *
_build_struct_type(MenaiValue *name, int tag, PyObject *fn_tup)
{
    ssize_t n = PyTuple_GET_SIZE(fn_tup);

    size_t sz = sizeof(MenaiStructType) + (size_t)n * sizeof(MenaiFieldEntry);
    MenaiStructType *self = (MenaiStructType *)menai_alloc(sz);
    if (!self) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = MENAITYPE_STRUCTTYPE;
    self->ob_destructor = MenaiStructType_dealloc;
    menai_retain(name);
    self->field_ht.slots = NULL;
    self->field_ht.slot_count = 0;
    self->field_ht.used = 0;
    self->name = name;
    self->tag = tag;
    self->nfields = (int)n;

    for (ssize_t i = 0; i < n; i++) {
        PyObject *fname = PyTuple_GET_ITEM(fn_tup, i);
        MenaiValue *fname_str = menai_string_from_pyunicode(fname);
        if (!fname_str) {
            /* Release fields already populated, then the object. */
            self->nfields = (int)i;
            MenaiStructType_dealloc((MenaiValue *)self);
            return NULL;
        }

        self->fields[i].name = fname_str;
        self->fields[i].index = (int)i;
    }

    if (menai_ht_init(&self->field_ht, n) < 0) {
        MenaiStructType_dealloc((MenaiValue *)self);
        return NULL;
    }

    for (ssize_t i = 0; i < n; i++) {
        Py_hash_t h = menai_string_hash(self->fields[i].name);
        menai_ht_insert(&self->field_ht, self->fields[i].name, h, i);
    }

    return (MenaiValue *)self;
}

MenaiValue *
menai_struct_type_new_from_args(PyObject *args)
{
    PyObject *py_name = NULL, *field_names = NULL;
    int tag = 0;
    if (!PyArg_ParseTuple(args, "UiO", &py_name, &tag, &field_names)) {
        return NULL;
    }

    PyObject *fn_tup = PySequence_Tuple(field_names);
    if (!fn_tup) {
        return NULL;
    }

    MenaiValue *name = menai_string_from_pyunicode(py_name);
    if (!name) {
        Py_DECREF(fn_tup);
        return NULL;
    }

    MenaiValue *result = _build_struct_type(name, tag, fn_tup);
    menai_release(name);
    Py_DECREF(fn_tup);
    return result;
}

static void
MenaiStruct_dealloc(MenaiValue *self)
{
    MenaiStruct *s = (MenaiStruct *)self;
    menai_xrelease(s->struct_type);
    int n = s->nfields;
    for (int i = 0; i < n; i++) {
        menai_xrelease(s->items[i]);
    }

    menai_free(self);
}

MenaiValue *
menai_struct_alloc(MenaiValue *struct_type, MenaiValue **field_values, ssize_t nfields)
{
    size_t sz = sizeof(MenaiStruct) + (size_t)nfields * sizeof(MenaiValue *);
    MenaiStruct *self = (MenaiStruct *)menai_alloc(sz);
    if (!self) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = MENAITYPE_STRUCT;
    self->ob_destructor = MenaiStruct_dealloc;
    self->nfields = (int)nfields;
    menai_retain(struct_type);
    self->struct_type = struct_type;

    for (ssize_t i = 0; i < nfields; i++) {
        menai_retain(field_values[i]);
        self->items[i] = field_values[i];
    }

    return (MenaiValue *)self;
}
