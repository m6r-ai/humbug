/*
 * menai_vm_struct.c — MenaiStructType and MenaiStruct type implementations.
 *
 * MenaiStructType: field names are stored in an inline C array of
 * (MenaiString name, index) pairs.  A MenaiHashTable built at construction
 * time provides O(1) name-to-index lookup; its slots hold borrowed references
 * into fields[].  All string fields are native MenaiString_Object * values
 * managed with menai_retain/menai_release.
 *
 * MenaiStruct: field values are stored in an inline C array (nfields entries),
 * eliminating the Python tuple previously heap-allocated on every struct
 * construction.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdlib.h>

#include "menai_vm_struct.h"
#include "menai_vm_memory.h"
#include "menai_vm_symbol.h"
#include "menai_vm_string.h"
#include "menai_vm_hashtable.h"

/* ---------------------------------------------------------------------------
 * MenaiStructType
 * ------------------------------------------------------------------------- */

static void
MenaiStructType_dealloc(MenaiValue self)
{
    MenaiStructType_Object *s = (MenaiStructType_Object *)self;
    menai_ht_free(&s->field_ht);
    menai_xrelease(s->name);
    int n = s->nfields;
    for (int i = 0; i < n; i++) {
        menai_xrelease(s->fields[i].name);
    }

    free(self);
}

PyTypeObject MenaiStructType_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiStructType",          /* tp_name */
    sizeof(MenaiStructType_Object),   /* tp_basicsize */
    0,                             /* tp_itemsize */
    (destructor)MenaiStructType_dealloc, /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

/*
 * _build_struct_type — shared constructor body for MenaiStructType.
 * name must be a MenaiString_Object * (borrowed).  tag is a C int.
 * fn_tup must be a Python tuple of PyUnicode field name strings (borrowed).
 * Returns a new reference, or NULL on error.
 */
static MenaiValue
_build_struct_type(MenaiValue name, int tag, PyObject *fn_tup)
{
    Py_ssize_t n = PyTuple_GET_SIZE(fn_tup);

    MenaiStructType_Object *self = (MenaiStructType_Object *)malloc(
        sizeof(MenaiStructType_Object) + (size_t)n * sizeof(MenaiFieldEntry));
    if (!self) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = &MenaiStructType_Type;
    self->ob_destructor = MenaiStructType_dealloc;
    menai_retain(name);
    self->field_ht.slots = NULL;
    self->field_ht.slot_count = 0;
    self->field_ht.used = 0;
    self->name = name;
    self->tag = tag;
    self->nfields = (int)n;

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *fname = PyTuple_GET_ITEM(fn_tup, i);
        MenaiValue fname_str = menai_string_from_pyunicode(fname);
        if (!fname_str) {
            /* Release fields already populated, then the object. */
            self->nfields = (int)i;
            MenaiStructType_dealloc((MenaiValue)self);
            return NULL;
        }

        self->fields[i].name = fname_str;
        self->fields[i].index = (int)i;
    }

    if (menai_ht_init(&self->field_ht, n) < 0) {
        MenaiStructType_dealloc((MenaiValue)self);
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        Py_hash_t h = menai_string_hash(self->fields[i].name);
        menai_ht_insert(&self->field_ht, self->fields[i].name, h, i);
    }

    return (MenaiValue)self;
}

MenaiValue
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

    MenaiValue name = menai_string_from_pyunicode(py_name);
    if (!name) {
        Py_DECREF(fn_tup);
        return NULL;
    }

    MenaiValue result = _build_struct_type(name, tag, fn_tup);
    menai_release(name);
    Py_DECREF(fn_tup);
    return result;
}

/* ---------------------------------------------------------------------------
 * MenaiStruct
 * ------------------------------------------------------------------------- */

static void
MenaiStruct_dealloc(MenaiValue self)
{
    MenaiStruct_Object *s = (MenaiStruct_Object *)self;
    menai_xrelease(s->struct_type);
    int n = s->nfields;
    for (int i = 0; i < n; i++) {
        menai_xrelease(s->items[i]);
    }

    free(self);
}

PyTypeObject MenaiStruct_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiStruct",          /* tp_name */
    sizeof(MenaiStruct_Object) - sizeof(MenaiValue),   /* tp_basicsize */
    0,                             /* tp_itemsize */
    (destructor)MenaiStruct_dealloc, /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue
menai_struct_alloc(MenaiValue struct_type, MenaiValue *field_values,
                   Py_ssize_t nfields)
{
    MenaiStruct_Object *self = (MenaiStruct_Object *)malloc(
        sizeof(MenaiStruct_Object) + (size_t)nfields * sizeof(MenaiValue));
    if (!self) {
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = &MenaiStruct_Type;
    self->ob_destructor = MenaiStruct_dealloc;
    self->nfields = (int)nfields;
    menai_retain(struct_type);
    self->struct_type = struct_type;

    for (Py_ssize_t i = 0; i < nfields; i++) {
        menai_retain(field_values[i]);
        self->items[i] = field_values[i];
    }

    return (MenaiValue)self;
}

int
menai_vm_struct_init(void)
{
    if (PyType_Ready(&MenaiStruct_Type) < 0) {
        return -1;
    }

    if (PyType_Ready(&MenaiStructType_Type) < 0) {
        return -1;
    }

    return 0;
}
