/*
 * menai_vm_struct.c — MenaiStructType and MenaiStruct type implementations.
 *
 * MenaiStructType: field lookup uses an inline C array of (interned name,
 * index) pairs rather than a Python dict.
 *
 * MenaiStruct: field values are stored in an inline C array (ob_size ==
 * nfields), eliminating the Python tuple previously heap-allocated on every
 * struct construction.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_struct.h"
#include "menai_vm_symbol.h"
#include "menai_vm_hashtable.h"

/*
 * _build_struct_type — shared constructor body for MenaiStructType.
 * name must be a PyUnicode.  tag is a C int.  fn_tup must be a tuple of
 * PyUnicode strings (already owned by the caller; this function steals it).
 * Returns a new reference, or NULL on error.
 */
static PyObject *
_build_struct_type(PyObject *name, int tag, PyObject *fn_tup)
{
    Py_ssize_t n = PyTuple_GET_SIZE(fn_tup);

    /*
     * Allocate MenaiStructType with n inline MenaiFieldEntry slots.
     * tp_basicsize covers everything up to but not including fields[];
     * tp_itemsize is sizeof(MenaiFieldEntry), so tp_alloc(type, n) gives
     * exactly the right amount.
     */
    MenaiStructType_Object *self = (MenaiStructType_Object *)MenaiStructType_Type.tp_alloc(&MenaiStructType_Type, n);
    if (!self) {
        Py_DECREF(fn_tup);
        return NULL;
    }

    Py_INCREF(name);
    self->name = name;
    self->tag = tag;
    self->field_names = fn_tup;  /* steal */
    self->nfields = (int)n;

    /* Populate the inline field-index table with interned name pointers. */
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *fname = PyTuple_GET_ITEM(fn_tup, i);
        Py_INCREF(fname);
        PyUnicode_InternInPlace(&fname);
        self->fields[i].name = fname;   /* owned */
        self->fields[i].index = (int)i;
    }

    return (PyObject *)self;
}

static PyObject *
MenaiStructType_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    (void)type;
    PyObject *name = NULL, *field_names = NULL;
    int tag = 0;
    static char *kwlist[] = {"name", "tag", "field_names", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "UiO", kwlist, &name, &tag, &field_names)) return NULL;

    PyObject *fn_tup = PySequence_Tuple(field_names);
    if (!fn_tup) return NULL;

    return _build_struct_type(name, tag, fn_tup);
}

static void
MenaiStructType_dealloc(PyObject *self)
{
    MenaiStructType_Object *s = (MenaiStructType_Object *)self;
    Py_XDECREF(s->name);
    Py_XDECREF(s->field_names);
    int n = s->nfields;
    for (int i = 0; i < n; i++) Py_XDECREF(s->fields[i].name);

    Py_TYPE(self)->tp_free(self);
}

static PyObject *
MenaiStructType_type_name(PyObject *self, PyObject *args)
{
    (void)self;
    (void)args;
    return PyUnicode_FromString("struct-type");
}

static PyObject *
MenaiStructType_describe(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiStructType_Object *s = (MenaiStructType_Object *)self;
    PyObject *sep = PyUnicode_FromString(" ");
    PyObject *fields_str = PyUnicode_Join(sep, s->field_names);
    Py_DECREF(sep);
    if (!fields_str) return NULL;

    PyObject *result = PyUnicode_FromFormat("<struct-type %U (%U)>", s->name, fields_str);
    Py_DECREF(fields_str);
    return result;
}

static PyObject *
MenaiStructType_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiStructType_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    int a = ((MenaiStructType_Object *)self)->tag;
    int b = ((MenaiStructType_Object *)other)->tag;
    switch (op) {
        case Py_EQ: return PyBool_FromLong(a == b);
        case Py_NE: return PyBool_FromLong(a != b);
        default: Py_RETURN_NOTIMPLEMENTED;
    }
}

static Py_hash_t
MenaiStructType_hash(PyObject *self)
{
    return (Py_hash_t)((MenaiStructType_Object *)self)->tag;
}

static PyObject *
MenaiStructType_get_name(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *n = ((MenaiStructType_Object *)self)->name;
    Py_INCREF(n);
    return n;
}

static PyObject *
MenaiStructType_get_tag(PyObject *self, void *closure)
{
    (void)closure;
    return PyLong_FromLong(((MenaiStructType_Object *)self)->tag);
}

static PyObject *
MenaiStructType_get_field_names(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *fn = ((MenaiStructType_Object *)self)->field_names;
    Py_INCREF(fn);
    return fn;
}

static PyGetSetDef MenaiStructType_getset[] = {
    {"name", MenaiStructType_get_name, NULL, NULL, NULL},
    {"tag", MenaiStructType_get_tag, NULL, NULL, NULL},
    {"field_names", MenaiStructType_get_field_names, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiStructType_methods[] = {
    {"type_name", MenaiStructType_type_name, METH_NOARGS, NULL},
    {"describe", MenaiStructType_describe, METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiStructType_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "menai.menai_vm_value.MenaiStructType",
    .tp_basicsize = sizeof(MenaiStructType_Object),
    .tp_itemsize = sizeof(MenaiFieldEntry),
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = MenaiStructType_new,
    .tp_dealloc = MenaiStructType_dealloc,
    .tp_methods = MenaiStructType_methods,
    .tp_getset = MenaiStructType_getset,
    .tp_richcompare = MenaiStructType_richcompare,
    .tp_hash = MenaiStructType_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiStruct
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiStruct_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *struct_type = NULL, *fields = NULL;
    static char *kwlist[] = {"struct_type", "fields", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "OO", kwlist, &struct_type, &fields)) return NULL;

    Py_ssize_t n = PySequence_Size(fields);
    if (n < 0) return NULL;

    MenaiStruct_Object *self = (MenaiStruct_Object *)type->tp_alloc(type, n);
    if (!self) return NULL;

    Py_INCREF(struct_type);
    self->struct_type = struct_type;

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *fv = PySequence_GetItem(fields, i);
        if (!fv) {
            for (Py_ssize_t j = 0; j < i; j++) Py_DECREF(self->items[j]);
            self->struct_type = NULL;
            Py_DECREF(struct_type);
            Py_TYPE(self)->tp_free(self);
            return NULL;
        }

        self->items[i] = fv;  /* owned */
    }

    return (PyObject *)self;
}

static void
MenaiStruct_dealloc(PyObject *self)
{
    MenaiStruct_Object *s = (MenaiStruct_Object *)self;
    Py_XDECREF(s->struct_type);
    Py_ssize_t n = Py_SIZE(s);
    for (Py_ssize_t i = 0; i < n; i++) Py_XDECREF(s->items[i]);

    Py_TYPE(self)->tp_free(self);
}

static PyObject *
MenaiStruct_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("struct");
}

static PyObject *
MenaiStruct_describe(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiStruct_Object *s = (MenaiStruct_Object *)self;
    MenaiStructType_Object *st = (MenaiStructType_Object *)s->struct_type;
    Py_ssize_t n = Py_SIZE(s);

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *desc = PyObject_CallMethod(s->items[i], "describe", NULL);
        if (!desc) {
            Py_DECREF(parts);
            return NULL;
        }
        PyList_SET_ITEM(parts, i, desc);
    }

    PyObject *sep = PyUnicode_FromString(" ");
    PyObject *joined = PyUnicode_Join(sep, parts);
    Py_DECREF(sep);
    Py_DECREF(parts);
    if (!joined) return NULL;

    PyObject *result = (n == 0)
        ? PyUnicode_FromFormat("(%U)", st->name)
        : PyUnicode_FromFormat("(%U %U)", st->name, joined);
    Py_DECREF(joined);
    return result;
}

static PyObject *
MenaiStruct_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiStruct_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    MenaiStruct_Object *a = (MenaiStruct_Object *)self;
    MenaiStruct_Object *b = (MenaiStruct_Object *)other;
    int tag_a = ((MenaiStructType_Object *)a->struct_type)->tag;
    int tag_b = ((MenaiStructType_Object *)b->struct_type)->tag;

    if (op == Py_EQ) {
        if (tag_a != tag_b) Py_RETURN_FALSE;
        Py_ssize_t n = Py_SIZE(a);
        for (Py_ssize_t i = 0; i < n; i++) {
            int eq = menai_value_equal(a->items[i], b->items[i]);
            if (eq < 0) return NULL;
            if (!eq) Py_RETURN_FALSE;
        }
        Py_RETURN_TRUE;
    }

    if (op == Py_NE) {
        if (tag_a != tag_b) Py_RETURN_TRUE;
        Py_ssize_t n = Py_SIZE(a);
        for (Py_ssize_t i = 0; i < n; i++) {
            int eq = menai_value_equal(a->items[i], b->items[i]);
            if (eq < 0) return NULL;
            if (!eq) Py_RETURN_TRUE;
        }

        Py_RETURN_FALSE;
    }
    Py_RETURN_NOTIMPLEMENTED;
}

static Py_hash_t
MenaiStruct_hash(PyObject *self)
{
    MenaiStruct_Object *s = (MenaiStruct_Object *)self;
    int tag = ((MenaiStructType_Object *)s->struct_type)->tag;
    Py_ssize_t n = Py_SIZE(s);

    /*
     * Combine tag and per-field hashes using the same djb2-style mixing
     * Python uses internally for tuple hashing.
     */
    Py_uhash_t acc = 0x345678UL ^ (Py_uhash_t)tag;
    for (Py_ssize_t i = 0; i < n; i++) {
        Py_hash_t fh = PyObject_Hash(s->items[i]);
        if (fh == -1) return -1;

        acc = acc * 1000003UL ^ (Py_uhash_t)fh;
    }

    acc ^= (Py_uhash_t)n;
    return (Py_hash_t)(acc == (Py_uhash_t)-1 ? -2 : acc);
}

static PyObject *
MenaiStruct_get_struct_type(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *st = ((MenaiStruct_Object *)self)->struct_type;
    Py_INCREF(st);
    return st;
}

/*
 * fields getter — builds a Python tuple on demand from the inline array.
 * Only used by the Python-facing API (to_slow, tests).
 */
static PyObject *
MenaiStruct_get_fields(PyObject *self, void *closure)
{
    (void)closure;
    MenaiStruct_Object *s = (MenaiStruct_Object *)self;
    Py_ssize_t n = Py_SIZE(s);
    PyObject *tup = PyTuple_New(n);
    if (!tup) return NULL;

    for (Py_ssize_t i = 0; i < n; i++) {
        Py_INCREF(s->items[i]);
        PyTuple_SET_ITEM(tup, i, s->items[i]);
    }

    return tup;
}

static PyGetSetDef MenaiStruct_getset[] = {
    {"struct_type", MenaiStruct_get_struct_type, NULL, NULL, NULL},
    {"fields", MenaiStruct_get_fields, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiStruct_methods[] = {
    {"type_name", MenaiStruct_type_name, METH_NOARGS, NULL},
    {"describe", MenaiStruct_describe, METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiStruct_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "menai.menai_vm_value.MenaiStruct",
    .tp_basicsize = sizeof(MenaiStruct_Object) - sizeof(PyObject *),
    .tp_itemsize = sizeof(PyObject *),
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = MenaiStruct_new,
    .tp_dealloc = MenaiStruct_dealloc,
    .tp_methods = MenaiStruct_methods,
    .tp_getset = MenaiStruct_getset,
    .tp_richcompare = MenaiStruct_richcompare,
    .tp_hash = MenaiStruct_hash,
};

PyObject *
menai_struct_alloc(PyObject *struct_type, PyObject **field_values, Py_ssize_t nfields)
{
    MenaiStruct_Object *self = (MenaiStruct_Object *)MenaiStruct_Type.tp_alloc(&MenaiStruct_Type, nfields);
    if (!self) return NULL;

    Py_INCREF(struct_type);
    self->struct_type = struct_type;
    for (Py_ssize_t i = 0; i < nfields; i++) {
        Py_INCREF(field_values[i]);
        self->items[i] = field_values[i];
    }

    return (PyObject *)self;
}

PyObject *
menai_struct_type_new_from_args(PyObject *args)
{
    return MenaiStructType_new(&MenaiStructType_Type, args, NULL);
}

int
menai_vm_struct_init(void)
{
    if (PyType_Ready(&MenaiStructType_Type) < 0) return -1;
    if (PyType_Ready(&MenaiStruct_Type) < 0) return -1;

    return 0;
}
