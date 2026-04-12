/*
 * menai_vm_struct.c — MenaiStructType and MenaiStruct type implementations.
 *
 * MenaiStructType describes a struct schema (name, tag, field names).
 * MenaiStruct is an instance holding a tuple of field values.
 *
 * Also provides:
 *   menai_struct_alloc()           — direct C constructor used by the VM
 *   menai_struct_type_new_from_args() — used by menai_convert_value()
 *   menai_struct_new_from_fast()   — used by menai_convert_value()
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_struct.h"

/* ---------------------------------------------------------------------------
 * MenaiStructType
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiStructType_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *name = NULL, *field_names = NULL;
    int tag = 0;
    static char *kwlist[] = {"name", "tag", "field_names", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "UiO", kwlist,
                                     &name, &tag, &field_names))
        return NULL;

    PyObject *fn_tup = PySequence_Tuple(field_names);
    if (!fn_tup) return NULL;

    /* Build _field_index dict */
    PyObject *fi = PyDict_New();
    if (!fi) {
        Py_DECREF(fn_tup);
        return NULL;
    }
    Py_ssize_t n = PyTuple_GET_SIZE(fn_tup);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *idx = PyLong_FromSsize_t(i);
        if (!idx || PyDict_SetItem(fi, PyTuple_GET_ITEM(fn_tup, i), idx) < 0) {
            Py_XDECREF(idx);
            Py_DECREF(fi);
            Py_DECREF(fn_tup);
            return NULL;
        }
        Py_DECREF(idx);
    }

    MenaiStructType_Object *self = (MenaiStructType_Object *)type->tp_alloc(type, 0);
    if (self) {
        Py_INCREF(name);
        self->name = name;
        self->tag = tag;
        self->field_names = fn_tup;
        self->_field_index = fi;
    } else {
        Py_DECREF(fn_tup);
        Py_DECREF(fi);
    }
    return (PyObject *)self;
}

static void
MenaiStructType_dealloc(PyObject *self)
{
    MenaiStructType_Object *s = (MenaiStructType_Object *)self;
    Py_XDECREF(s->name);
    Py_XDECREF(s->field_names);
    Py_XDECREF(s->_field_index);
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
MenaiStructType_field_index(PyObject *self, PyObject *name)
{
    PyObject *idx = PyDict_GetItem(((MenaiStructType_Object *)self)->_field_index, name);
    if (!idx) {
        PyErr_SetObject(PyExc_KeyError, name);
        return NULL;
    }
    Py_INCREF(idx);
    return idx;
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
        default:    Py_RETURN_NOTIMPLEMENTED;
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
    {"name",        MenaiStructType_get_name,        NULL, NULL, NULL},
    {"tag",         MenaiStructType_get_tag,          NULL, NULL, NULL},
    {"field_names", MenaiStructType_get_field_names,  NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiStructType_methods[] = {
    {"type_name",   MenaiStructType_type_name,  METH_NOARGS, NULL},
    {"describe",    MenaiStructType_describe,   METH_NOARGS, NULL},
    {"field_index", MenaiStructType_field_index, METH_O,     NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiStructType_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name        = "menai.menai_vm_value.MenaiStructType",
    .tp_basicsize   = sizeof(MenaiStructType_Object),
    .tp_flags       = Py_TPFLAGS_DEFAULT,
    .tp_new         = MenaiStructType_new,
    .tp_dealloc     = MenaiStructType_dealloc,
    .tp_methods     = MenaiStructType_methods,
    .tp_getset      = MenaiStructType_getset,
    .tp_richcompare = MenaiStructType_richcompare,
    .tp_hash        = MenaiStructType_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiStruct
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiStruct_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *struct_type = NULL, *fields = NULL;
    static char *kwlist[] = {"struct_type", "fields", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "OO", kwlist,
                                     &struct_type, &fields))
        return NULL;

    PyObject *fields_tup = PySequence_Tuple(fields);
    if (!fields_tup) return NULL;

    MenaiStruct_Object *self = (MenaiStruct_Object *)type->tp_alloc(type, 0);
    if (self) {
        Py_INCREF(struct_type);
        self->struct_type = struct_type;
        self->fields = fields_tup;
    } else {
        Py_DECREF(fields_tup);
    }
    return (PyObject *)self;
}

static void
MenaiStruct_dealloc(PyObject *self)
{
    Py_XDECREF(((MenaiStruct_Object *)self)->struct_type);
    Py_XDECREF(((MenaiStruct_Object *)self)->fields);
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
MenaiStruct_type_name(PyObject *self, PyObject *args)
{
    (void)self;
    (void)args;
    return PyUnicode_FromString("struct");
}

static PyObject *
MenaiStruct_describe(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiStruct_Object *s = (MenaiStruct_Object *)self;
    MenaiStructType_Object *st = (MenaiStructType_Object *)s->struct_type;
    Py_ssize_t n = PyTuple_GET_SIZE(s->fields);

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *desc = PyObject_CallMethod(PyTuple_GET_ITEM(s->fields, i), "describe", NULL);
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

    PyObject *result;
    if (n == 0)
        result = PyUnicode_FromFormat("(%U)", st->name);
    else
        result = PyUnicode_FromFormat("(%U %U)", st->name, joined);
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
    int tag_eq = (((MenaiStructType_Object *)a->struct_type)->tag ==
                  ((MenaiStructType_Object *)b->struct_type)->tag);
    if (op == Py_EQ) {
        if (!tag_eq) Py_RETURN_FALSE;
        return PyObject_RichCompare(a->fields, b->fields, Py_EQ);
    }
    if (op == Py_NE) {
        if (!tag_eq) Py_RETURN_TRUE;
        return PyObject_RichCompare(a->fields, b->fields, Py_NE);
    }
    Py_RETURN_NOTIMPLEMENTED;
}

static Py_hash_t
MenaiStruct_hash(PyObject *self)
{
    MenaiStruct_Object *s = (MenaiStruct_Object *)self;
    Py_hash_t fh = PyObject_Hash(s->fields);
    if (fh == -1) return -1;
    int tag = ((MenaiStructType_Object *)s->struct_type)->tag;
    PyObject *pair = Py_BuildValue("(iN)", tag, PyLong_FromSsize_t(fh));
    if (!pair) return -1;
    Py_hash_t h = PyObject_Hash(pair);
    Py_DECREF(pair);
    return h;
}

static PyObject *
MenaiStruct_get_struct_type(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *st = ((MenaiStruct_Object *)self)->struct_type;
    Py_INCREF(st);
    return st;
}

static PyObject *
MenaiStruct_get_fields(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *f = ((MenaiStruct_Object *)self)->fields;
    Py_INCREF(f);
    return f;
}

static PyGetSetDef MenaiStruct_getset[] = {
    {"struct_type", MenaiStruct_get_struct_type, NULL, NULL, NULL},
    {"fields",      MenaiStruct_get_fields,      NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiStruct_methods[] = {
    {"type_name", MenaiStruct_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiStruct_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiStruct_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name        = "menai.menai_vm_value.MenaiStruct",
    .tp_basicsize   = sizeof(MenaiStruct_Object),
    .tp_flags       = Py_TPFLAGS_DEFAULT,
    .tp_new         = MenaiStruct_new,
    .tp_dealloc     = MenaiStruct_dealloc,
    .tp_methods     = MenaiStruct_methods,
    .tp_getset      = MenaiStruct_getset,
    .tp_richcompare = MenaiStruct_richcompare,
    .tp_hash        = MenaiStruct_hash,
};

/* ---------------------------------------------------------------------------
 * Public C-level constructors
 * ------------------------------------------------------------------------- */

PyObject *
menai_struct_alloc(PyObject *struct_type, PyObject *fields_tup)
{
    MenaiStruct_Object *self = (MenaiStruct_Object *)MenaiStruct_Type.tp_alloc(&MenaiStruct_Type, 0);
    if (!self) {
        Py_DECREF(fields_tup);
        return NULL;
    }
    Py_INCREF(struct_type);
    self->struct_type = struct_type;
    self->fields = fields_tup;  /* steal */
    return (PyObject *)self;
}

PyObject *
menai_struct_type_new_from_args(PyObject *args)
{
    return MenaiStructType_new(&MenaiStructType_Type, args, NULL);
}

PyObject *
menai_struct_new_from_fast(PyObject *fast_st, PyObject *fast_fields_tup)
{
    MenaiStruct_Object *self = (MenaiStruct_Object *)MenaiStruct_Type.tp_alloc(&MenaiStruct_Type, 0);
    if (!self) {
        Py_DECREF(fast_fields_tup);
        return NULL;
    }
    Py_INCREF(fast_st);
    self->struct_type = fast_st;
    self->fields = fast_fields_tup;  /* steal */
    return (PyObject *)self;
}

int
menai_vm_struct_init(void)
{
    if (PyType_Ready(&MenaiStructType_Type) < 0)
        return -1;
    if (PyType_Ready(&MenaiStruct_Type) < 0)
        return -1;
    return 0;
}
