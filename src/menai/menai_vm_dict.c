/*
 * menai_vm_dict.c — MenaiDict type implementation.
 *
 * MenaiDict stores an ordered sequence of key-value entries as three parallel
 * C arrays (keys, values, hkeys) plus a Python dict mapping canonical hash
 * keys to integer indices for O(1) lookup.  Canonical hash keys are computed
 * once at construction time and reused for all subsequent operations.
 *
 * The primary construction path for VM operations is menai_dict_from_arrays_steal,
 * which takes already-prepared parallel arrays and builds the lookup dict in a
 * single pass.  The Python-callable MenaiDict() constructor (MenaiDict_new) is
 * used only for the empty-dict singleton and for slow-path construction from
 * Python-level sequences.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_dict.h"
#include "menai_vm_value.h"

/*
 * _dict_alloc_arrays — allocate three parallel PyObject* arrays of size n.
 *
 * On success, *out_keys, *out_values, and *out_hkeys each point to a freshly
 * allocated array of n pointers (uninitialised).  Returns 0 on success, -1
 * on MemoryError with all three pointers set to NULL.
 */
static int
_dict_alloc_arrays(Py_ssize_t n, PyObject ***out_keys,
                   PyObject ***out_values, PyObject ***out_hkeys)
{
    *out_keys = NULL;
    *out_values = NULL;
    *out_hkeys = NULL;
    if (n == 0) return 0;

    *out_keys = (PyObject **)PyMem_Malloc(n * sizeof(PyObject *));
    if (!*out_keys) goto oom;

    *out_values = (PyObject **)PyMem_Malloc(n * sizeof(PyObject *));
    if (!*out_values) goto oom;

    *out_hkeys = (PyObject **)PyMem_Malloc(n * sizeof(PyObject *));
    if (!*out_hkeys) goto oom;

    return 0;

oom:
    PyMem_Free(*out_keys);
    PyMem_Free(*out_values);
    *out_keys = NULL;
    *out_values = NULL;
    *out_hkeys = NULL;
    PyErr_NoMemory();
    return -1;
}

/*
 * _dict_free_arrays — release n owned references in each array and free them.
 * Any NULL array pointer is safely ignored.
 */
static void
_dict_free_arrays(PyObject **keys, PyObject **values, PyObject **hkeys,
                  Py_ssize_t n)
{
    if (keys) {
        for (Py_ssize_t i = 0; i < n; i++) Py_XDECREF(keys[i]);
        PyMem_Free(keys);
    }
    if (values) {
        for (Py_ssize_t i = 0; i < n; i++) Py_XDECREF(values[i]);
        PyMem_Free(values);
    }
    if (hkeys) {
        for (Py_ssize_t i = 0; i < n; i++) Py_XDECREF(hkeys[i]);
        PyMem_Free(hkeys);
    }
}

/*
 * _dict_build_lookup — build a Python dict mapping hkeys[i] -> PyLong(i).
 *
 * Returns a new reference to the lookup dict, or NULL on error.
 * Does not consume or modify the arrays.
 */
static PyObject *
_dict_build_lookup(PyObject **hkeys, Py_ssize_t n)
{
    PyObject *lookup = PyDict_New();
    if (!lookup) return NULL;

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *idx = PyLong_FromSsize_t(i);
        if (!idx) {
            Py_DECREF(lookup);
            return NULL;
        }
        int ok = PyDict_SetItem(lookup, hkeys[i], idx);
        Py_DECREF(idx);
        if (ok < 0) {
            Py_DECREF(lookup);
            return NULL;
        }
    }
    return lookup;
}

/*
 * menai_dict_from_arrays_steal — the primary fast constructor.
 *
 * Takes ownership of keys, values, and hkeys arrays (and their contents).
 * Builds the lookup dict from hkeys then wraps everything in a MenaiDict.
 * Frees all arrays on failure.
 */
PyObject *
menai_dict_from_arrays_steal(PyObject **keys, PyObject **values,
                              PyObject **hkeys, Py_ssize_t n)
{
    PyObject *lookup = _dict_build_lookup(hkeys, n);
    if (!lookup) {
        _dict_free_arrays(keys, values, hkeys, n);
        return NULL;
    }

    MenaiDict_Object *obj =
        (MenaiDict_Object *)MenaiDict_Type.tp_alloc(&MenaiDict_Type, 0);
    if (!obj) {
        Py_DECREF(lookup);
        _dict_free_arrays(keys, values, hkeys, n);
        return NULL;
    }

    obj->keys = keys;
    obj->values = values;
    obj->hkeys = hkeys;
    obj->lookup = lookup;
    obj->length = n;
    return (PyObject *)obj;
}

/*
 * MenaiDict_new — Python-callable constructor: MenaiDict(pairs=None).
 *
 * pairs may be any sequence of (key, value) 2-tuples.  Used for the
 * empty-dict singleton and for the Python-level MenaiDict() call path.
 * VM operations use menai_dict_from_arrays_steal instead.
 */
static PyObject *
MenaiDict_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    (void)type;
    PyObject *pairs_arg = NULL;
    static char *kwlist[] = {"pairs", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "|O", kwlist, &pairs_arg))
        return NULL;

    if (pairs_arg == NULL || PySequence_Size(pairs_arg) == 0) {
        return menai_dict_new_empty();
    }

    PyObject *src = PySequence_Tuple(pairs_arg);
    if (!src) return NULL;

    Py_ssize_t n = PyTuple_GET_SIZE(src);
    PyObject **keys = NULL, **values = NULL, **hkeys = NULL;
    if (_dict_alloc_arrays(n, &keys, &values, &hkeys) < 0) {
        Py_DECREF(src);
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *pair = PyTuple_GET_ITEM(src, i);
        PyObject *k = PyTuple_GET_ITEM(pair, 0);
        PyObject *v = PyTuple_GET_ITEM(pair, 1);
        PyObject *hk = menai_hashable_key(k);
        if (!hk) {
            /* Free the i entries already initialised. */
            for (Py_ssize_t j = 0; j < i; j++) {
                Py_DECREF(keys[j]);
                Py_DECREF(values[j]);
                Py_DECREF(hkeys[j]);
            }
            PyMem_Free(keys);
            PyMem_Free(values);
            PyMem_Free(hkeys);
            Py_DECREF(src);
            return NULL;
        }
        Py_INCREF(k);
        Py_INCREF(v);
        keys[i] = k;
        values[i] = v;
        hkeys[i] = hk;
    }
    Py_DECREF(src);

    return menai_dict_from_arrays_steal(keys, values, hkeys, n);
}

static void
MenaiDict_dealloc(PyObject *self)
{
    MenaiDict_Object *d = (MenaiDict_Object *)self;
    Py_ssize_t n = d->length;
    _dict_free_arrays(d->keys, d->values, d->hkeys, n);
    Py_XDECREF(d->lookup);
    d->keys = NULL;
    d->values = NULL;
    d->hkeys = NULL;
    d->lookup = NULL;
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
MenaiDict_type_name(PyObject *self, PyObject *args)
{
    (void)self;
    (void)args;
    return PyUnicode_FromString("dict");
}

static PyObject *
MenaiDict_describe(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiDict_Object *d = (MenaiDict_Object *)self;
    Py_ssize_t n = d->length;
    if (n == 0) return PyUnicode_FromString("{}");

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *kd = PyObject_CallMethod(d->keys[i], "describe", NULL);
        PyObject *vd = kd
            ? PyObject_CallMethod(d->values[i], "describe", NULL)
            : NULL;
        if (!vd) {
            Py_XDECREF(kd);
            Py_DECREF(parts);
            return NULL;
        }
        PyObject *entry = PyUnicode_FromFormat("(%U %U)", kd, vd);
        Py_DECREF(kd);
        Py_DECREF(vd);
        if (!entry) {
            Py_DECREF(parts);
            return NULL;
        }
        PyList_SET_ITEM(parts, i, entry);
    }

    PyObject *sep = PyUnicode_FromString(" ");
    PyObject *joined = PyUnicode_Join(sep, parts);
    Py_DECREF(sep);
    Py_DECREF(parts);
    if (!joined) return NULL;

    PyObject *result = PyUnicode_FromFormat("{%U}", joined);
    Py_DECREF(joined);
    return result;
}

static PyObject *
MenaiDict_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiDict_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }

    MenaiDict_Object *a = (MenaiDict_Object *)self;
    MenaiDict_Object *b = (MenaiDict_Object *)other;

    if (op == Py_EQ) {
        if (a->length != b->length) Py_RETURN_FALSE;
        for (Py_ssize_t i = 0; i < a->length; i++) {
            int keq = PyObject_RichCompareBool(a->hkeys[i], b->hkeys[i], Py_EQ);
            if (keq <= 0) {
                if (keq < 0) return NULL;
                Py_RETURN_FALSE;
            }
            int veq = PyObject_RichCompareBool(a->values[i], b->values[i], Py_EQ);
            if (veq <= 0) {
                if (veq < 0) return NULL;
                Py_RETURN_FALSE;
            }
        }
        Py_RETURN_TRUE;
    }

    if (op == Py_NE) {
        if (a->length != b->length) Py_RETURN_TRUE;
        for (Py_ssize_t i = 0; i < a->length; i++) {
            int keq = PyObject_RichCompareBool(a->hkeys[i], b->hkeys[i], Py_EQ);
            if (keq < 0) return NULL;
            if (!keq) Py_RETURN_TRUE;
            int veq = PyObject_RichCompareBool(a->values[i], b->values[i], Py_EQ);
            if (veq < 0) return NULL;
            if (!veq) Py_RETURN_TRUE;
        }
        Py_RETURN_FALSE;
    }

    Py_RETURN_NOTIMPLEMENTED;
}

static Py_hash_t
MenaiDict_hash(PyObject *self)
{
    MenaiDict_Object *d = (MenaiDict_Object *)self;
    Py_ssize_t n = d->length;

    /*
     * Build a temporary tuple of (hkey, value) pairs and hash that.
     * Dicts are rarely used as keys so this path is uncommon.
     */
    PyObject *tup = PyTuple_New(n);
    if (!tup) return -1;

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *pair = PyTuple_Pack(2, d->hkeys[i], d->values[i]);
        if (!pair) {
            Py_DECREF(tup);
            return -1;
        }
        PyTuple_SET_ITEM(tup, i, pair);
    }

    Py_hash_t h = PyObject_Hash(tup);
    Py_DECREF(tup);
    return h;
}

static PyMethodDef MenaiDict_methods[] = {
    {"type_name", MenaiDict_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiDict_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiDict_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_vm_value.MenaiDict",
    .tp_basicsize = sizeof(MenaiDict_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT,
    .tp_new       = MenaiDict_new,
    .tp_dealloc   = MenaiDict_dealloc,
    .tp_methods   = MenaiDict_methods,
    .tp_richcompare = MenaiDict_richcompare,
    .tp_hash      = MenaiDict_hash,
};

PyObject *
menai_dict_new_empty(void)
{
    MenaiDict_Object *obj =
        (MenaiDict_Object *)MenaiDict_Type.tp_alloc(&MenaiDict_Type, 0);
    if (!obj) return NULL;

    PyObject *lookup = PyDict_New();
    if (!lookup) {
        Py_DECREF(obj);
        return NULL;
    }

    obj->keys = NULL;
    obj->values = NULL;
    obj->hkeys = NULL;
    obj->lookup = lookup;
    obj->length = 0;
    return (PyObject *)obj;
}

int
menai_vm_dict_init(void)
{
    return PyType_Ready(&MenaiDict_Type);
}
