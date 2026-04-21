/*
 * menai_vm_dict.c — MenaiDict type implementation.
 *
 * MenaiDict stores an ordered sequence of key-value entries as three parallel
 * C arrays (keys, values, hashes) plus a pure-C MenaiHashTable for O(1) index
 * lookup.  Hash values are computed once at construction time via
 * menai_value_hash() and stored in hashes[], so no Python objects are
 * allocated during dict operations.
 *
 * Primary construction path for VM operations: menai_dict_from_arrays_steal.
 * Python-callable constructor MenaiDict_new is used only for the empty-dict
 * singleton and for slow-path construction from Python-level sequences.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_dict.h"
#include "menai_vm_memory.h"
#include "menai_vm_hashtable.h"
#include "menai_vm_value.h"

/*
 * _dict_alloc_arrays — allocate keys, values, and hashes arrays of size n.
 * Returns 0 on success, -1 on MemoryError with all pointers set to NULL.
 */
static int
_dict_alloc_arrays(Py_ssize_t n, PyObject ***out_keys,
                   PyObject ***out_values, Py_hash_t **out_hashes)
{
    *out_keys = NULL;
    *out_values = NULL;
    *out_hashes = NULL;
    if (n == 0) return 0;

    *out_keys = (PyObject **)PyMem_Malloc(n * sizeof(PyObject *));
    if (!*out_keys) goto oom;

    *out_values = (PyObject **)PyMem_Malloc(n * sizeof(PyObject *));
    if (!*out_values) goto oom;

    *out_hashes = (Py_hash_t *)PyMem_Malloc(n * sizeof(Py_hash_t));
    if (!*out_hashes) goto oom;

    return 0;

oom:
    PyMem_Free(*out_keys);
    PyMem_Free(*out_values);
    *out_keys = NULL;
    *out_values = NULL;
    *out_hashes = NULL;
    PyErr_NoMemory();
    return -1;
}

/*
 * _dict_free_arrays — release n owned references in keys and values, then
 * free all three arrays.  NULL pointers are safely ignored.
 */
static void
_dict_free_arrays(PyObject **keys, PyObject **values, Py_hash_t *hashes,
                  Py_ssize_t n)
{
    if (keys) {
        for (Py_ssize_t i = 0; i < n; i++) menai_xrelease(keys[i]);
        PyMem_Free(keys);
    }
    if (values) {
        for (Py_ssize_t i = 0; i < n; i++) menai_xrelease(values[i]);
        PyMem_Free(values);
    }
    PyMem_Free(hashes);
}

/*
 * menai_dict_from_arrays_steal — primary fast constructor.
 *
 * Takes ownership of keys, values, and hashes arrays (and key/value
 * references).  Builds the hash table from hashes then wraps everything in a
 * MenaiDict.  Frees all arrays on failure.
 */
PyObject *
menai_dict_from_arrays_steal(PyObject **keys, PyObject **values,
                              Py_hash_t *hashes, Py_ssize_t n)
{
    MenaiDict_Object *obj =
        (MenaiDict_Object *)MenaiDict_Type.tp_alloc(&MenaiDict_Type, 0);
    if (!obj) {
        _dict_free_arrays(keys, values, hashes, n);
        return NULL;
    }

    if (menai_ht_build(&obj->ht, keys, hashes, n) < 0) {
        _dict_free_arrays(keys, values, hashes, n);
        Py_DECREF(obj);
        return NULL;
    }

    obj->keys = keys;
    obj->values = values;
    obj->hashes = hashes;
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

    if (pairs_arg == NULL || PySequence_Size(pairs_arg) == 0)
        return menai_dict_new_empty();

    PyObject *src = PySequence_Tuple(pairs_arg);
    if (!src) return NULL;

    Py_ssize_t n = PyTuple_GET_SIZE(src);
    PyObject **keys = NULL, **values = NULL;
    Py_hash_t *hashes = NULL;
    if (_dict_alloc_arrays(n, &keys, &values, &hashes) < 0) {
        Py_DECREF(src);
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *pair = PyTuple_GET_ITEM(src, i);
        PyObject *k = PyTuple_GET_ITEM(pair, 0);
        PyObject *v = PyTuple_GET_ITEM(pair, 1);
        Py_hash_t h = menai_value_hash(k);
        if (h == -1) {
            for (Py_ssize_t j = 0; j < i; j++) {
                menai_release(keys[j]);
                menai_release(values[j]);
            }
            PyMem_Free(keys);
            PyMem_Free(values);
            PyMem_Free(hashes);
            Py_DECREF(src);
            return NULL;
        }
        menai_retain(k);
        menai_retain(v);
        keys[i] = k;
        values[i] = v;
        hashes[i] = h;
    }
    Py_DECREF(src);

    return menai_dict_from_arrays_steal(keys, values, hashes, n);
}

static void
MenaiDict_dealloc(PyObject *self)
{
    MenaiDict_Object *d = (MenaiDict_Object *)self;
    _dict_free_arrays(d->keys, d->values, d->hashes, d->length);
    menai_ht_free(&d->ht);
    d->keys = NULL;
    d->values = NULL;
    d->hashes = NULL;
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
MenaiDict_type_name(PyObject *self, PyObject *args)
{
    (void)self;
    (void)args;
    return PyUnicode_FromString("dict");
}

PyObject *
MenaiDict_describe(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiDict_Object *d = (MenaiDict_Object *)self;
    Py_ssize_t n = d->length;
    if (n == 0) return PyUnicode_FromString("{}");

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *kd = menai_value_describe(d->keys[i]);
        PyObject *vd = kd
            ? menai_value_describe(d->values[i])
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
            if (a->hashes[i] != b->hashes[i]) Py_RETURN_FALSE;
            int keq = menai_value_equal(a->keys[i], b->keys[i]);
            if (!keq) Py_RETURN_FALSE;
            int veq = menai_value_equal(a->values[i], b->values[i]);
            if (!veq) Py_RETURN_FALSE;
        }
        Py_RETURN_TRUE;
    }

    if (op == Py_NE) {
        if (a->length != b->length) Py_RETURN_TRUE;
        for (Py_ssize_t i = 0; i < a->length; i++) {
            if (a->hashes[i] != b->hashes[i]) Py_RETURN_TRUE;
            int keq = menai_value_equal(a->keys[i], b->keys[i]);
            if (!keq) Py_RETURN_TRUE;
            int veq = menai_value_equal(a->values[i], b->values[i]);
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
    Py_uhash_t acc = 0x64696374UL;  /* "dict" */
    for (Py_ssize_t i = 0; i < n; i++) {
        /* Combine key hash and value hash for each entry */
        Py_hash_t vh = menai_value_hash(d->values[i]);
        if (vh == -1) return -1;
        acc = acc * 1000003UL ^ (Py_uhash_t)d->hashes[i];
        acc = acc * 1000003UL ^ (Py_uhash_t)vh;
    }
    acc ^= (Py_uhash_t)n;
    return (Py_hash_t)(acc == (Py_uhash_t)-1 ? -2 : acc);
}

/*
 * to_python — convert to a Python dict.
 * String and symbol keys are converted to Python str; all other key types
 * use str(key.to_python()).  Values are recursively converted.
 */
PyObject *
MenaiDict_to_python(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiDict_Object *d = (MenaiDict_Object *)self;
    Py_ssize_t n = d->length;
    PyObject *result = PyDict_New();
    if (!result) return NULL;
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *k = d->keys[i];
        PyObject *py_key;
        if (Py_TYPE(k) == &MenaiString_Type) {
            py_key = menai_string_to_pyunicode(k);
        } else if (Py_TYPE(k) == &MenaiSymbol_Type) {
            py_key = ((MenaiSymbol_Object *)k)->name;
            Py_INCREF(py_key);
        } else {
            PyObject *kv = menai_value_to_python(k);
            if (!kv) { Py_DECREF(result); return NULL; }
            py_key = PyObject_Str(kv);
            Py_DECREF(kv);
        }
        if (!py_key) { Py_DECREF(result); return NULL; }
        PyObject *py_val = menai_value_to_python(d->values[i]);
        if (!py_val) { Py_DECREF(py_key); Py_DECREF(result); return NULL; }
        int ok = PyDict_SetItem(result, py_key, py_val);
        Py_DECREF(py_key);
        Py_DECREF(py_val);
        if (ok < 0) { Py_DECREF(result); return NULL; }
    }
    return result;
}

/*
 * pairs getter — returns a tuple of (key, value) 2-tuples, matching the
 * slow-world MenaiDict.pairs interface consumed by _load_prelude.
 */
static PyObject *
MenaiDict_get_pairs(PyObject *self, void *closure)
{
    (void)closure;
    MenaiDict_Object *d = (MenaiDict_Object *)self;
    Py_ssize_t n = d->length;
    PyObject *tup = PyTuple_New(n);
    if (!tup) return NULL;
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *pair = PyTuple_Pack(2, d->keys[i], d->values[i]);
        if (!pair) { Py_DECREF(tup); return NULL; }
        PyTuple_SET_ITEM(tup, i, pair);
    }
    return tup;
}

static PyGetSetDef MenaiDict_getset[] = {
    {"pairs", MenaiDict_get_pairs, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiDict_methods[] = {
    {"type_name", MenaiDict_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiDict_describe,  METH_NOARGS, NULL},
    {"to_python", MenaiDict_to_python, METH_NOARGS, NULL},
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
    .tp_getset    = MenaiDict_getset,
    .tp_richcompare = MenaiDict_richcompare,
    .tp_hash      = MenaiDict_hash,
};

PyObject *
menai_dict_new_empty(void)
{
    MenaiDict_Object *obj =
        (MenaiDict_Object *)MenaiDict_Type.tp_alloc(&MenaiDict_Type, 0);
    if (!obj) return NULL;

    obj->keys = NULL;
    obj->values = NULL;
    obj->hashes = NULL;
    obj->ht.slots = NULL;
    obj->ht.slot_count = 0;
    obj->ht.used = 0;
    obj->length = 0;
    return (PyObject *)obj;
}

int
menai_vm_dict_init(void)
{
    return PyType_Ready(&MenaiDict_Type);
}
