/*
 * menai_vm_list.c — MenaiList type implementation.
 *
 * MenaiList stores a C array of PyObject* elements.  A two-level free-list
 * cache (one for object structs, one for element arrays bucketed by power-of-2
 * size) reduces allocation pressure in the hot VM loop.
 *
 * Also provides the three C-level constructors used by the VM:
 *   menai_list_from_array        — copy items, INCREF each
 *   menai_list_from_array_steal  — take ownership, no INCREF
 *   menai_list_from_tuple        — copy from tuple, INCREF each, DECREF tuple
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#ifdef _MSC_VER
#include <intrin.h>
#endif

#include "menai_vm_list.h"

/* ---------------------------------------------------------------------------
 * Free-list cache
 * ------------------------------------------------------------------------- */

#define LIST_CACHE_NUM_BUCKETS 8
#define LIST_CACHE_MAX_BUCKET 256
#define LIST_CACHE_MAX_SIZE 128

static Py_ssize_t _list_size_classes[LIST_CACHE_NUM_BUCKETS] = {
    1, 2, 4, 8, 16, 32, 64, 128
};

/* Object free list — singly-linked via the elements pointer overlay */
static MenaiList_Object *_list_obj_free = NULL;

/* Element array cache — power-of-2 size buckets */
static PyObject ***_list_arr_buckets[LIST_CACHE_NUM_BUCKETS];
static int _list_arr_counts[LIST_CACHE_NUM_BUCKETS];

static inline int
_bucket_index(Py_ssize_t n)
{
    if (n <= 1) return 0;

    /* ceil_log2(n): find the position of the highest set bit in (n-1). */
#if defined(_MSC_VER)
    unsigned long idx;
    _BitScanReverse(&idx, (unsigned long)(n - 1));
    int bucket = (int)(idx + 1);
#elif defined(__GNUC__) || defined(__clang__)
    int bucket = (int)(sizeof(unsigned long) * 8) - __builtin_clzl((unsigned long)(n - 1));
#else
    int bucket = 0;
    unsigned long v = (unsigned long)(n - 1);
    while (v >>= 1) bucket++;
    bucket++;
#endif
    return bucket < LIST_CACHE_NUM_BUCKETS ? bucket : LIST_CACHE_NUM_BUCKETS - 1;
}

static MenaiList_Object *
_menai_list_cache_alloc_obj(void)
{
    if (_list_obj_free) {
        MenaiList_Object *obj = _list_obj_free;
        _list_obj_free = (MenaiList_Object *)obj->elements;
        obj->elements = NULL;
        obj->length = 0;
        /* Restore refcount to 1, matching what tp_alloc produces. */
        Py_SET_REFCNT((PyObject *)obj, 1);
        return obj;
    }

    return (MenaiList_Object *)MenaiList_Type.tp_alloc(&MenaiList_Type, 0);
}

static void
_menai_list_cache_free_obj(MenaiList_Object *obj)
{
    obj->elements = (PyObject **)_list_obj_free;
    obj->length = 0;
    _list_obj_free = obj;
}

static PyObject **
_menai_list_cache_alloc_arr(Py_ssize_t n)
{
    if (n > 0 && n <= LIST_CACHE_MAX_SIZE) {
        int bucket = _bucket_index(n);
        if (_list_arr_counts[bucket] > 0) {
            return _list_arr_buckets[bucket][--_list_arr_counts[bucket]];
        }

        /* No cached entry — allocate at the bucket's full size class so
         * it can be safely recycled into this bucket later. */
        n = _list_size_classes[bucket];
    }

    return (PyObject **)PyMem_Malloc(n * sizeof(PyObject *));
}

static void
_menai_list_cache_free_arr(PyObject **arr, Py_ssize_t n)
{
    for (Py_ssize_t i = 0; i < n; i++) Py_DECREF(arr[i]);
    if (arr && n > 0 && n <= LIST_CACHE_MAX_SIZE) {
        int bucket = _bucket_index(n);
        if (_list_arr_counts[bucket] < LIST_CACHE_MAX_BUCKET) {
            if (_list_arr_counts[bucket] == 0) {
                _list_arr_buckets[bucket] = (PyObject ***)PyMem_Malloc(LIST_CACHE_MAX_BUCKET * sizeof(PyObject **));
                if (!_list_arr_buckets[bucket]) return;
            }

            _list_arr_buckets[bucket][_list_arr_counts[bucket]++] = arr;
            return;
        }
    }
    PyMem_Free(arr);
}

static void
_menai_list_cache_clear(void)
{
    /* Free every object on the free list.  Each was allocated via tp_alloc
     * (_PyObject_GC_New) and must be released with tp_free. */
    MenaiList_Object *obj = _list_obj_free;
    while (obj) {
        MenaiList_Object *next = (MenaiList_Object *)obj->elements;
        MenaiList_Type.tp_free((PyObject *)obj);
        obj = next;
    }

    _list_obj_free = NULL;
    for (int i = 0; i < LIST_CACHE_NUM_BUCKETS; i++) {
        for (int j = 0; j < _list_arr_counts[i]; j++)
            PyMem_Free(_list_arr_buckets[i][j]);

        if (_list_arr_buckets[i]) {
            PyMem_Free(_list_arr_buckets[i]);
            _list_arr_buckets[i] = NULL;
        }

        _list_arr_counts[i] = 0;
    }
}

/* ---------------------------------------------------------------------------
 * Type implementation
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiList_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *elements = NULL;
    static char *kwlist[] = {"elements", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "|O", kwlist, &elements)) return NULL;

    PyObject *tup = NULL;
    if (elements != NULL) {
        tup = PySequence_Tuple(elements);
        if (!tup) return NULL;
    }

    Py_ssize_t n = tup ? PyTuple_GET_SIZE(tup) : 0;
    PyObject **arr = n > 0 ? _menai_list_cache_alloc_arr(n) : NULL;
    if (n > 0 && !arr) {
        Py_XDECREF(tup);
        PyErr_NoMemory();
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        arr[i] = PyTuple_GET_ITEM(tup, i);
        Py_INCREF(arr[i]);
    }
    Py_XDECREF(tup);

    MenaiList_Object *self = _menai_list_cache_alloc_obj();
    if (self) {
        self->elements = arr;
        self->length = n;
    } else {
        _menai_list_cache_free_arr(arr, n);
    }

    return (PyObject *)self;
}

static void
MenaiList_dealloc(PyObject *self)
{
    MenaiList_Object *lst = (MenaiList_Object *)self;
    Py_ssize_t n = lst->length;
    lst->length = 0;
    PyObject **arr = lst->elements;
    lst->elements = NULL;
    _menai_list_cache_free_arr(arr, n);
    _menai_list_cache_free_obj((MenaiList_Object *)self);
}

static PyObject *
MenaiList_type_name(PyObject *self, PyObject *args)
{
    (void)self;
    (void)args;
    return PyUnicode_FromString("list");
}

static PyObject *
MenaiList_describe(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiList_Object *lst = (MenaiList_Object *)self;
    Py_ssize_t n = lst->length;
    if (n == 0)
        return PyUnicode_FromString("()");

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *desc = PyObject_CallMethod(lst->elements[i], "describe", NULL);
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

    PyObject *result = PyUnicode_FromFormat("(%U)", joined);
    Py_DECREF(joined);
    return result;
}

static PyObject *
MenaiList_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiList_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }

    MenaiList_Object *a = (MenaiList_Object *)self;
    MenaiList_Object *b = (MenaiList_Object *)other;
    if (op == Py_EQ) {
        if (a->length != b->length) Py_RETURN_FALSE;
        for (Py_ssize_t i = 0; i < a->length; i++) {
            int eq = PyObject_RichCompareBool(a->elements[i], b->elements[i], Py_EQ);
            if (eq < 0) return NULL;
            if (!eq) Py_RETURN_FALSE;
        }
        Py_RETURN_TRUE;
    }
    if (op == Py_NE) {
        if (a->length != b->length) Py_RETURN_TRUE;
        for (Py_ssize_t i = 0; i < a->length; i++) {
            int eq = PyObject_RichCompareBool(a->elements[i], b->elements[i], Py_EQ);
            if (eq < 0) return NULL;
            if (!eq) Py_RETURN_TRUE;
        }
        Py_RETURN_FALSE;
    }
    Py_RETURN_NOTIMPLEMENTED;
}

static Py_hash_t
MenaiList_hash(PyObject *self)
{
    MenaiList_Object *lst = (MenaiList_Object *)self;
    PyObject *tup = PyTuple_New(lst->length);
    if (!tup) return -1;

    for (Py_ssize_t i = 0; i < lst->length; i++) {
        Py_INCREF(lst->elements[i]);
        PyTuple_SET_ITEM(tup, i, lst->elements[i]);
    }

    Py_hash_t h = PyObject_Hash(tup);
    Py_DECREF(tup);
    return h;
}

static PyObject *
MenaiList_get_elements(PyObject *self, void *closure)
{
    (void)closure;
    MenaiList_Object *lst = (MenaiList_Object *)self;
    PyObject *tup = PyTuple_New(lst->length);
    if (!tup) return NULL;

    for (Py_ssize_t i = 0; i < lst->length; i++) {
        Py_INCREF(lst->elements[i]);
        PyTuple_SET_ITEM(tup, i, lst->elements[i]);
    }

    return tup;
}

static PyGetSetDef MenaiList_getset[] = {
    {"elements", MenaiList_get_elements, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiList_methods[] = {
    {"type_name", MenaiList_type_name, METH_NOARGS, NULL},
    {"describe", MenaiList_describe, METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiList_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "menai.menai_vm_value.MenaiList",
    .tp_basicsize = sizeof(MenaiList_Object),
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = MenaiList_new,
    .tp_dealloc = MenaiList_dealloc,
    .tp_methods = MenaiList_methods,
    .tp_getset = MenaiList_getset,
    .tp_richcompare = MenaiList_richcompare,
    .tp_hash = MenaiList_hash,
};

PyObject *
menai_list_from_array(PyObject **items, Py_ssize_t n)
{
    PyObject **arr = NULL;
    if (n > 0) {
        arr = _menai_list_cache_alloc_arr(n);
        if (!arr) {
            PyErr_NoMemory();
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            arr[i] = items[i];
            Py_INCREF(arr[i]);
        }
    }

    MenaiList_Object *obj = _menai_list_cache_alloc_obj();
    if (!obj) {
        _menai_list_cache_free_arr(arr, n);
        return NULL;
    }
    obj->elements = arr;
    obj->length = n;
    return (PyObject *)obj;
}

PyObject *
menai_list_from_array_steal(PyObject **items, Py_ssize_t n)
{
    MenaiList_Object *obj = _menai_list_cache_alloc_obj();
    if (!obj) {
        _menai_list_cache_free_arr(items, n);
        return NULL;
    }
    obj->elements = items;
    obj->length = n;
    return (PyObject *)obj;
}

PyObject *
menai_list_from_tuple(PyObject *tup)
{
    Py_ssize_t n = PyTuple_GET_SIZE(tup);
    PyObject **arr = NULL;
    if (n > 0) {
        arr = _menai_list_cache_alloc_arr(n);
        if (!arr) {
            Py_DECREF(tup);
            PyErr_NoMemory();
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            arr[i] = PyTuple_GET_ITEM(tup, i);
            Py_INCREF(arr[i]);
        }
    }

    Py_DECREF(tup);
    MenaiList_Object *obj = _menai_list_cache_alloc_obj();
    if (!obj) {
        _menai_list_cache_free_arr(arr, n);
        return NULL;
    }

    obj->elements = arr;
    obj->length = n;
    return (PyObject *)obj;
}

PyObject *
menai_list_new_empty(void)
{
    MenaiList_Object *obj = _menai_list_cache_alloc_obj();
    if (!obj) return NULL;

    obj->elements = NULL;
    obj->length = 0;
    return (PyObject *)obj;
}

int
menai_vm_list_init(void)
{
    if (PyType_Ready(&MenaiList_Type) < 0) return -1;

    _menai_list_cache_clear();
    return 0;
}
