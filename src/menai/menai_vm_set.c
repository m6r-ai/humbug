/*
 * menai_vm_set.c — MenaiSet type implementation.
 *
 * MenaiSet stores an ordered, deduplicated sequence of elements as two
 * parallel C arrays (elements, hashes) plus a pure-C MenaiHashTable for O(1)
 * membership testing.  Hash values are computed once at construction time via
 * menai_value_hash() and reused for all subsequent set operations, with no
 * Python objects allocated during set transforms.
 *
 * Primary construction path for VM operations: menai_set_from_arrays_steal.
 * Python-callable constructor MenaiSet_new is used only for the empty-set
 * singleton and for slow-path construction from Python-level sequences.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_set.h"
#include "menai_vm_memory.h"
#include "menai_vm_hashtable.h"
#include "menai_vm_value.h"

/*
 * _set_free_arrays — release n owned references in elements, then free
 * both arrays.  NULL pointers are safely ignored.
 */
static void
_set_free_arrays(PyObject **elements, Py_hash_t *hashes, Py_ssize_t n)
{
    if (elements) {
        for (Py_ssize_t i = 0; i < n; i++) menai_xrelease(elements[i]);
        PyMem_Free(elements);
    }
    PyMem_Free(hashes);
}

/*
 * menai_set_from_arrays_steal — primary fast constructor.
 *
 * Takes ownership of elements and hashes arrays (and element references).
 * Builds the hash table from hashes then wraps everything in a MenaiSet.
 * Frees both arrays on failure.
 */
PyObject *
menai_set_from_arrays_steal(PyObject **elements, Py_hash_t *hashes, Py_ssize_t n)
{
    MenaiSet_Object *obj =
        (MenaiSet_Object *)MenaiSet_Type.tp_alloc(&MenaiSet_Type, 0);
    if (!obj) {
        _set_free_arrays(elements, hashes, n);
        return NULL;
    }

    if (menai_ht_build(&obj->ht, elements, hashes, n) < 0) {
        _set_free_arrays(elements, hashes, n);
        Py_DECREF(obj);
        return NULL;
    }

    obj->elements = elements;
    obj->hashes = hashes;
    obj->length = n;
    return (PyObject *)obj;
}

/*
 * MenaiSet_new — Python-callable constructor: MenaiSet(elements=None).
 *
 * elements may be any sequence of MenaiValues.  Handles deduplication.
 * Used for the empty-set singleton and for the Python-level MenaiSet() call
 * path.  VM operations use menai_set_from_arrays_steal instead.
 */
static PyObject *
MenaiSet_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    (void)type;
    PyObject *elements_arg = NULL;
    static char *kwlist[] = {"elements", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "|O", kwlist, &elements_arg))
        return NULL;

    if (elements_arg == NULL || PySequence_Size(elements_arg) == 0)
        return menai_set_new_empty();

    PyObject *src = PySequence_Tuple(elements_arg);
    if (!src) return NULL;

    Py_ssize_t src_n = PyTuple_GET_SIZE(src);

    PyObject **elements = (PyObject **)PyMem_Malloc(src_n * sizeof(PyObject *));
    Py_hash_t *hashes = (Py_hash_t *)PyMem_Malloc(src_n * sizeof(Py_hash_t));
    if (!elements || !hashes) {
        PyMem_Free(elements);
        PyMem_Free(hashes);
        Py_DECREF(src);
        PyErr_NoMemory();
        return NULL;
    }

    /*
     * Deduplicate in a single pass using a temporary MenaiHashTable.
     * We initialise it for src_n entries (worst case: no duplicates) and
     * use menai_ht_lookup to detect duplicates before inserting.
     */
    MenaiHashTable seen;
    if (menai_ht_init(&seen, src_n) < 0) {
        PyMem_Free(elements);
        PyMem_Free(hashes);
        Py_DECREF(src);
        return NULL;
    }

    Py_ssize_t out = 0;
    for (Py_ssize_t i = 0; i < src_n; i++) {
        PyObject *elem = PyTuple_GET_ITEM(src, i);
        Py_hash_t h = menai_value_hash(elem);
        if (h == -1) {
            _set_free_arrays(elements, hashes, out);
            menai_ht_free(&seen);
            Py_DECREF(src);
            return NULL;
        }

        Py_ssize_t existing = menai_ht_lookup(&seen, elem, h);
        if (existing == -2) {
            _set_free_arrays(elements, hashes, out);
            menai_ht_free(&seen);
            Py_DECREF(src);
            return NULL;
        }

        if (existing == -1) {
            /* Not yet seen — add it */
            menai_ht_insert(&seen, elem, h, out);
            menai_retain(elem);
            elements[out] = elem;
            hashes[out] = h;
            out++;
        }
        /* else: duplicate — skip */
    }

    menai_ht_free(&seen);
    Py_DECREF(src);

    if (out == 0) {
        PyMem_Free(elements);
        PyMem_Free(hashes);
        return menai_set_new_empty();
    }

    /* Shrink arrays to actual size if deduplication removed elements */
    if (out < src_n) {
        PyObject **te = (PyObject **)PyMem_Realloc(elements, out * sizeof(PyObject *));
        Py_hash_t *th = (Py_hash_t *)PyMem_Realloc(hashes, out * sizeof(Py_hash_t));
        if (te) elements = te;
        if (th) hashes = th;
    }

    return menai_set_from_arrays_steal(elements, hashes, out);
}

static void
MenaiSet_dealloc(PyObject *self)
{
    MenaiSet_Object *s = (MenaiSet_Object *)self;
    _set_free_arrays(s->elements, s->hashes, s->length);
    menai_ht_free(&s->ht);
    s->elements = NULL;
    s->hashes = NULL;
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
MenaiSet_type_name(PyObject *self, PyObject *args)
{
    (void)self;
    (void)args;
    return PyUnicode_FromString("set");
}

static PyObject *
MenaiSet_describe(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiSet_Object *s = (MenaiSet_Object *)self;
    Py_ssize_t n = s->length;
    if (n == 0) return PyUnicode_FromString("#{}");

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *desc = PyObject_CallMethod(s->elements[i], "describe", NULL);
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

    PyObject *result = PyUnicode_FromFormat("#{%U}", joined);
    Py_DECREF(joined);
    return result;
}

static PyObject *
MenaiSet_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiSet_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }

    MenaiSet_Object *a = (MenaiSet_Object *)self;
    MenaiSet_Object *b = (MenaiSet_Object *)other;

    if (op == Py_EQ) {
        if (a->length != b->length) Py_RETURN_FALSE;
        /* Every element of a must be in b */
        for (Py_ssize_t i = 0; i < a->length; i++) {
            Py_ssize_t idx = menai_ht_lookup(&b->ht, a->elements[i], a->hashes[i]);
            if (idx == -2) return NULL;
            if (idx == -1) Py_RETURN_FALSE;
        }
        Py_RETURN_TRUE;
    }

    if (op == Py_NE) {
        if (a->length != b->length) Py_RETURN_TRUE;
        for (Py_ssize_t i = 0; i < a->length; i++) {
            Py_ssize_t idx = menai_ht_lookup(&b->ht, a->elements[i], a->hashes[i]);
            if (idx == -2) return NULL;
            if (idx == -1) Py_RETURN_TRUE;
        }
        Py_RETURN_FALSE;
    }

    /* LE: a is subset of b */
    if (op == Py_LE) {
        if (a->length > b->length) Py_RETURN_FALSE;
        for (Py_ssize_t i = 0; i < a->length; i++) {
            Py_ssize_t idx = menai_ht_lookup(&b->ht, a->elements[i], a->hashes[i]);
            if (idx == -2) return NULL;
            if (idx == -1) Py_RETURN_FALSE;
        }
        Py_RETURN_TRUE;
    }

    Py_RETURN_NOTIMPLEMENTED;
}

static Py_hash_t
MenaiSet_hash(PyObject *self)
{
    MenaiSet_Object *s = (MenaiSet_Object *)self;
    Py_ssize_t n = s->length;
    /*
     * XOR all element hashes — order-independent, matching frozenset semantics.
     */
    Py_uhash_t acc = 0;
    for (Py_ssize_t i = 0; i < n; i++)
        acc ^= (Py_uhash_t)s->hashes[i];
    Py_hash_t h = (Py_hash_t)(acc == (Py_uhash_t)-1 ? -2 : acc);
    return h;
}

static PyMethodDef MenaiSet_methods[] = {
    {"type_name", MenaiSet_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiSet_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiSet_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_vm_value.MenaiSet",
    .tp_basicsize = sizeof(MenaiSet_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT,
    .tp_new       = MenaiSet_new,
    .tp_dealloc   = MenaiSet_dealloc,
    .tp_methods   = MenaiSet_methods,
    .tp_richcompare = MenaiSet_richcompare,
    .tp_hash      = MenaiSet_hash,
};

PyObject *
menai_set_new_empty(void)
{
    MenaiSet_Object *obj =
        (MenaiSet_Object *)MenaiSet_Type.tp_alloc(&MenaiSet_Type, 0);
    if (!obj) return NULL;

    obj->elements = NULL;
    obj->hashes = NULL;
    obj->ht.slots = NULL;
    obj->ht.slot_count = 0;
    obj->ht.used = 0;
    obj->length = 0;
    return (PyObject *)obj;
}

int
menai_vm_set_init(void)
{
    return PyType_Ready(&MenaiSet_Type);
}
