/*
 * menai_vm_set.c — MenaiSet type implementation.
 *
 * MenaiSet stores an ordered, deduplicated sequence of elements as two
 * parallel C arrays (elements, hkeys) plus a Python frozenset of canonical
 * hash keys for O(1) membership testing.  Canonical hash keys are computed
 * once at construction time and reused for all subsequent set operations.
 *
 * The primary construction path for VM operations is menai_set_from_arrays_steal,
 * which takes already-prepared parallel arrays and builds the members frozenset
 * in a single pass.  The Python-callable MenaiSet() constructor (MenaiSet_new)
 * is used only for the empty-set singleton and for slow-path construction from
 * Python-level sequences.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_set.h"
#include "menai_vm_value.h"

/*
 * _set_free_arrays — release n owned references in each array and free them.
 * Any NULL array pointer is safely ignored.
 */
static void
_set_free_arrays(PyObject **elements, PyObject **hkeys, Py_ssize_t n)
{
    if (elements) {
        for (Py_ssize_t i = 0; i < n; i++) Py_XDECREF(elements[i]);
        PyMem_Free(elements);
    }
    if (hkeys) {
        for (Py_ssize_t i = 0; i < n; i++) Py_XDECREF(hkeys[i]);
        PyMem_Free(hkeys);
    }
}

/*
 * _set_build_members — build a frozenset from an array of hkeys.
 *
 * Returns a new reference, or NULL on error.
 * Does not consume or modify the array.
 */
static PyObject *
_set_build_members(PyObject **hkeys, Py_ssize_t n)
{
    PyObject *mset = PySet_New(NULL);
    if (!mset) return NULL;

    for (Py_ssize_t i = 0; i < n; i++) {
        if (PySet_Add(mset, hkeys[i]) < 0) {
            Py_DECREF(mset);
            return NULL;
        }
    }

    PyObject *members = PyFrozenSet_New(mset);
    Py_DECREF(mset);
    return members;
}

/*
 * menai_set_from_arrays_steal — the primary fast constructor.
 *
 * Takes ownership of elements and hkeys arrays (and their contents).
 * Builds the members frozenset from hkeys then wraps everything in a MenaiSet.
 * Frees both arrays on failure.
 */
PyObject *
menai_set_from_arrays_steal(PyObject **elements, PyObject **hkeys, Py_ssize_t n)
{
    PyObject *members = _set_build_members(hkeys, n);
    if (!members) {
        _set_free_arrays(elements, hkeys, n);
        return NULL;
    }

    MenaiSet_Object *obj =
        (MenaiSet_Object *)MenaiSet_Type.tp_alloc(&MenaiSet_Type, 0);
    if (!obj) {
        Py_DECREF(members);
        _set_free_arrays(elements, hkeys, n);
        return NULL;
    }

    obj->elements = elements;
    obj->hkeys = hkeys;
    obj->members = members;
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

    /* Deduplicate in a single pass, computing hkeys as we go. */
    PyObject **elements = (PyObject **)PyMem_Malloc(src_n * sizeof(PyObject *));
    PyObject **hkeys = (PyObject **)PyMem_Malloc(src_n * sizeof(PyObject *));
    if (!elements || !hkeys) {
        PyMem_Free(elements);
        PyMem_Free(hkeys);
        Py_DECREF(src);
        PyErr_NoMemory();
        return NULL;
    }

    PyObject *seen = PySet_New(NULL);
    if (!seen) {
        PyMem_Free(elements);
        PyMem_Free(hkeys);
        Py_DECREF(src);
        return NULL;
    }

    Py_ssize_t out = 0;
    for (Py_ssize_t i = 0; i < src_n; i++) {
        PyObject *elem = PyTuple_GET_ITEM(src, i);
        PyObject *hk = menai_hashable_key(elem);
        if (!hk) {
            _set_free_arrays(elements, hkeys, out);
            Py_DECREF(seen);
            Py_DECREF(src);
            return NULL;
        }

        int has = PySet_Contains(seen, hk);
        if (has < 0) {
            Py_DECREF(hk);
            _set_free_arrays(elements, hkeys, out);
            Py_DECREF(seen);
            Py_DECREF(src);
            return NULL;
        }

        if (!has) {
            if (PySet_Add(seen, hk) < 0) {
                Py_DECREF(hk);
                _set_free_arrays(elements, hkeys, out);
                Py_DECREF(seen);
                Py_DECREF(src);
                return NULL;
            }
            Py_INCREF(elem);
            elements[out] = elem;
            hkeys[out] = hk;  /* steal hk */
            out++;
        } else {
            Py_DECREF(hk);
        }
    }

    Py_DECREF(seen);
    Py_DECREF(src);

    if (out == 0) {
        PyMem_Free(elements);
        PyMem_Free(hkeys);
        return menai_set_new_empty();
    }

    /* Shrink arrays to actual size if deduplication removed elements. */
    if (out < src_n) {
        PyObject **te = (PyObject **)PyMem_Realloc(elements, out * sizeof(PyObject *));
        PyObject **th = (PyObject **)PyMem_Realloc(hkeys, out * sizeof(PyObject *));
        if (te) elements = te;
        if (th) hkeys = th;
    }

    return menai_set_from_arrays_steal(elements, hkeys, out);
}

static void
MenaiSet_dealloc(PyObject *self)
{
    MenaiSet_Object *s = (MenaiSet_Object *)self;
    Py_ssize_t n = s->length;
    _set_free_arrays(s->elements, s->hkeys, n);
    Py_XDECREF(s->members);
    s->elements = NULL;
    s->hkeys = NULL;
    s->members = NULL;
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

    /* Set equality/inequality is membership-based, not order-based. */
    return PyObject_RichCompare(
        ((MenaiSet_Object *)self)->members,
        ((MenaiSet_Object *)other)->members, op);
}

static Py_hash_t
MenaiSet_hash(PyObject *self)
{
    return PyObject_Hash(((MenaiSet_Object *)self)->members);
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

    PyObject *members = PyFrozenSet_New(NULL);
    if (!members) {
        Py_DECREF(obj);
        return NULL;
    }

    obj->elements = NULL;
    obj->hkeys = NULL;
    obj->members = members;
    obj->length = 0;
    return (PyObject *)obj;
}

int
menai_vm_set_init(void)
{
    return PyType_Ready(&MenaiSet_Type);
}
