/*
 * menai_vm_set.c — MenaiSet type implementation.
 *
 * MenaiSet stores an ordered, deduplicated tuple of elements alongside a
 * frozenset of hashable keys for O(1) membership testing.
 *
 * Also provides:
 *   menai_set_new_empty()         — zero-element set for the singleton
 *   menai_set_from_fast_tuple()   — build from a tuple of fast values,
 *                                   used by menai_convert_value()
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_set.h"
#include "menai_vm_value.h"

static PyObject *
MenaiSet_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *elements_arg = NULL;
    static char *kwlist[] = {"elements", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "|O", kwlist, &elements_arg)) return NULL;

    PyObject *src_tup;
    if (elements_arg == NULL) {
        src_tup = PyTuple_New(0);
    } else {
        src_tup = PySequence_Tuple(elements_arg);
    }

    if (!src_tup) return NULL;

    /* Deduplicate, preserving order */
    PyObject *seen = PySet_New(NULL);
    if (!seen) {
        Py_DECREF(src_tup);
        return NULL;
    }
    PyObject *deduped = PyList_New(0);
    if (!deduped) {
        Py_DECREF(seen);
        Py_DECREF(src_tup);
        return NULL;
    }

    Py_ssize_t n = PyTuple_GET_SIZE(src_tup);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *elem = PyTuple_GET_ITEM(src_tup, i);
        PyObject *hk = menai_hashable_key(elem);
        if (!hk) {
            Py_DECREF(deduped);
            Py_DECREF(seen);
            Py_DECREF(src_tup);
            return NULL;
        }

        int has = PySet_Contains(seen, hk);
        if (has < 0) {
            Py_DECREF(hk);
            Py_DECREF(deduped);
            Py_DECREF(seen);
            Py_DECREF(src_tup);
            return NULL;
        }

        if (!has) {
            if (PySet_Add(seen, hk) < 0 || PyList_Append(deduped, elem) < 0) {
                Py_DECREF(hk);
                Py_DECREF(deduped);
                Py_DECREF(seen);
                Py_DECREF(src_tup);
                return NULL;
            }
        }

        Py_DECREF(hk);
    }

    Py_DECREF(seen);
    Py_DECREF(src_tup);

    PyObject *elements = PyList_AsTuple(deduped);
    Py_DECREF(deduped);
    if (!elements) return NULL;

    /* Build frozenset of hashable keys */
    PyObject *members_set = PySet_New(NULL);
    if (!members_set) {
        Py_DECREF(elements);
        return NULL;
    }

    n = PyTuple_GET_SIZE(elements);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *hk = menai_hashable_key(PyTuple_GET_ITEM(elements, i));
        if (!hk) {
            Py_DECREF(members_set);
            Py_DECREF(elements);
            return NULL;
        }

        if (PySet_Add(members_set, hk) < 0) {
            Py_DECREF(hk);
            Py_DECREF(members_set);
            Py_DECREF(elements);
            return NULL;
        }

        Py_DECREF(hk);
    }

    PyObject *members = PyFrozenSet_New(members_set);
    Py_DECREF(members_set);
    if (!members) {
        Py_DECREF(elements);
        return NULL;
    }

    MenaiSet_Object *self = (MenaiSet_Object *)type->tp_alloc(type, 0);
    if (self) {
        self->elements = elements;
        self->members = members;
        self->length = PyTuple_GET_SIZE(elements);
    } else {
        Py_DECREF(elements);
        Py_DECREF(members);
    }

    return (PyObject *)self;
}

static void
MenaiSet_dealloc(PyObject *self)
{
    Py_XDECREF(((MenaiSet_Object *)self)->elements);
    Py_XDECREF(((MenaiSet_Object *)self)->members);
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
    PyObject *elems = ((MenaiSet_Object *)self)->elements;
    Py_ssize_t n = PyTuple_GET_SIZE(elems);
    if (n == 0) return PyUnicode_FromString("#{}");

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *desc = PyObject_CallMethod(PyTuple_GET_ITEM(elems, i), "describe", NULL);
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

    return PyObject_RichCompare(
        ((MenaiSet_Object *)self)->members,
        ((MenaiSet_Object *)other)->members, op);
}

static Py_hash_t
MenaiSet_hash(PyObject *self)
{
    return PyObject_Hash(((MenaiSet_Object *)self)->members);
}

static PyObject *
MenaiSet_get_elements(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *e = ((MenaiSet_Object *)self)->elements;
    Py_INCREF(e);
    return e;
}

static PyObject *
MenaiSet_get_members(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *m = ((MenaiSet_Object *)self)->members;
    Py_INCREF(m);
    return m;
}

static PyGetSetDef MenaiSet_getset[] = {
    {"elements", MenaiSet_get_elements, NULL, NULL, NULL},
    {"members", MenaiSet_get_members, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiSet_methods[] = {
    {"type_name", MenaiSet_type_name, METH_NOARGS, NULL},
    {"describe", MenaiSet_describe, METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiSet_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "menai.menai_vm_value.MenaiSet",
    .tp_basicsize = sizeof(MenaiSet_Object),
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = MenaiSet_new,
    .tp_dealloc = MenaiSet_dealloc,
    .tp_methods = MenaiSet_methods,
    .tp_getset = MenaiSet_getset,
    .tp_richcompare = MenaiSet_richcompare,
    .tp_hash = MenaiSet_hash,
};

PyObject *
menai_set_new_empty(void)
{
    PyObject *empty_tup = PyTuple_New(0);
    if (!empty_tup) return NULL;

    PyObject *args = PyTuple_Pack(1, empty_tup);
    Py_DECREF(empty_tup);
    if (!args) return NULL;

    PyObject *r = MenaiSet_new(&MenaiSet_Type, args, NULL);
    Py_DECREF(args);
    return r;
}

PyObject *
menai_set_from_fast_tuple(PyObject *fast_tup)
{
    PyObject *args = PyTuple_Pack(1, fast_tup);
    Py_DECREF(fast_tup);
    if (!args) return NULL;

    PyObject *r = MenaiSet_new(&MenaiSet_Type, args, NULL);
    Py_DECREF(args);
    return r;
}

int
menai_vm_set_init(void)
{
    return PyType_Ready(&MenaiSet_Type);
}
