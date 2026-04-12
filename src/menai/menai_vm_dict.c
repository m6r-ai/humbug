/*
 * menai_vm_dict.c — MenaiDict type implementation.
 *
 * MenaiDict stores an ordered tuple of (key, value) pairs and a Python dict
 * mapping hashable keys to pairs for O(1) lookup.
 *
 * Also provides:
 *   menai_dict_new_empty()         — zero-pair dict for the singleton
 *   menai_dict_from_fast_pairs()   — build from a tuple of fast (k,v) pairs,
 *                                    used by menai_convert_value()
 *
 * menai_hashable_key() is also defined here — it is shared by MenaiDict,
 * MenaiSet (via menai_vm_value.h), and the C VM.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_dict.h"
#include "menai_vm_value.h"

static PyObject *MenaiEvalError_type = NULL;

/* ---------------------------------------------------------------------------
 * menai_hashable_key — shared by MenaiDict, MenaiSet, and the C VM
 * ------------------------------------------------------------------------- */

PyObject *
menai_hashable_key(PyObject *key)
{
    PyTypeObject *t = Py_TYPE(key);
    if (t == &MenaiString_Type) {
        PyObject *pystr = menai_string_to_pyunicode(key);
        if (!pystr) return NULL;
        PyObject *r = Py_BuildValue("(sO)", "str", pystr);
        Py_DECREF(pystr);
        return r;
    }
    if (t == &MenaiInteger_Type) return Py_BuildValue("(sO)", "int", ((MenaiInteger_Object *)key)->value);
    if (t == &MenaiFloat_Type) {
        PyObject *pf = PyFloat_FromDouble(((MenaiFloat_Object *)key)->value);
        if (!pf) return NULL;
        PyObject *r = Py_BuildValue("(sO)", "flt", pf);
        Py_DECREF(pf);
        return r;
    }
    if (t == &MenaiComplex_Type) return Py_BuildValue("(sO)", "cplx", ((MenaiComplex_Object *)key)->value);
    if (t == &MenaiBoolean_Type) {
        PyObject *bv = PyBool_FromLong(((MenaiBoolean_Object *)key)->value);
        PyObject *r = Py_BuildValue("(sO)", "bool", bv);
        Py_DECREF(bv);
        return r;
    }
    if (t == &MenaiSymbol_Type) return Py_BuildValue("(sO)", "sym", ((MenaiSymbol_Object *)key)->name);
    if (t == &MenaiStruct_Type) {
        Py_hash_t h = PyObject_Hash(key);
        if (h == -1 && PyErr_Occurred()) {
            /* Re-raise as MenaiEvalError */
            PyObject *exc = PyErr_GetRaisedException();
            PyObject *msg = PyObject_Str(exc);
            Py_XDECREF(exc);
            if (msg) {
                PyErr_SetObject(MenaiEvalError_type, msg);
                Py_DECREF(msg);
            }
            return NULL;
        }
        PyObject *hobj = PyLong_FromSsize_t((Py_ssize_t)h);
        PyObject *r = Py_BuildValue("(sO)", "struct", hobj);
        Py_DECREF(hobj);
        return r;
    }

    PyObject *tn = PyObject_CallMethod(key, "type_name", NULL);
    PyErr_Format(MenaiEvalError_type,
        "Dict keys must be strings, numbers, booleans, or symbols, got %s",
        tn ? PyUnicode_AsUTF8(tn) : "?");

    Py_XDECREF(tn);
    return NULL;
}

/* ---------------------------------------------------------------------------
 * MenaiDict
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiDict_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *pairs_arg = NULL;
    static char *kwlist[] = {"pairs", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "|O", kwlist, &pairs_arg))
        return NULL;

    PyObject *pairs;
    if (pairs_arg == NULL) {
        pairs = PyTuple_New(0);
    } else {
        pairs = PySequence_Tuple(pairs_arg);
    }
    if (!pairs) return NULL;

    /* Build lookup dict */
    PyObject *lookup = PyDict_New();
    if (!lookup) {
        Py_DECREF(pairs);
        return NULL;
    }

    Py_ssize_t n = PyTuple_GET_SIZE(pairs);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *pair = PyTuple_GET_ITEM(pairs, i);
        PyObject *k = PyTuple_GET_ITEM(pair, 0);
        PyObject *hk = menai_hashable_key(k);
        if (!hk) {
            Py_DECREF(lookup);
            Py_DECREF(pairs);
            return NULL;
        }
        if (PyDict_SetItem(lookup, hk, pair) < 0) {
            Py_DECREF(hk);
            Py_DECREF(lookup);
            Py_DECREF(pairs);
            return NULL;
        }
        Py_DECREF(hk);
    }

    MenaiDict_Object *self = (MenaiDict_Object *)type->tp_alloc(type, 0);
    if (self) {
        self->pairs  = pairs;
        self->lookup = lookup;
        self->length = n;
    } else {
        Py_DECREF(pairs);
        Py_DECREF(lookup);
    }
    return (PyObject *)self;
}

static void
MenaiDict_dealloc(PyObject *self)
{
    Py_XDECREF(((MenaiDict_Object *)self)->pairs);
    Py_XDECREF(((MenaiDict_Object *)self)->lookup);
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
    PyObject *pairs = ((MenaiDict_Object *)self)->pairs;
    Py_ssize_t n = PyTuple_GET_SIZE(pairs);
    if (n == 0)
        return PyUnicode_FromString("{}");

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *pair = PyTuple_GET_ITEM(pairs, i);
        PyObject *kd = PyObject_CallMethod(PyTuple_GET_ITEM(pair, 0), "describe", NULL);
        PyObject *vd = kd ? PyObject_CallMethod(PyTuple_GET_ITEM(pair, 1), "describe", NULL) : NULL;
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
    return PyObject_RichCompare(
        ((MenaiDict_Object *)self)->pairs,
        ((MenaiDict_Object *)other)->pairs, op);
}

static Py_hash_t
MenaiDict_hash(PyObject *self)
{
    return PyObject_Hash(((MenaiDict_Object *)self)->pairs);
}

static PyObject *
MenaiDict_to_hashable_key(PyObject *self, PyObject *key)
{
    (void)self;
    return menai_hashable_key(key);
}

static PyObject *
MenaiDict_get_pairs(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *p = ((MenaiDict_Object *)self)->pairs;
    Py_INCREF(p);
    return p;
}

static PyObject *
MenaiDict_get_lookup(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *l = ((MenaiDict_Object *)self)->lookup;
    Py_INCREF(l);
    return l;
}

static PyGetSetDef MenaiDict_getset[] = {
    {"pairs",  MenaiDict_get_pairs,  NULL, NULL, NULL},
    {"lookup", MenaiDict_get_lookup, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiDict_methods[] = {
    {"type_name",       MenaiDict_type_name,       METH_NOARGS,  NULL},
    {"describe",        MenaiDict_describe,         METH_NOARGS,  NULL},
    {"to_hashable_key", (PyCFunction)MenaiDict_to_hashable_key, METH_O | METH_STATIC, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiDict_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name        = "menai.menai_vm_value.MenaiDict",
    .tp_basicsize   = sizeof(MenaiDict_Object),
    .tp_flags       = Py_TPFLAGS_DEFAULT,
    .tp_new         = MenaiDict_new,
    .tp_dealloc     = MenaiDict_dealloc,
    .tp_methods     = MenaiDict_methods,
    .tp_getset      = MenaiDict_getset,
    .tp_richcompare = MenaiDict_richcompare,
    .tp_hash        = MenaiDict_hash,
};

PyObject *
menai_dict_new_empty(void)
{
    PyObject *empty_tup = PyTuple_New(0);
    if (!empty_tup) return NULL;
    PyObject *args = PyTuple_Pack(1, empty_tup);
    Py_DECREF(empty_tup);
    if (!args) return NULL;
    PyObject *r = MenaiDict_new(&MenaiDict_Type, args, NULL);
    Py_DECREF(args);
    return r;
}

PyObject *
menai_dict_from_fast_pairs(PyObject *fast_pairs)
{
    PyObject *args = PyTuple_Pack(1, fast_pairs);
    Py_DECREF(fast_pairs);
    if (!args) return NULL;
    PyObject *r = MenaiDict_new(&MenaiDict_Type, args, NULL);
    Py_DECREF(args);
    return r;
}

int
menai_vm_dict_init(PyObject *eval_error_type)
{
    MenaiEvalError_type = eval_error_type;
    return PyType_Ready(&MenaiDict_Type);
}
