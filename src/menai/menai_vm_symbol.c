/*
 * menai_vm_symbol.c — MenaiSymbol type implementation.
 *
 * MenaiSymbol wraps an interned Python str.  Interning is applied at
 * construction time so that symbol equality reduces to a single pointer
 * comparison.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_symbol.h"

static PyObject *
MenaiSymbol_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *name = NULL;
    static char *kwlist[] = {"name", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "U", kwlist, &name)) return NULL;
    Py_INCREF(name);
    PyUnicode_InternInPlace(&name);
    MenaiSymbol_Object *self = (MenaiSymbol_Object *)type->tp_alloc(type, 0);
    if (self) {
        self->name = name;
    } else {
        Py_DECREF(name);
    }
    return (PyObject *)self;
}

static void
MenaiSymbol_dealloc(PyObject *self)
{
    Py_XDECREF(((MenaiSymbol_Object *)self)->name);
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
MenaiSymbol_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("symbol");
}

static PyObject *
MenaiSymbol_describe(PyObject *self, PyObject *args)
{
    (void)args;
    PyObject *n = ((MenaiSymbol_Object *)self)->name;
    Py_INCREF(n);
    return n;
}

static PyObject *
MenaiSymbol_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiSymbol_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    PyObject *na = ((MenaiSymbol_Object *)self)->name;
    PyObject *nb = ((MenaiSymbol_Object *)other)->name;
    switch (op) {
        case Py_EQ: return PyBool_FromLong(na == nb);
        case Py_NE: return PyBool_FromLong(na != nb);
        default:    Py_RETURN_NOTIMPLEMENTED;
    }
}

static Py_hash_t
MenaiSymbol_hash(PyObject *self)
{
    return PyObject_Hash(((MenaiSymbol_Object *)self)->name);
}

static PyObject *
MenaiSymbol_get_name(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *n = ((MenaiSymbol_Object *)self)->name;
    Py_INCREF(n);
    return n;
}

static PyGetSetDef MenaiSymbol_getset[] = {
    {"name", MenaiSymbol_get_name, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiSymbol_methods[] = {
    {"type_name", MenaiSymbol_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiSymbol_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiSymbol_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name        = "menai.menai_vm_value.MenaiSymbol",
    .tp_basicsize   = sizeof(MenaiSymbol_Object),
    .tp_flags       = Py_TPFLAGS_DEFAULT,
    .tp_new         = MenaiSymbol_new,
    .tp_dealloc     = MenaiSymbol_dealloc,
    .tp_methods     = MenaiSymbol_methods,
    .tp_getset      = MenaiSymbol_getset,
    .tp_richcompare = MenaiSymbol_richcompare,
    .tp_hash        = MenaiSymbol_hash,
};

PyObject *
menai_symbol_alloc(PyObject *name)
{
    Py_INCREF(name);
    PyUnicode_InternInPlace(&name);
    MenaiSymbol_Object *self = (MenaiSymbol_Object *)MenaiSymbol_Type.tp_alloc(&MenaiSymbol_Type, 0);
    if (self) {
        self->name = name;
    } else {
        Py_DECREF(name);
    }
    return (PyObject *)self;
}

int
menai_vm_symbol_init(void)
{
    return PyType_Ready(&MenaiSymbol_Type);
}
