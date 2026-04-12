/*
 * menai_vm_integer.c — MenaiInteger type implementation.
 *
 * MenaiInteger wraps a Python int (arbitrary precision).  Values are
 * allocated on demand; there are no singletons.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_integer.h"

static PyObject *
MenaiInteger_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *value = NULL;
    static char *kwlist[] = {"value", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O", kwlist, &value)) return NULL;
    if (!PyLong_Check(value)) {
        PyErr_SetString(PyExc_TypeError, "MenaiInteger requires an int");
        return NULL;
    }
    MenaiInteger_Object *self = (MenaiInteger_Object *)type->tp_alloc(type, 0);
    if (self) { Py_INCREF(value); self->value = value; }
    return (PyObject *)self;
}

static void
MenaiInteger_dealloc(PyObject *self)
{
    Py_XDECREF(((MenaiInteger_Object *)self)->value);
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
MenaiInteger_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("integer");
}

static PyObject *
MenaiInteger_describe(PyObject *self, PyObject *args)
{
    (void)args;
    return PyObject_Str(((MenaiInteger_Object *)self)->value);
}

static PyObject *
MenaiInteger_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiInteger_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    return PyObject_RichCompare(
        ((MenaiInteger_Object *)self)->value,
        ((MenaiInteger_Object *)other)->value, op);
}

static Py_hash_t
MenaiInteger_hash(PyObject *self)
{
    return PyObject_Hash(((MenaiInteger_Object *)self)->value);
}

static PyObject *
MenaiInteger_get_value(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *v = ((MenaiInteger_Object *)self)->value;
    Py_INCREF(v);
    return v;
}

static PyGetSetDef MenaiInteger_getset[] = {
    {"value", MenaiInteger_get_value, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiInteger_methods[] = {
    {"type_name", MenaiInteger_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiInteger_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiInteger_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name        = "menai.menai_vm_value.MenaiInteger",
    .tp_basicsize   = sizeof(MenaiInteger_Object),
    .tp_flags       = Py_TPFLAGS_DEFAULT,
    .tp_new         = MenaiInteger_new,
    .tp_dealloc     = MenaiInteger_dealloc,
    .tp_methods     = MenaiInteger_methods,
    .tp_getset      = MenaiInteger_getset,
    .tp_richcompare = MenaiInteger_richcompare,
    .tp_hash        = MenaiInteger_hash,
};

int
menai_vm_integer_init(void)
{
    return PyType_Ready(&MenaiInteger_Type);
}
