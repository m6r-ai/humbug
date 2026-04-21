/*
 * menai_vm_boolean.c — MenaiBoolean type implementation.
 *
 * MenaiBoolean stores a C int (0 or 1).  Two singletons (_Menai_TRUE and
 * _Menai_FALSE) are created at init time and returned by menai_boolean_true()
 * and menai_boolean_false().
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_boolean.h"

static PyObject *_Menai_TRUE = NULL;
static PyObject *_Menai_FALSE = NULL;

static PyObject *
MenaiBoolean_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    int value = 0;
    static char *kwlist[] = {"value", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "p", kwlist, &value)) return NULL;
    MenaiBoolean_Object *self = (MenaiBoolean_Object *)type->tp_alloc(type, 0);
    if (self) self->value = value;
    return (PyObject *)self;
}

static PyObject *
MenaiBoolean_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("boolean");
}

PyObject *
MenaiBoolean_describe(PyObject *self, PyObject *args)
{
    (void)args;
    return PyUnicode_FromString(((MenaiBoolean_Object *)self)->value ? "#t" : "#f");
}

PyObject *
MenaiBoolean_to_python(PyObject *self, PyObject *args)
{
    (void)args;
    return PyBool_FromLong(((MenaiBoolean_Object *)self)->value);
}

static PyObject *
MenaiBoolean_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiBoolean_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    int a = ((MenaiBoolean_Object *)self)->value;
    int b = ((MenaiBoolean_Object *)other)->value;
    switch (op) {
        case Py_EQ: return PyBool_FromLong(a == b);
        case Py_NE: return PyBool_FromLong(a != b);
        default:    Py_RETURN_NOTIMPLEMENTED;
    }
}

static Py_hash_t
MenaiBoolean_hash(PyObject *self)
{
    /* Menai booleans hash to 0 (false) or 1 (true), matching Python's convention. */
    return (Py_hash_t)((MenaiBoolean_Object *)self)->value;
}

static PyObject *
MenaiBoolean_get_value(PyObject *self, void *closure)
{
    (void)closure;
    return PyBool_FromLong(((MenaiBoolean_Object *)self)->value);
}

static PyGetSetDef MenaiBoolean_getset[] = {
    {"value", MenaiBoolean_get_value, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiBoolean_methods[] = {
    {"type_name", MenaiBoolean_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiBoolean_describe,  METH_NOARGS, NULL},
    {"to_python", MenaiBoolean_to_python, METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiBoolean_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "menai.menai_vm_value.MenaiBoolean",
    .tp_basicsize = sizeof(MenaiBoolean_Object),
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = MenaiBoolean_new,
    .tp_methods = MenaiBoolean_methods,
    .tp_getset = MenaiBoolean_getset,
    .tp_richcompare = MenaiBoolean_richcompare,
    .tp_hash = MenaiBoolean_hash,
};

PyObject *
menai_boolean_true(void)
{
    return _Menai_TRUE;
}

PyObject *
menai_boolean_false(void)
{
    return _Menai_FALSE;
}

int
menai_vm_boolean_init(void)
{
    if (PyType_Ready(&MenaiBoolean_Type) < 0)
        return -1;

    PyObject *true_args = Py_BuildValue("(i)", 1);
    PyObject *false_args = Py_BuildValue("(i)", 0);
    _Menai_TRUE = true_args ? MenaiBoolean_new(&MenaiBoolean_Type, true_args,  NULL) : NULL;
    _Menai_FALSE = false_args ? MenaiBoolean_new(&MenaiBoolean_Type, false_args, NULL) : NULL;
    Py_XDECREF(true_args);
    Py_XDECREF(false_args);

    if (!_Menai_TRUE || !_Menai_FALSE)
        return -1;

    return 0;
}
