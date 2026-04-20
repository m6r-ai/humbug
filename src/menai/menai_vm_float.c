/*
 * menai_vm_float.c — MenaiFloat type implementation.
 *
 * MenaiFloat stores a C double.  Values are allocated on demand; there are
 * no singletons.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_hashtable.h"

#include "menai_vm_float.h"

static PyObject *
MenaiFloat_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    double value = 0.0;
    static char *kwlist[] = {"value", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "d", kwlist, &value)) return NULL;

    MenaiFloat_Object *self = (MenaiFloat_Object *)type->tp_alloc(type, 0);
    if (self) self->value = value;

    return (PyObject *)self;
}

static PyObject *
MenaiFloat_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("float");
}

static PyObject *
MenaiFloat_describe(PyObject *self, PyObject *args)
{
    (void)args;
    char *buf = PyOS_double_to_string(((MenaiFloat_Object *)self)->value,
                                      'r', 0, Py_DTSF_ADD_DOT_0, NULL);
    if (!buf) return NULL;
    PyObject *s = PyUnicode_FromString(buf);
    PyMem_Free(buf);
    return s;
}

static PyObject *
MenaiFloat_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiFloat_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }

    double a = ((MenaiFloat_Object *)self)->value;
    double b = ((MenaiFloat_Object *)other)->value;
    switch (op) {
        case Py_EQ: return PyBool_FromLong(a == b);
        case Py_NE: return PyBool_FromLong(a != b);
        case Py_LT: return PyBool_FromLong(a < b);
        case Py_LE: return PyBool_FromLong(a <= b);
        case Py_GT: return PyBool_FromLong(a > b);
        case Py_GE: return PyBool_FromLong(a >= b);
        default: Py_RETURN_NOTIMPLEMENTED;
    }
}

static Py_hash_t
MenaiFloat_hash(PyObject *self)
{
    return menai_hash_double(((MenaiFloat_Object *)self)->value);
}

static PyObject *
MenaiFloat_get_value(PyObject *self, void *closure)
{
    (void)closure;
    return PyFloat_FromDouble(((MenaiFloat_Object *)self)->value);
}

static PyGetSetDef MenaiFloat_getset[] = {
    {"value", MenaiFloat_get_value, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiFloat_methods[] = {
    {"type_name", MenaiFloat_type_name, METH_NOARGS, NULL},
    {"describe", MenaiFloat_describe, METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiFloat_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "menai.menai_vm_value.MenaiFloat",
    .tp_basicsize = sizeof(MenaiFloat_Object),
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = MenaiFloat_new,
    .tp_methods = MenaiFloat_methods,
    .tp_getset = MenaiFloat_getset,
    .tp_richcompare = MenaiFloat_richcompare,
    .tp_hash = MenaiFloat_hash,
};

int
menai_vm_float_init(void)
{
    return PyType_Ready(&MenaiFloat_Type);
}
