/*
 * menai_vm_none.c — MenaiNone type implementation.
 *
 * MenaiNone is a singleton with no payload.  A single instance (_Menai_NONE)
 * is created at init time and returned by menai_none_singleton().
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_none.h"

static PyObject *_Menai_NONE = NULL;

static PyObject *
MenaiNone_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    (void)args; (void)kwargs;
    MenaiNone_Object *self = (MenaiNone_Object *)type->tp_alloc(type, 0);
    return (PyObject *)self;
}

static PyObject *
MenaiNone_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("none");
}

PyObject *
MenaiNone_describe(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("#none");
}

PyObject *
MenaiNone_to_python(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    Py_RETURN_NONE;
}

static PyObject *
MenaiNone_richcompare(PyObject *self, PyObject *other, int op)
{
    (void)self;
    if (op == Py_EQ) return PyBool_FromLong(Py_TYPE(other) == &MenaiNone_Type);
    if (op == Py_NE) return PyBool_FromLong(Py_TYPE(other) != &MenaiNone_Type);
    Py_RETURN_NOTIMPLEMENTED;
}

static Py_hash_t
MenaiNone_hash(PyObject *self)
{
    (void)self;
    return (Py_hash_t)0x4e6f6e65UL;  /* "None" — matches menai_value_hash */
}

static PyMethodDef MenaiNone_methods[] = {
    {"type_name", MenaiNone_type_name, METH_NOARGS, NULL},
    {"describe", MenaiNone_describe, METH_NOARGS, NULL},
    {"to_python", MenaiNone_to_python, METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiNone_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "menai.menai_vm_value.MenaiNone",
    .tp_basicsize = sizeof(MenaiNone_Object),
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = MenaiNone_new,
    .tp_methods = MenaiNone_methods,
    .tp_richcompare = MenaiNone_richcompare,
    .tp_hash = MenaiNone_hash,
};

PyObject *
menai_none_singleton(void)
{
    return _Menai_NONE;
}

int
menai_vm_none_init(void)
{
    if (PyType_Ready(&MenaiNone_Type) < 0) return -1;

    _Menai_NONE = PyObject_CallNoArgs((PyObject *)&MenaiNone_Type);
    if (!_Menai_NONE) return -1;

    return 0;
}
