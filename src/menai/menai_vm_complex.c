/*
 * menai_vm_complex.c — MenaiComplex type implementation.
 *
 * MenaiComplex wraps a Python complex.  Values are allocated on demand;
 * there are no singletons.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_complex.h"

static PyObject *
MenaiComplex_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *value = NULL;
    static char *kwlist[] = {"value", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O", kwlist, &value)) return NULL;
    if (!PyComplex_Check(value)) {
        PyErr_SetString(PyExc_TypeError, "MenaiComplex requires a complex");
        return NULL;
    }
    MenaiComplex_Object *self = (MenaiComplex_Object *)type->tp_alloc(type, 0);
    if (self) { Py_INCREF(value); self->value = value; }
    return (PyObject *)self;
}

static void
MenaiComplex_dealloc(PyObject *self)
{
    Py_XDECREF(((MenaiComplex_Object *)self)->value);
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
MenaiComplex_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("complex");
}

static PyObject *
MenaiComplex_describe(PyObject *self, PyObject *args)
{
    (void)args;
    /*
     * Delegate to the Python describe() logic via the slow type's method.
     * This is only called for display, not in the hot loop.
     */
    PyObject *cv = ((MenaiComplex_Object *)self)->value;
    PyObject *mod = PyImport_ImportModule("menai.menai_value");
    if (!mod) return NULL;
    PyObject *cls = PyObject_GetAttrString(mod, "MenaiComplex");
    Py_DECREF(mod);
    if (!cls) return NULL;
    PyObject *inst = PyObject_CallOneArg(cls, cv);
    Py_DECREF(cls);
    if (!inst) return NULL;
    PyObject *result = PyObject_CallMethod(inst, "describe", NULL);
    Py_DECREF(inst);
    return result;
}

static PyObject *
MenaiComplex_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiComplex_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    return PyObject_RichCompare(
        ((MenaiComplex_Object *)self)->value,
        ((MenaiComplex_Object *)other)->value, op);
}

static Py_hash_t
MenaiComplex_hash(PyObject *self)
{
    return PyObject_Hash(((MenaiComplex_Object *)self)->value);
}

static PyObject *
MenaiComplex_get_value(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *v = ((MenaiComplex_Object *)self)->value;
    Py_INCREF(v);
    return v;
}

static PyGetSetDef MenaiComplex_getset[] = {
    {"value", MenaiComplex_get_value, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiComplex_methods[] = {
    {"type_name", MenaiComplex_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiComplex_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiComplex_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name        = "menai.menai_vm_value.MenaiComplex",
    .tp_basicsize   = sizeof(MenaiComplex_Object),
    .tp_flags       = Py_TPFLAGS_DEFAULT,
    .tp_new         = MenaiComplex_new,
    .tp_dealloc     = MenaiComplex_dealloc,
    .tp_methods     = MenaiComplex_methods,
    .tp_getset      = MenaiComplex_getset,
    .tp_richcompare = MenaiComplex_richcompare,
    .tp_hash        = MenaiComplex_hash,
};

int
menai_vm_complex_init(void)
{
    return PyType_Ready(&MenaiComplex_Type);
}
