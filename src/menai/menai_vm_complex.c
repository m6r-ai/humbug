/*
 * menai_vm_complex.c — MenaiComplex type implementation.
 *
 * MenaiComplex stores a C double pair (real, imag).  Values are allocated
 * on demand; there are no singletons.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <math.h>

#include "menai_vm_complex.h"

static PyObject *
MenaiComplex_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    double real = 0.0, imag = 0.0;
    static char *kwlist[] = {"real", "imag", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "dd", kwlist, &real, &imag)) return NULL;
    MenaiComplex_Object *self = (MenaiComplex_Object *)type->tp_alloc(type, 0);
    if (self) {
        self->real = real;
        self->imag = imag;
    }
    return (PyObject *)self;
}

static PyObject *
MenaiComplex_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("complex");
}

/*
 * Format a single float component for describe():
 * if the value is exactly representable as an integer, use integer notation.
 */
static int
fmt_component(char *buf, size_t bufsize, double x)
{
    double ipart;
    if (modf(x, &ipart) == 0.0 && ipart >= -1e15 && ipart <= 1e15) {
        return snprintf(buf, bufsize, "%.0f", ipart);
    }
    return snprintf(buf, bufsize, "%g", x);
}

PyObject *
MenaiComplex_describe(PyObject *self, PyObject *args)
{
    (void)args;
    double r = ((MenaiComplex_Object *)self)->real;
    double i = ((MenaiComplex_Object *)self)->imag;

    char rbuf[64], ibuf[64], out[160];

    if (r == 0.0 && i == 0.0) {
        return PyUnicode_FromString("0+0j");
    }

    if (r == 0.0) {
        fmt_component(ibuf, sizeof(ibuf), i);
        snprintf(out, sizeof(out), "%sj", ibuf);
        return PyUnicode_FromString(out);
    }

    fmt_component(rbuf, sizeof(rbuf), r);
    fmt_component(ibuf, sizeof(ibuf), i >= 0.0 ? i : -i);
    snprintf(out, sizeof(out), "%s%s%sj",
             rbuf, i >= 0.0 ? "+" : "-", ibuf);
    return PyUnicode_FromString(out);
}

static PyObject *
MenaiComplex_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiComplex_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    double ar = ((MenaiComplex_Object *)self)->real;
    double ai = ((MenaiComplex_Object *)self)->imag;
    double br = ((MenaiComplex_Object *)other)->real;
    double bi = ((MenaiComplex_Object *)other)->imag;
    switch (op) {
        case Py_EQ: return PyBool_FromLong(ar == br && ai == bi);
        case Py_NE: return PyBool_FromLong(ar != br || ai != bi);
        default:    Py_RETURN_NOTIMPLEMENTED;
    }
}

static Py_hash_t
MenaiComplex_hash(PyObject *self)
{
    double r = ((MenaiComplex_Object *)self)->real;
    double i = ((MenaiComplex_Object *)self)->imag;
    PyObject *pc = PyComplex_FromDoubles(r, i);
    if (!pc) return -1;
    Py_hash_t h = PyObject_Hash(pc);
    Py_DECREF(pc);
    return h;
}

static PyObject *
MenaiComplex_get_real(PyObject *self, void *closure)
{
    (void)closure;
    return PyFloat_FromDouble(((MenaiComplex_Object *)self)->real);
}

static PyObject *
MenaiComplex_get_imag(PyObject *self, void *closure)
{
    (void)closure;
    return PyFloat_FromDouble(((MenaiComplex_Object *)self)->imag);
}

static PyGetSetDef MenaiComplex_getset[] = {
    {"real", MenaiComplex_get_real, NULL, NULL, NULL},
    {"imag", MenaiComplex_get_imag, NULL, NULL, NULL},
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
