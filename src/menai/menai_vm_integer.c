/*
 * menai_vm_integer.c — MenaiInteger type implementation.
 *
 * Three-tier representation: small integer cache for [-5, 256], inline long
 * for values that fit in a C long, and MenaiInt bignum for everything else.
 * The Python C API is only used at the boundary (convert_value / to_slow).
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <string.h>

#include "menai_vm_integer.h"
#include "menai_vm_memory.h"

static PyObject *_integer_cache[MENAI_INT_CACHE_SIZE];

static PyObject *
MenaiInteger_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *value = NULL;
    static char *kwlist[] = {"value", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O", kwlist, &value)) {
        return NULL;
    }
    if (!PyLong_Check(value)) {
        PyErr_SetString(PyExc_TypeError, "MenaiInteger requires an int");
        return NULL;
    }

    int overflow = 0;
    long v = PyLong_AsLongAndOverflow(value, &overflow);
    if (!overflow) {
        if (v == -1 && PyErr_Occurred()) {
            return NULL;
        }
        return menai_integer_from_long(v);
    }

    MenaiInt big;
    menai_int_init(&big);
    if (menai_int_from_pylong(value, &big) < 0) {
        return NULL;
    }
    return menai_integer_from_bigint(big);
}

static void
MenaiInteger_dealloc(PyObject *self)
{
    MenaiInteger_Object *obj = (MenaiInteger_Object *)self;
    if (!obj->is_big) {
        long v = obj->small;
        if (v >= MENAI_INT_CACHE_MIN && v <= MENAI_INT_CACHE_MAX) {
            /* Cached singleton — must never be freed. Restore refcount. */
            Py_SET_REFCNT(self, 1);
            return;
        }
    } else {
        menai_int_free(&obj->big);
    }
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
MenaiInteger_type_name(PyObject *self, PyObject *args)
{
    (void)self;
    (void)args;
    return PyUnicode_FromString("integer");
}

static PyObject *
MenaiInteger_describe(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiInteger_Object *obj = (MenaiInteger_Object *)self;
    if (!obj->is_big) {
        return PyUnicode_FromFormat("%ld", obj->small);
    }
    char *s = NULL;
    if (menai_int_to_string(&obj->big, 10, &s) < 0) {
        return NULL;
    }
    PyObject *r = PyUnicode_FromString(s);
    PyMem_Free(s);
    return r;
}

static PyObject *
MenaiInteger_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiInteger_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }

    MenaiInteger_Object *a = (MenaiInteger_Object *)self;
    MenaiInteger_Object *b = (MenaiInteger_Object *)other;

    /*
     * Build temporary MenaiInts on the stack for small values so we can
     * use the unified menai_int_* comparison path.
     */
    MenaiInt tmp_a, tmp_b;
    menai_int_init(&tmp_a);
    menai_int_init(&tmp_b);

    const MenaiInt *pa;
    const MenaiInt *pb;

    if (!a->is_big) {
        if (menai_int_from_long(a->small, &tmp_a) < 0) {
            return NULL;
        }
        pa = &tmp_a;
    } else {
        pa = &a->big;
    }

    if (!b->is_big) {
        if (menai_int_from_long(b->small, &tmp_b) < 0) {
            menai_int_free(&tmp_a);
            return NULL;
        }
        pb = &tmp_b;
    } else {
        pb = &b->big;
    }

    int result;
    switch (op) {
        case Py_EQ: result = menai_int_eq(pa, pb); break;
        case Py_NE: result = menai_int_ne(pa, pb); break;
        case Py_LT: result = menai_int_lt(pa, pb); break;
        case Py_GT: result = menai_int_gt(pa, pb); break;
        case Py_LE: result = menai_int_le(pa, pb); break;
        case Py_GE: result = menai_int_ge(pa, pb); break;
        default:
            menai_int_free(&tmp_a);
            menai_int_free(&tmp_b);
            Py_RETURN_NOTIMPLEMENTED;
    }

    menai_int_free(&tmp_a);
    menai_int_free(&tmp_b);
    return result ? Py_True : Py_False;
}

static Py_hash_t
MenaiInteger_hash(PyObject *self)
{
    MenaiInteger_Object *obj = (MenaiInteger_Object *)self;
    if (!obj->is_big) {
        /* CPython's hash of an integer is the value itself, -1 mapped to -2. */
        Py_hash_t h = (Py_hash_t)obj->small;
        return h == -1 ? -2 : h;
    }
    return menai_int_hash(&obj->big);
}

static PyObject *
MenaiInteger_get_value(PyObject *self, void *closure)
{
    (void)closure;
    MenaiInteger_Object *obj = (MenaiInteger_Object *)self;
    if (!obj->is_big) {
        return PyLong_FromLong(obj->small);
    }
    return menai_int_to_pylong(&obj->big);
}

static PyGetSetDef MenaiInteger_getset[] = {
    {"value", MenaiInteger_get_value, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiInteger_methods[] = {
    {"type_name", MenaiInteger_type_name, METH_NOARGS, NULL},
    {"describe", MenaiInteger_describe, METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiInteger_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "menai.menai_vm_value.MenaiInteger",
    .tp_basicsize = sizeof(MenaiInteger_Object),
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_new = MenaiInteger_new,
    .tp_dealloc = MenaiInteger_dealloc,
    .tp_methods = MenaiInteger_methods,
    .tp_getset = MenaiInteger_getset,
    .tp_richcompare = MenaiInteger_richcompare,
    .tp_hash = MenaiInteger_hash,
};

PyObject *
menai_integer_from_long(long n)
{
    if (n >= MENAI_INT_CACHE_MIN && n <= MENAI_INT_CACHE_MAX) {
        PyObject *cached = _integer_cache[n - MENAI_INT_CACHE_MIN];
        menai_retain(cached);
        return cached;
    }

    MenaiInteger_Object *r = (MenaiInteger_Object *)MenaiInteger_Type.tp_alloc(&MenaiInteger_Type, 0);
    if (r == NULL) {
        return NULL;
    }
    r->is_big = 0;
    r->small = n;
    menai_int_init(&r->big);
    return (PyObject *)r;
}

PyObject *
menai_integer_from_bigint(MenaiInt src)
{
    /*
     * If the value fits in a long, demote to small representation so the
     * inline fast path is used for subsequent operations.
     */
    if (menai_int_fits_long(&src)) {
        long v;
        if (menai_int_to_long(&src, &v) < 0) {
            menai_int_free(&src);
            return NULL;
        }
        menai_int_free(&src);
        return menai_integer_from_long(v);
    }

    MenaiInteger_Object *r = (MenaiInteger_Object *)MenaiInteger_Type.tp_alloc(&MenaiInteger_Type, 0);
    if (r == NULL) {
        menai_int_free(&src);
        return NULL;
    }
    r->is_big = 1;
    r->small = 0;
    r->big = src; /* transfer ownership */
    return (PyObject *)r;
}

int
menai_vm_integer_init(void)
{
    if (PyType_Ready(&MenaiInteger_Type) < 0) {
        return -1;
    }

    for (long v = MENAI_INT_CACHE_MIN; v <= MENAI_INT_CACHE_MAX; v++) {
        MenaiInteger_Object *obj = (MenaiInteger_Object *)MenaiInteger_Type.tp_alloc(&MenaiInteger_Type, 0);
        if (obj == NULL) {
            return -1;
        }
        obj->is_big = 0;
        obj->small = v;
        menai_int_init(&obj->big);
        _integer_cache[v - MENAI_INT_CACHE_MIN] = (PyObject *)obj;
    }
    return 0;
}
