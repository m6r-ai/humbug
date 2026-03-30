/*
 * menai_value_c.c — native C implementation of all Menai runtime value types.
 *
 * Replaces menai_value_fast.pyx.  Defines the same types (MenaiNone,
 * MenaiBoolean, MenaiInteger, MenaiFloat, MenaiComplex, MenaiString,
 * MenaiSymbol, MenaiList, MenaiDict, MenaiSet, MenaiFunction,
 * MenaiStructType, MenaiStruct) as Python extension types with known C
 * struct layouts, allowing the C VM to access fields by direct cast.
 *
 * Also provides:
 *   menai_convert_value()       — slow menai_value.py -> fast C type
 *   menai_convert_code_object() — walk CodeObject tree, convert constants
 *   menai_to_slow()             — fast C type -> slow menai_value.py
 *
 * Module name: menai.menai_value_c
 * Exported singletons: Menai_NONE, Menai_BOOLEAN_TRUE, Menai_BOOLEAN_FALSE,
 *                      Menai_LIST_EMPTY, Menai_DICT_EMPTY, Menai_SET_EMPTY
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stddef.h>
#include <string.h>

#include "menai_value_c.h"

/* ---------------------------------------------------------------------------
 * Forward declarations of type objects
 * ------------------------------------------------------------------------- */

static PyTypeObject MenaiNone_Type;
static PyTypeObject MenaiBoolean_Type;
static PyTypeObject MenaiInteger_Type;
static PyTypeObject MenaiFloat_Type;
static PyTypeObject MenaiComplex_Type;
static PyTypeObject MenaiString_Type;
static PyTypeObject MenaiSymbol_Type;
static PyTypeObject MenaiList_Type;
static PyTypeObject MenaiDict_Type;
static PyTypeObject MenaiSet_Type;
static PyTypeObject MenaiFunction_Type;
static PyTypeObject MenaiStructType_Type;
static PyTypeObject MenaiStruct_Type;

/* ---------------------------------------------------------------------------
 * Module-level singletons
 * ------------------------------------------------------------------------- */

static PyObject *_Menai_NONE        = NULL;
static PyObject *_Menai_TRUE        = NULL;
static PyObject *_Menai_FALSE       = NULL;
static PyObject *_Menai_EMPTY_LIST  = NULL;
static PyObject *_Menai_EMPTY_DICT  = NULL;
static PyObject *_Menai_EMPTY_SET   = NULL;

/* ---------------------------------------------------------------------------
 * Slow-world type objects — fetched once at module init
 * ------------------------------------------------------------------------- */

static PyTypeObject *Slow_NoneType       = NULL;
static PyTypeObject *Slow_BooleanType    = NULL;
static PyTypeObject *Slow_IntegerType    = NULL;
static PyTypeObject *Slow_FloatType      = NULL;
static PyTypeObject *Slow_ComplexType    = NULL;
static PyTypeObject *Slow_StringType     = NULL;
static PyTypeObject *Slow_SymbolType     = NULL;
static PyTypeObject *Slow_ListType       = NULL;
static PyTypeObject *Slow_DictType       = NULL;
static PyTypeObject *Slow_SetType        = NULL;
static PyTypeObject *Slow_FunctionType   = NULL;
static PyTypeObject *Slow_StructTypeType = NULL;
static PyTypeObject *Slow_StructType     = NULL;

/* Error type */
static PyObject *MenaiEvalError_type = NULL;

/* ---------------------------------------------------------------------------
 * Helpers
 * ------------------------------------------------------------------------- */

/* ---------------------------------------------------------------------------
 * MenaiNone
 * ------------------------------------------------------------------------- */

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

static PyObject *
MenaiNone_describe(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("#none");
}

static PyObject *
MenaiNone_richcompare(PyObject *self, PyObject *other, int op)
{
    if (op == Py_EQ)
        return PyBool_FromLong(Py_TYPE(other) == &MenaiNone_Type);
    if (op == Py_NE)
        return PyBool_FromLong(Py_TYPE(other) != &MenaiNone_Type);
    Py_RETURN_NOTIMPLEMENTED;
}

static Py_hash_t
MenaiNone_hash(PyObject *self)
{
    (void)self;
    return PyObject_Hash(Py_None);
}

static PyMethodDef MenaiNone_methods[] = {
    {"type_name", MenaiNone_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiNone_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

static PyTypeObject MenaiNone_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiNone",
    .tp_basicsize = sizeof(MenaiNone_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT,
    .tp_new       = MenaiNone_new,
    .tp_methods   = MenaiNone_methods,
    .tp_richcompare = MenaiNone_richcompare,
    .tp_hash      = MenaiNone_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiBoolean
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiBoolean_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    int value = 0;
    static char *kwlist[] = {"value", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "p", kwlist, &value))
        return NULL;
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

static PyObject *
MenaiBoolean_describe(PyObject *self, PyObject *args)
{
    (void)args;
    return PyUnicode_FromString(((MenaiBoolean_Object *)self)->value ? "#t" : "#f");
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
    return PyObject_Hash(((MenaiBoolean_Object *)self)->value ? Py_True : Py_False);
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
    {NULL, NULL, 0, NULL}
};

static PyTypeObject MenaiBoolean_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiBoolean",
    .tp_basicsize = sizeof(MenaiBoolean_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT,
    .tp_new       = MenaiBoolean_new,
    .tp_methods   = MenaiBoolean_methods,
    .tp_getset    = MenaiBoolean_getset,
    .tp_richcompare = MenaiBoolean_richcompare,
    .tp_hash      = MenaiBoolean_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiInteger
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiInteger_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *value = NULL;
    static char *kwlist[] = {"value", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O", kwlist, &value))
        return NULL;
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

static int
MenaiInteger_traverse(PyObject *self, visitproc visit, void *arg)
{
    Py_VISIT(((MenaiInteger_Object *)self)->value);
    return 0;
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

static PyTypeObject MenaiInteger_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiInteger",
    .tp_basicsize = sizeof(MenaiInteger_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
    .tp_new       = MenaiInteger_new,
    .tp_dealloc   = MenaiInteger_dealloc,
    .tp_traverse  = MenaiInteger_traverse,
    .tp_methods   = MenaiInteger_methods,
    .tp_getset    = MenaiInteger_getset,
    .tp_richcompare = MenaiInteger_richcompare,
    .tp_hash      = MenaiInteger_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiFloat
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiFloat_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    double value = 0.0;
    static char *kwlist[] = {"value", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "d", kwlist, &value))
        return NULL;
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
    PyObject *pf = PyFloat_FromDouble(((MenaiFloat_Object *)self)->value);
    if (!pf) return NULL;
    PyObject *s = PyObject_Str(pf);
    Py_DECREF(pf);
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
        case Py_LT: return PyBool_FromLong(a <  b);
        case Py_LE: return PyBool_FromLong(a <= b);
        case Py_GT: return PyBool_FromLong(a >  b);
        case Py_GE: return PyBool_FromLong(a >= b);
        default:    Py_RETURN_NOTIMPLEMENTED;
    }
}

static Py_hash_t
MenaiFloat_hash(PyObject *self)
{
    PyObject *pf = PyFloat_FromDouble(((MenaiFloat_Object *)self)->value);
    if (!pf) return -1;
    Py_hash_t h = PyObject_Hash(pf);
    Py_DECREF(pf);
    return h;
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
    {"describe",  MenaiFloat_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

static PyTypeObject MenaiFloat_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiFloat",
    .tp_basicsize = sizeof(MenaiFloat_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT,
    .tp_new       = MenaiFloat_new,
    .tp_methods   = MenaiFloat_methods,
    .tp_getset    = MenaiFloat_getset,
    .tp_richcompare = MenaiFloat_richcompare,
    .tp_hash      = MenaiFloat_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiComplex
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiComplex_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *value = NULL;
    static char *kwlist[] = {"value", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "O", kwlist, &value))
        return NULL;
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

static int
MenaiComplex_traverse(PyObject *self, visitproc visit, void *arg)
{
    Py_VISIT(((MenaiComplex_Object *)self)->value);
    return 0;
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
    /* Delegate to the Python describe() logic via the slow type's method.
     * This is only called for display, not in the hot loop. */
    PyObject *cv = ((MenaiComplex_Object *)self)->value;

    /* Format matching menai_value.py MenaiComplex.describe() */
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

static PyTypeObject MenaiComplex_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiComplex",
    .tp_basicsize = sizeof(MenaiComplex_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
    .tp_new       = MenaiComplex_new,
    .tp_dealloc   = MenaiComplex_dealloc,
    .tp_traverse  = MenaiComplex_traverse,
    .tp_methods   = MenaiComplex_methods,
    .tp_getset    = MenaiComplex_getset,
    .tp_richcompare = MenaiComplex_richcompare,
    .tp_hash      = MenaiComplex_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiString
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiString_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *value = NULL;
    static char *kwlist[] = {"value", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "U", kwlist, &value))
        return NULL;
    MenaiString_Object *self = (MenaiString_Object *)type->tp_alloc(type, 0);
    if (self) { Py_INCREF(value); self->value = value; }
    return (PyObject *)self;
}

static void
MenaiString_dealloc(PyObject *self)
{
    Py_XDECREF(((MenaiString_Object *)self)->value);
    Py_TYPE(self)->tp_free(self);
}

static int
MenaiString_traverse(PyObject *self, visitproc visit, void *arg)
{
    Py_VISIT(((MenaiString_Object *)self)->value);
    return 0;
}

static PyObject *
MenaiString_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("string");
}

static PyObject *
MenaiString_describe(PyObject *self, PyObject *args)
{
    (void)args;
    /* Delegate to slow type for the escape logic */
    PyObject *mod = PyImport_ImportModule("menai.menai_value");
    if (!mod) return NULL;
    PyObject *cls = PyObject_GetAttrString(mod, "MenaiString");
    Py_DECREF(mod);
    if (!cls) return NULL;
    PyObject *sv = ((MenaiString_Object *)self)->value;
    PyObject *inst = PyObject_CallOneArg(cls, sv);
    Py_DECREF(cls);
    if (!inst) return NULL;
    PyObject *result = PyObject_CallMethod(inst, "describe", NULL);
    Py_DECREF(inst);
    return result;
}

static PyObject *
MenaiString_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiString_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    return PyUnicode_RichCompare(
        ((MenaiString_Object *)self)->value,
        ((MenaiString_Object *)other)->value, op);
}

static Py_hash_t
MenaiString_hash(PyObject *self)
{
    return PyObject_Hash(((MenaiString_Object *)self)->value);
}

static PyObject *
MenaiString_get_value(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *v = ((MenaiString_Object *)self)->value;
    Py_INCREF(v);
    return v;
}

static PyGetSetDef MenaiString_getset[] = {
    {"value", MenaiString_get_value, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiString_methods[] = {
    {"type_name", MenaiString_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiString_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

static PyTypeObject MenaiString_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiString",
    .tp_basicsize = sizeof(MenaiString_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
    .tp_new       = MenaiString_new,
    .tp_dealloc   = MenaiString_dealloc,
    .tp_traverse  = MenaiString_traverse,
    .tp_methods   = MenaiString_methods,
    .tp_getset    = MenaiString_getset,
    .tp_richcompare = MenaiString_richcompare,
    .tp_hash      = MenaiString_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiSymbol
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiSymbol_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *name = NULL;
    static char *kwlist[] = {"name", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "U", kwlist, &name))
        return NULL;
    MenaiSymbol_Object *self = (MenaiSymbol_Object *)type->tp_alloc(type, 0);
    if (self) { Py_INCREF(name); self->name = name; }
    return (PyObject *)self;
}

static void
MenaiSymbol_dealloc(PyObject *self)
{
    Py_XDECREF(((MenaiSymbol_Object *)self)->name);
    Py_TYPE(self)->tp_free(self);
}

static int
MenaiSymbol_traverse(PyObject *self, visitproc visit, void *arg)
{
    Py_VISIT(((MenaiSymbol_Object *)self)->name);
    return 0;
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
    return PyUnicode_RichCompare(
        ((MenaiSymbol_Object *)self)->name,
        ((MenaiSymbol_Object *)other)->name, op);
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

static PyTypeObject MenaiSymbol_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiSymbol",
    .tp_basicsize = sizeof(MenaiSymbol_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
    .tp_new       = MenaiSymbol_new,
    .tp_dealloc   = MenaiSymbol_dealloc,
    .tp_traverse  = MenaiSymbol_traverse,
    .tp_methods   = MenaiSymbol_methods,
    .tp_getset    = MenaiSymbol_getset,
    .tp_richcompare = MenaiSymbol_richcompare,
    .tp_hash      = MenaiSymbol_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiList
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiList_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *elements = NULL;
    static char *kwlist[] = {"elements", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "|O", kwlist, &elements))
        return NULL;

    PyObject *tup;
    if (elements == NULL) {
        tup = PyTuple_New(0);
    } else {
        tup = PySequence_Tuple(elements);
    }
    if (!tup) return NULL;

    MenaiList_Object *self = (MenaiList_Object *)type->tp_alloc(type, 0);
    if (self) {
        self->elements = tup;
    } else {
        Py_DECREF(tup);
    }
    return (PyObject *)self;
}

static void
MenaiList_dealloc(PyObject *self)
{
    Py_XDECREF(((MenaiList_Object *)self)->elements);
    Py_TYPE(self)->tp_free(self);
}

static int
MenaiList_traverse(PyObject *self, visitproc visit, void *arg)
{
    Py_VISIT(((MenaiList_Object *)self)->elements);
    return 0;
}

static PyObject *
MenaiList_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("list");
}

static PyObject *
MenaiList_describe(PyObject *self, PyObject *args)
{
    (void)args;
    PyObject *elems = ((MenaiList_Object *)self)->elements;
    Py_ssize_t n = PyTuple_GET_SIZE(elems);
    if (n == 0)
        return PyUnicode_FromString("()");

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *desc = PyObject_CallMethod(PyTuple_GET_ITEM(elems, i), "describe", NULL);
        if (!desc) { Py_DECREF(parts); return NULL; }
        PyList_SET_ITEM(parts, i, desc);
    }
    PyObject *sep = PyUnicode_FromString(" ");
    PyObject *joined = PyUnicode_Join(sep, parts);
    Py_DECREF(sep); Py_DECREF(parts);
    if (!joined) return NULL;
    PyObject *result = PyUnicode_FromFormat("(%U)", joined);
    Py_DECREF(joined);
    return result;
}

static PyObject *
MenaiList_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiList_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    return PyObject_RichCompare(
        ((MenaiList_Object *)self)->elements,
        ((MenaiList_Object *)other)->elements, op);
}

static Py_hash_t
MenaiList_hash(PyObject *self)
{
    return PyObject_Hash(((MenaiList_Object *)self)->elements);
}

static PyObject *
MenaiList_get_elements(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *e = ((MenaiList_Object *)self)->elements;
    Py_INCREF(e);
    return e;
}

static PyGetSetDef MenaiList_getset[] = {
    {"elements", MenaiList_get_elements, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiList_methods[] = {
    {"type_name", MenaiList_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiList_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

static PyTypeObject MenaiList_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiList",
    .tp_basicsize = sizeof(MenaiList_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
    .tp_new       = MenaiList_new,
    .tp_dealloc   = MenaiList_dealloc,
    .tp_traverse  = MenaiList_traverse,
    .tp_methods   = MenaiList_methods,
    .tp_getset    = MenaiList_getset,
    .tp_richcompare = MenaiList_richcompare,
    .tp_hash      = MenaiList_hash,
};

/* ---------------------------------------------------------------------------
 * _hashable_key — shared by MenaiDict and MenaiSet
 *
 * Converts a MenaiValue key to a hashable Python tuple (tag, value).
 * Returns a new reference, or NULL on error (MenaiEvalError set).
 * ------------------------------------------------------------------------- */

static PyObject *
_hashable_key(PyObject *key)
{
    PyTypeObject *t = Py_TYPE(key);

    if (t == &MenaiString_Type)
        return Py_BuildValue("(sO)", "str", ((MenaiString_Object *)key)->value);
    if (t == &MenaiInteger_Type)
        return Py_BuildValue("(sO)", "int", ((MenaiInteger_Object *)key)->value);
    if (t == &MenaiFloat_Type) {
        PyObject *pf = PyFloat_FromDouble(((MenaiFloat_Object *)key)->value);
        if (!pf) return NULL;
        PyObject *r = Py_BuildValue("(sO)", "flt", pf);
        Py_DECREF(pf);
        return r;
    }
    if (t == &MenaiComplex_Type)
        return Py_BuildValue("(sO)", "cplx", ((MenaiComplex_Object *)key)->value);
    if (t == &MenaiBoolean_Type) {
        PyObject *bv = PyBool_FromLong(((MenaiBoolean_Object *)key)->value);
        PyObject *r = Py_BuildValue("(sO)", "bool", bv);
        Py_DECREF(bv);
        return r;
    }
    if (t == &MenaiSymbol_Type)
        return Py_BuildValue("(sO)", "sym", ((MenaiSymbol_Object *)key)->name);
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
    if (!lookup) { Py_DECREF(pairs); return NULL; }

    Py_ssize_t n = PyTuple_GET_SIZE(pairs);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *pair = PyTuple_GET_ITEM(pairs, i);
        PyObject *k = PyTuple_GET_ITEM(pair, 0);
        PyObject *hk = _hashable_key(k);
        if (!hk) { Py_DECREF(lookup); Py_DECREF(pairs); return NULL; }
        if (PyDict_SetItem(lookup, hk, pair) < 0) {
            Py_DECREF(hk); Py_DECREF(lookup); Py_DECREF(pairs); return NULL;
        }
        Py_DECREF(hk);
    }

    MenaiDict_Object *self = (MenaiDict_Object *)type->tp_alloc(type, 0);
    if (self) {
        self->pairs  = pairs;
        self->lookup = lookup;
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

static int
MenaiDict_traverse(PyObject *self, visitproc visit, void *arg)
{
    Py_VISIT(((MenaiDict_Object *)self)->pairs);
    Py_VISIT(((MenaiDict_Object *)self)->lookup);
    return 0;
}

static PyObject *
MenaiDict_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
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
        if (!vd) { Py_XDECREF(kd); Py_DECREF(parts); return NULL; }
        PyObject *entry = PyUnicode_FromFormat("(%U %U)", kd, vd);
        Py_DECREF(kd); Py_DECREF(vd);
        if (!entry) { Py_DECREF(parts); return NULL; }
        PyList_SET_ITEM(parts, i, entry);
    }
    PyObject *sep = PyUnicode_FromString(" ");
    PyObject *joined = PyUnicode_Join(sep, parts);
    Py_DECREF(sep); Py_DECREF(parts);
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
    return _hashable_key(key);
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

static PyTypeObject MenaiDict_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiDict",
    .tp_basicsize = sizeof(MenaiDict_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
    .tp_new       = MenaiDict_new,
    .tp_dealloc   = MenaiDict_dealloc,
    .tp_traverse  = MenaiDict_traverse,
    .tp_methods   = MenaiDict_methods,
    .tp_getset    = MenaiDict_getset,
    .tp_richcompare = MenaiDict_richcompare,
    .tp_hash      = MenaiDict_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiSet
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiSet_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *elements_arg = NULL;
    static char *kwlist[] = {"elements", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "|O", kwlist, &elements_arg))
        return NULL;

    PyObject *src_tup;
    if (elements_arg == NULL) {
        src_tup = PyTuple_New(0);
    } else {
        src_tup = PySequence_Tuple(elements_arg);
    }
    if (!src_tup) return NULL;

    /* Deduplicate, preserving order */
    PyObject *seen = PySet_New(NULL);
    if (!seen) { Py_DECREF(src_tup); return NULL; }
    PyObject *deduped = PyList_New(0);
    if (!deduped) { Py_DECREF(seen); Py_DECREF(src_tup); return NULL; }

    Py_ssize_t n = PyTuple_GET_SIZE(src_tup);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *elem = PyTuple_GET_ITEM(src_tup, i);
        PyObject *hk = _hashable_key(elem);
        if (!hk) { Py_DECREF(deduped); Py_DECREF(seen); Py_DECREF(src_tup); return NULL; }
        int has = PySet_Contains(seen, hk);
        if (has < 0) { Py_DECREF(hk); Py_DECREF(deduped); Py_DECREF(seen); Py_DECREF(src_tup); return NULL; }
        if (!has) {
            if (PySet_Add(seen, hk) < 0 || PyList_Append(deduped, elem) < 0) {
                Py_DECREF(hk); Py_DECREF(deduped); Py_DECREF(seen); Py_DECREF(src_tup); return NULL;
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
    if (!members_set) { Py_DECREF(elements); return NULL; }
    n = PyTuple_GET_SIZE(elements);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *hk = _hashable_key(PyTuple_GET_ITEM(elements, i));
        if (!hk) { Py_DECREF(members_set); Py_DECREF(elements); return NULL; }
        if (PySet_Add(members_set, hk) < 0) {
            Py_DECREF(hk); Py_DECREF(members_set); Py_DECREF(elements); return NULL;
        }
        Py_DECREF(hk);
    }
    PyObject *members = PyFrozenSet_New(members_set);
    Py_DECREF(members_set);
    if (!members) { Py_DECREF(elements); return NULL; }

    MenaiSet_Object *self = (MenaiSet_Object *)type->tp_alloc(type, 0);
    if (self) {
        self->elements = elements;
        self->members  = members;
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

static int
MenaiSet_traverse(PyObject *self, visitproc visit, void *arg)
{
    Py_VISIT(((MenaiSet_Object *)self)->elements);
    Py_VISIT(((MenaiSet_Object *)self)->members);
    return 0;
}

static PyObject *
MenaiSet_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("set");
}

static PyObject *
MenaiSet_describe(PyObject *self, PyObject *args)
{
    (void)args;
    PyObject *elems = ((MenaiSet_Object *)self)->elements;
    Py_ssize_t n = PyTuple_GET_SIZE(elems);
    if (n == 0)
        return PyUnicode_FromString("#{}");

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *desc = PyObject_CallMethod(PyTuple_GET_ITEM(elems, i), "describe", NULL);
        if (!desc) { Py_DECREF(parts); return NULL; }
        PyList_SET_ITEM(parts, i, desc);
    }
    PyObject *sep = PyUnicode_FromString(" ");
    PyObject *joined = PyUnicode_Join(sep, parts);
    Py_DECREF(sep); Py_DECREF(parts);
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
    {"members",  MenaiSet_get_members,  NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiSet_methods[] = {
    {"type_name", MenaiSet_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiSet_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

static PyTypeObject MenaiSet_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiSet",
    .tp_basicsize = sizeof(MenaiSet_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
    .tp_new       = MenaiSet_new,
    .tp_dealloc   = MenaiSet_dealloc,
    .tp_traverse  = MenaiSet_traverse,
    .tp_methods   = MenaiSet_methods,
    .tp_getset    = MenaiSet_getset,
    .tp_richcompare = MenaiSet_richcompare,
    .tp_hash      = MenaiSet_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiFunction
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiFunction_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *parameters = NULL, *name = Py_None, *bytecode = Py_None;
    PyObject *captured_values = NULL;
    int is_variadic = 0;
    static char *kwlist[] = {"parameters", "name", "bytecode",
                             "captured_values", "is_variadic", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "|OOOOp", kwlist,
                                     &parameters, &name, &bytecode,
                                     &captured_values, &is_variadic))
        return NULL;

    PyObject *params_tup = parameters ? PySequence_Tuple(parameters) : PyTuple_New(0);
    if (!params_tup) return NULL;

    PyObject *cap_list = captured_values
        ? (Py_INCREF(captured_values), captured_values)
        : PyList_New(0);
    if (!cap_list) { Py_DECREF(params_tup); return NULL; }
    if (!PyList_Check(cap_list)) {
        PyObject *tmp = PySequence_List(cap_list);
        Py_DECREF(cap_list);
        cap_list = tmp;
        if (!cap_list) { Py_DECREF(params_tup); return NULL; }
    }

    MenaiFunction_Object *self = (MenaiFunction_Object *)type->tp_alloc(type, 0);
    if (self) {
        self->parameters      = params_tup;
        Py_INCREF(name);     self->name             = name;
        Py_INCREF(bytecode); self->bytecode          = bytecode;
        self->captured_values = cap_list;
        self->is_variadic     = is_variadic;
        /* Cache param_count from the bytecode object to avoid repeated
         * PyObject_GetAttrString calls in call_setup on every function call. */
        if (bytecode != Py_None) {
            PyObject *pc = PyObject_GetAttrString(bytecode, "param_count");
            self->param_count = pc ? (int)PyLong_AsLong(pc) : 0;
            Py_XDECREF(pc);
        } else {
            self->param_count = (int)PyTuple_GET_SIZE(params_tup);
        }
    } else {
        Py_DECREF(params_tup);
        Py_DECREF(cap_list);
    }
    return (PyObject *)self;
}

static void
MenaiFunction_dealloc(PyObject *self)
{
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    Py_XDECREF(f->parameters);
    Py_XDECREF(f->name);
    Py_XDECREF(f->bytecode);
    Py_XDECREF(f->captured_values);
    Py_TYPE(self)->tp_free(self);
}

static int
MenaiFunction_traverse(PyObject *self, visitproc visit, void *arg)
{
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    Py_VISIT(f->parameters);
    Py_VISIT(f->name);
    Py_VISIT(f->bytecode);
    Py_VISIT(f->captured_values);
    return 0;
}

static int
MenaiFunction_clear(PyObject *self)
{
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    Py_CLEAR(f->bytecode);
    Py_CLEAR(f->captured_values);
    return 0;
}

static PyObject *
MenaiFunction_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("function");
}

static PyObject *
MenaiFunction_describe(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    PyObject *sep = PyUnicode_FromString(", ");
    PyObject *joined = PyUnicode_Join(sep, f->parameters);
    Py_DECREF(sep);
    if (!joined) return NULL;
    PyObject *result = PyUnicode_FromFormat("<lambda (%U)>", joined);
    Py_DECREF(joined);
    return result;
}

static PyObject *
MenaiFunction_richcompare(PyObject *self, PyObject *other, int op)
{
    if (op == Py_EQ) return PyBool_FromLong(self == other);
    if (op == Py_NE) return PyBool_FromLong(self != other);
    Py_RETURN_NOTIMPLEMENTED;
}

static Py_hash_t
MenaiFunction_hash(PyObject *self)
{
    return (Py_hash_t)self;
}

static PyObject *
MenaiFunction_get_parameters(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *p = ((MenaiFunction_Object *)self)->parameters;
    Py_INCREF(p); return p;
}

static PyObject *
MenaiFunction_get_name(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *n = ((MenaiFunction_Object *)self)->name;
    Py_INCREF(n); return n;
}

static PyObject *
MenaiFunction_get_bytecode(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *b = ((MenaiFunction_Object *)self)->bytecode;
    Py_INCREF(b); return b;
}

static PyObject *
MenaiFunction_get_captured_values(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *c = ((MenaiFunction_Object *)self)->captured_values;
    Py_INCREF(c); return c;
}

static int
MenaiFunction_set_captured_values(PyObject *self, PyObject *value, void *closure)
{
    (void)closure;
    if (!PyList_Check(value)) {
        PyErr_SetString(PyExc_TypeError, "captured_values must be a list");
        return -1;
    }
    PyObject *old = ((MenaiFunction_Object *)self)->captured_values;
    Py_INCREF(value);
    ((MenaiFunction_Object *)self)->captured_values = value;
    Py_XDECREF(old);
    return 0;
}

static PyObject *
MenaiFunction_get_is_variadic(PyObject *self, void *closure)
{
    (void)closure;
    return PyBool_FromLong(((MenaiFunction_Object *)self)->is_variadic);
}

static PyObject *
MenaiFunction_get_param_count(PyObject *self, void *closure)
{
    (void)closure;
    return PyLong_FromLong(((MenaiFunction_Object *)self)->param_count);
}

static PyGetSetDef MenaiFunction_getset[] = {
    {"parameters",      MenaiFunction_get_parameters,      NULL,                              NULL, NULL},
    {"name",            MenaiFunction_get_name,             NULL,                              NULL, NULL},
    {"bytecode",        MenaiFunction_get_bytecode,         NULL,                              NULL, NULL},
    {"captured_values", MenaiFunction_get_captured_values,  MenaiFunction_set_captured_values, NULL, NULL},
    {"is_variadic",     MenaiFunction_get_is_variadic,      NULL,                              NULL, NULL},
    {"param_count",     MenaiFunction_get_param_count,      NULL,                              NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiFunction_methods[] = {
    {"type_name", MenaiFunction_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiFunction_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

static PyTypeObject MenaiFunction_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiFunction",
    .tp_basicsize = sizeof(MenaiFunction_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
    .tp_new       = MenaiFunction_new,
    .tp_dealloc   = MenaiFunction_dealloc,
    .tp_traverse  = MenaiFunction_traverse,
    .tp_clear     = MenaiFunction_clear,
    .tp_methods   = MenaiFunction_methods,
    .tp_getset    = MenaiFunction_getset,
    .tp_richcompare = MenaiFunction_richcompare,
    .tp_hash      = MenaiFunction_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiStructType
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiStructType_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *name = NULL, *field_names = NULL;
    int tag = 0;
    static char *kwlist[] = {"name", "tag", "field_names", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "UiO", kwlist,
                                     &name, &tag, &field_names))
        return NULL;

    PyObject *fn_tup = PySequence_Tuple(field_names);
    if (!fn_tup) return NULL;

    /* Build _field_index dict */
    PyObject *fi = PyDict_New();
    if (!fi) { Py_DECREF(fn_tup); return NULL; }
    Py_ssize_t n = PyTuple_GET_SIZE(fn_tup);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *idx = PyLong_FromSsize_t(i);
        if (!idx || PyDict_SetItem(fi, PyTuple_GET_ITEM(fn_tup, i), idx) < 0) {
            Py_XDECREF(idx); Py_DECREF(fi); Py_DECREF(fn_tup); return NULL;
        }
        Py_DECREF(idx);
    }

    MenaiStructType_Object *self = (MenaiStructType_Object *)type->tp_alloc(type, 0);
    if (self) {
        Py_INCREF(name); self->name        = name;
        self->tag         = tag;
        self->field_names = fn_tup;
        self->_field_index = fi;
    } else {
        Py_DECREF(fn_tup);
        Py_DECREF(fi);
    }
    return (PyObject *)self;
}

static void
MenaiStructType_dealloc(PyObject *self)
{
    MenaiStructType_Object *s = (MenaiStructType_Object *)self;
    Py_XDECREF(s->name);
    Py_XDECREF(s->field_names);
    Py_XDECREF(s->_field_index);
    Py_TYPE(self)->tp_free(self);
}

static int
MenaiStructType_traverse(PyObject *self, visitproc visit, void *arg)
{
    MenaiStructType_Object *s = (MenaiStructType_Object *)self;
    Py_VISIT(s->name);
    Py_VISIT(s->field_names);
    Py_VISIT(s->_field_index);
    return 0;
}

static PyObject *
MenaiStructType_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("struct-type");
}

static PyObject *
MenaiStructType_describe(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiStructType_Object *s = (MenaiStructType_Object *)self;
    PyObject *sep = PyUnicode_FromString(" ");
    PyObject *fields_str = PyUnicode_Join(sep, s->field_names);
    Py_DECREF(sep);
    if (!fields_str) return NULL;
    PyObject *result = PyUnicode_FromFormat("<struct-type %U (%U)>", s->name, fields_str);
    Py_DECREF(fields_str);
    return result;
}

static PyObject *
MenaiStructType_field_index(PyObject *self, PyObject *name)
{
    PyObject *idx = PyDict_GetItem(((MenaiStructType_Object *)self)->_field_index, name);
    if (!idx) {
        PyErr_SetObject(PyExc_KeyError, name);
        return NULL;
    }
    Py_INCREF(idx);
    return idx;
}

static PyObject *
MenaiStructType_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiStructType_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    int a = ((MenaiStructType_Object *)self)->tag;
    int b = ((MenaiStructType_Object *)other)->tag;
    switch (op) {
        case Py_EQ: return PyBool_FromLong(a == b);
        case Py_NE: return PyBool_FromLong(a != b);
        default:    Py_RETURN_NOTIMPLEMENTED;
    }
}

static Py_hash_t
MenaiStructType_hash(PyObject *self)
{
    return (Py_hash_t)((MenaiStructType_Object *)self)->tag;
}

static PyObject *
MenaiStructType_get_name(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *n = ((MenaiStructType_Object *)self)->name;
    Py_INCREF(n); return n;
}

static PyObject *
MenaiStructType_get_tag(PyObject *self, void *closure)
{
    (void)closure;
    return PyLong_FromLong(((MenaiStructType_Object *)self)->tag);
}

static PyObject *
MenaiStructType_get_field_names(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *fn = ((MenaiStructType_Object *)self)->field_names;
    Py_INCREF(fn); return fn;
}

static PyGetSetDef MenaiStructType_getset[] = {
    {"name",        MenaiStructType_get_name,        NULL, NULL, NULL},
    {"tag",         MenaiStructType_get_tag,          NULL, NULL, NULL},
    {"field_names", MenaiStructType_get_field_names,  NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiStructType_methods[] = {
    {"type_name",   MenaiStructType_type_name,  METH_NOARGS, NULL},
    {"describe",    MenaiStructType_describe,   METH_NOARGS, NULL},
    {"field_index", MenaiStructType_field_index, METH_O,     NULL},
    {NULL, NULL, 0, NULL}
};

static PyTypeObject MenaiStructType_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiStructType",
    .tp_basicsize = sizeof(MenaiStructType_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
    .tp_new       = MenaiStructType_new,
    .tp_dealloc   = MenaiStructType_dealloc,
    .tp_traverse  = MenaiStructType_traverse,
    .tp_methods   = MenaiStructType_methods,
    .tp_getset    = MenaiStructType_getset,
    .tp_richcompare = MenaiStructType_richcompare,
    .tp_hash      = MenaiStructType_hash,
};

/* ---------------------------------------------------------------------------
 * MenaiStruct
 * ------------------------------------------------------------------------- */

static PyObject *
MenaiStruct_new(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
    PyObject *struct_type = NULL, *fields = NULL;
    static char *kwlist[] = {"struct_type", "fields", NULL};
    if (!PyArg_ParseTupleAndKeywords(args, kwargs, "OO", kwlist,
                                     &struct_type, &fields))
        return NULL;

    PyObject *fields_tup = PySequence_Tuple(fields);
    if (!fields_tup) return NULL;

    MenaiStruct_Object *self = (MenaiStruct_Object *)type->tp_alloc(type, 0);
    if (self) {
        Py_INCREF(struct_type); self->struct_type = struct_type;
        self->fields = fields_tup;
    } else {
        Py_DECREF(fields_tup);
    }
    return (PyObject *)self;
}

static void
MenaiStruct_dealloc(PyObject *self)
{
    Py_XDECREF(((MenaiStruct_Object *)self)->struct_type);
    Py_XDECREF(((MenaiStruct_Object *)self)->fields);
    Py_TYPE(self)->tp_free(self);
}

static int
MenaiStruct_traverse(PyObject *self, visitproc visit, void *arg)
{
    Py_VISIT(((MenaiStruct_Object *)self)->struct_type);
    Py_VISIT(((MenaiStruct_Object *)self)->fields);
    return 0;
}

static PyObject *
MenaiStruct_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("struct");
}

static PyObject *
MenaiStruct_describe(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiStruct_Object *s = (MenaiStruct_Object *)self;
    MenaiStructType_Object *st = (MenaiStructType_Object *)s->struct_type;
    Py_ssize_t n = PyTuple_GET_SIZE(s->fields);

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *desc = PyObject_CallMethod(PyTuple_GET_ITEM(s->fields, i), "describe", NULL);
        if (!desc) { Py_DECREF(parts); return NULL; }
        PyList_SET_ITEM(parts, i, desc);
    }
    PyObject *sep = PyUnicode_FromString(" ");
    PyObject *joined = PyUnicode_Join(sep, parts);
    Py_DECREF(sep); Py_DECREF(parts);
    if (!joined) return NULL;

    PyObject *result;
    if (n == 0)
        result = PyUnicode_FromFormat("(%U)", st->name);
    else
        result = PyUnicode_FromFormat("(%U %U)", st->name, joined);
    Py_DECREF(joined);
    return result;
}

static PyObject *
MenaiStruct_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiStruct_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    MenaiStruct_Object *a = (MenaiStruct_Object *)self;
    MenaiStruct_Object *b = (MenaiStruct_Object *)other;
    int tag_eq = (((MenaiStructType_Object *)a->struct_type)->tag ==
                  ((MenaiStructType_Object *)b->struct_type)->tag);
    if (op == Py_EQ) {
        if (!tag_eq) Py_RETURN_FALSE;
        return PyObject_RichCompare(a->fields, b->fields, Py_EQ);
    }
    if (op == Py_NE) {
        if (!tag_eq) Py_RETURN_TRUE;
        return PyObject_RichCompare(a->fields, b->fields, Py_NE);
    }
    Py_RETURN_NOTIMPLEMENTED;
}

static Py_hash_t
MenaiStruct_hash(PyObject *self)
{
    MenaiStruct_Object *s = (MenaiStruct_Object *)self;
    Py_hash_t fh = PyObject_Hash(s->fields);
    if (fh == -1) return -1;
    int tag = ((MenaiStructType_Object *)s->struct_type)->tag;
    PyObject *pair = Py_BuildValue("(iN)", tag, PyLong_FromSsize_t(fh));
    if (!pair) return -1;
    Py_hash_t h = PyObject_Hash(pair);
    Py_DECREF(pair);
    return h;
}

static PyObject *
MenaiStruct_get_struct_type(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *st = ((MenaiStruct_Object *)self)->struct_type;
    Py_INCREF(st); return st;
}

static PyObject *
MenaiStruct_get_fields(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *f = ((MenaiStruct_Object *)self)->fields;
    Py_INCREF(f); return f;
}

static PyGetSetDef MenaiStruct_getset[] = {
    {"struct_type", MenaiStruct_get_struct_type, NULL, NULL, NULL},
    {"fields",      MenaiStruct_get_fields,      NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiStruct_methods[] = {
    {"type_name", MenaiStruct_type_name, METH_NOARGS, NULL},
    {"describe",  MenaiStruct_describe,  METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

static PyTypeObject MenaiStruct_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "menai.menai_value_c.MenaiStruct",
    .tp_basicsize = sizeof(MenaiStruct_Object),
    .tp_flags     = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
    .tp_new       = MenaiStruct_new,
    .tp_dealloc   = MenaiStruct_dealloc,
    .tp_traverse  = MenaiStruct_traverse,
    .tp_methods   = MenaiStruct_methods,
    .tp_getset    = MenaiStruct_getset,
    .tp_richcompare = MenaiStruct_richcompare,
    .tp_hash      = MenaiStruct_hash,
};

/* ===========================================================================
 * Conversion functions
 * =========================================================================*/

/*
 * _is_fast — return 1 if obj is already a fast C value type.
 */
static int
_is_fast(PyObject *obj)
{
    PyTypeObject *t = Py_TYPE(obj);
    return (t == &MenaiNone_Type     || t == &MenaiBoolean_Type  ||
            t == &MenaiInteger_Type  || t == &MenaiFloat_Type    ||
            t == &MenaiComplex_Type  || t == &MenaiString_Type   ||
            t == &MenaiSymbol_Type   || t == &MenaiList_Type     ||
            t == &MenaiDict_Type     || t == &MenaiSet_Type      ||
            t == &MenaiFunction_Type || t == &MenaiStructType_Type ||
            t == &MenaiStruct_Type);
}

/*
 * menai_convert_value — convert one slow menai_value.py object to a fast type.
 *
 * Returns a new reference.  If src is already a fast type, returns it
 * with an incremented refcount.  For MenaiFunction, captured_values are
 * NOT recursively converted here — call_setup in the VM does that lazily
 * at call time to avoid cycles in letrec closures.
 */
PyObject *
menai_convert_value(PyObject *src)
{
    if (_is_fast(src)) {
        Py_INCREF(src);
        return src;
    }

    PyTypeObject *t = Py_TYPE(src);

    if (t == Slow_NoneType)
        return _Menai_NONE ? (Py_INCREF(_Menai_NONE), _Menai_NONE)
                           : PyObject_CallNoArgs((PyObject *)&MenaiNone_Type);

    if (t == Slow_BooleanType) {
        PyObject *bv = PyObject_GetAttrString(src, "value");
        if (!bv) return NULL;
        int b = PyObject_IsTrue(bv);
        Py_DECREF(bv);
        if (b < 0) return NULL;
        PyObject *r = b ? _Menai_TRUE : _Menai_FALSE;
        Py_INCREF(r); return r;
    }

    if (t == Slow_IntegerType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
        PyObject *args = PyTuple_Pack(1, v);
        Py_DECREF(v);
        if (!args) return NULL;
        PyObject *r = MenaiInteger_new(&MenaiInteger_Type, args, NULL);
        Py_DECREF(args);
        return r;
    }

    if (t == Slow_FloatType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
        double d = PyFloat_AsDouble(v);
        Py_DECREF(v);
        if (d == -1.0 && PyErr_Occurred()) return NULL;
        MenaiFloat_Object *r = (MenaiFloat_Object *)MenaiFloat_Type.tp_alloc(&MenaiFloat_Type, 0);
        if (r) r->value = d;
        return (PyObject *)r;
    }

    if (t == Slow_ComplexType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
        PyObject *args = PyTuple_Pack(1, v);
        Py_DECREF(v);
        if (!args) return NULL;
        PyObject *r = MenaiComplex_new(&MenaiComplex_Type, args, NULL);
        Py_DECREF(args);
        return r;
    }

    if (t == Slow_StringType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
        PyObject *args = PyTuple_Pack(1, v);
        Py_DECREF(v);
        if (!args) return NULL;
        PyObject *r = MenaiString_new(&MenaiString_Type, args, NULL);
        Py_DECREF(args);
        return r;
    }

    if (t == Slow_SymbolType) {
        PyObject *n = PyObject_GetAttrString(src, "name");
        if (!n) return NULL;
        PyObject *args = PyTuple_Pack(1, n);
        Py_DECREF(n);
        if (!args) return NULL;
        PyObject *r = MenaiSymbol_new(&MenaiSymbol_Type, args, NULL);
        Py_DECREF(args);
        return r;
    }

    if (t == Slow_ListType) {
        PyObject *elems = PyObject_GetAttrString(src, "elements");
        if (!elems) return NULL;
        Py_ssize_t n = PyTuple_GET_SIZE(elems);
        PyObject *fast_tup = PyTuple_New(n);
        if (!fast_tup) { Py_DECREF(elems); return NULL; }
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *fe = menai_convert_value(PyTuple_GET_ITEM(elems, i));
            if (!fe) { Py_DECREF(fast_tup); Py_DECREF(elems); return NULL; }
            PyTuple_SET_ITEM(fast_tup, i, fe);
        }
        Py_DECREF(elems);
        PyObject *args = PyTuple_Pack(1, fast_tup);
        Py_DECREF(fast_tup);
        if (!args) return NULL;
        PyObject *r = MenaiList_new(&MenaiList_Type, args, NULL);
        Py_DECREF(args);
        return r;
    }

    if (t == Slow_DictType) {
        PyObject *pairs = PyObject_GetAttrString(src, "pairs");
        if (!pairs) return NULL;
        Py_ssize_t n = PyTuple_GET_SIZE(pairs);
        PyObject *fast_pairs = PyTuple_New(n);
        if (!fast_pairs) { Py_DECREF(pairs); return NULL; }
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *pair = PyTuple_GET_ITEM(pairs, i);
            PyObject *fk = menai_convert_value(PyTuple_GET_ITEM(pair, 0));
            if (!fk) { Py_DECREF(fast_pairs); Py_DECREF(pairs); return NULL; }
            PyObject *fv = menai_convert_value(PyTuple_GET_ITEM(pair, 1));
            if (!fv) { Py_DECREF(fk); Py_DECREF(fast_pairs); Py_DECREF(pairs); return NULL; }
            PyObject *fp = PyTuple_Pack(2, fk, fv);
            Py_DECREF(fk); Py_DECREF(fv);
            if (!fp) { Py_DECREF(fast_pairs); Py_DECREF(pairs); return NULL; }
            PyTuple_SET_ITEM(fast_pairs, i, fp);
        }
        Py_DECREF(pairs);
        PyObject *args = PyTuple_Pack(1, fast_pairs);
        Py_DECREF(fast_pairs);
        if (!args) return NULL;
        PyObject *r = MenaiDict_new(&MenaiDict_Type, args, NULL);
        Py_DECREF(args);
        return r;
    }

    if (t == Slow_SetType) {
        PyObject *elems = PyObject_GetAttrString(src, "elements");
        if (!elems) return NULL;
        Py_ssize_t n = PyTuple_GET_SIZE(elems);
        PyObject *fast_tup = PyTuple_New(n);
        if (!fast_tup) { Py_DECREF(elems); return NULL; }
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *fe = menai_convert_value(PyTuple_GET_ITEM(elems, i));
            if (!fe) { Py_DECREF(fast_tup); Py_DECREF(elems); return NULL; }
            PyTuple_SET_ITEM(fast_tup, i, fe);
        }
        Py_DECREF(elems);
        PyObject *args = PyTuple_Pack(1, fast_tup);
        Py_DECREF(fast_tup);
        if (!args) return NULL;
        PyObject *r = MenaiSet_new(&MenaiSet_Type, args, NULL);
        Py_DECREF(args);
        return r;
    }

    if (t == Slow_StructTypeType) {
        PyObject *name = PyObject_GetAttrString(src, "name");
        PyObject *tag  = PyObject_GetAttrString(src, "tag");
        PyObject *fn   = PyObject_GetAttrString(src, "field_names");
        if (!name || !tag || !fn) {
            Py_XDECREF(name); Py_XDECREF(tag); Py_XDECREF(fn); return NULL;
        }
        PyObject *args = PyTuple_Pack(3, name, tag, fn);
        Py_DECREF(name); Py_DECREF(tag); Py_DECREF(fn);
        if (!args) return NULL;
        PyObject *r = MenaiStructType_new(&MenaiStructType_Type, args, NULL);
        Py_DECREF(args);
        return r;
    }

    if (t == Slow_StructType) {
        PyObject *st     = PyObject_GetAttrString(src, "struct_type");
        PyObject *fields = PyObject_GetAttrString(src, "fields");
        if (!st || !fields) { Py_XDECREF(st); Py_XDECREF(fields); return NULL; }
        PyObject *fast_st = menai_convert_value(st);
        Py_DECREF(st);
        if (!fast_st) { Py_DECREF(fields); return NULL; }
        Py_ssize_t n = PyTuple_GET_SIZE(fields);
        PyObject *fast_fields = PyTuple_New(n);
        if (!fast_fields) { Py_DECREF(fast_st); Py_DECREF(fields); return NULL; }
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *ff = menai_convert_value(PyTuple_GET_ITEM(fields, i));
            if (!ff) { Py_DECREF(fast_fields); Py_DECREF(fast_st); Py_DECREF(fields); return NULL; }
            PyTuple_SET_ITEM(fast_fields, i, ff);
        }
        Py_DECREF(fields);
        PyObject *kwargs = Py_BuildValue("{sOsO}", "struct_type", fast_st, "fields", fast_fields);
        Py_DECREF(fast_st); Py_DECREF(fast_fields);
        if (!kwargs) return NULL;
        PyObject *empty = PyTuple_New(0);
        if (!empty) { Py_DECREF(kwargs); return NULL; }
        PyObject *r = MenaiStruct_new(&MenaiStruct_Type, empty, kwargs);
        Py_DECREF(empty); Py_DECREF(kwargs);
        return r;
    }

    if (t == Slow_FunctionType) {
        PyObject *params  = PyObject_GetAttrString(src, "parameters");
        PyObject *name    = PyObject_GetAttrString(src, "name");
        PyObject *bc      = PyObject_GetAttrString(src, "bytecode");
        PyObject *cap     = PyObject_GetAttrString(src, "captured_values");
        PyObject *is_var  = PyObject_GetAttrString(src, "is_variadic");
        if (!params || !name || !bc || !cap || !is_var) {
            Py_XDECREF(params); Py_XDECREF(name); Py_XDECREF(bc);
            Py_XDECREF(cap); Py_XDECREF(is_var); return NULL;
        }
        /* Copy captured_values as a plain list — not recursively converted
         * here; call_setup converts lazily to handle letrec cycles. */
        PyObject *cap_list = PySequence_List(cap);
        Py_DECREF(cap);
        if (!cap_list) {
            Py_DECREF(params); Py_DECREF(name); Py_DECREF(bc); Py_DECREF(is_var);
            return NULL;
        }
        int iv = PyObject_IsTrue(is_var);
        Py_DECREF(is_var);
        if (iv < 0) {
            Py_DECREF(params); Py_DECREF(name); Py_DECREF(bc); Py_DECREF(cap_list);
            return NULL;
        }
        PyObject *kwargs = Py_BuildValue("{sOsOsOsOsi}",
            "parameters",      params,
            "name",            name,
            "bytecode",        bc,
            "captured_values", cap_list,
            "is_variadic",     iv);
        Py_DECREF(params); Py_DECREF(name); Py_DECREF(bc); Py_DECREF(cap_list);
        if (!kwargs) return NULL;
        PyObject *empty = PyTuple_New(0);
        if (!empty) { Py_DECREF(kwargs); return NULL; }
        PyObject *r = MenaiFunction_new(&MenaiFunction_Type, empty, kwargs);
        Py_DECREF(empty); Py_DECREF(kwargs);
        return r;
    }

    PyErr_Format(PyExc_TypeError, "menai_convert_value: unexpected type %R", (PyObject *)t);
    return NULL;
}

/*
 * menai_convert_code_object — walk a CodeObject tree, converting all
 * constants lists in-place from slow to fast types.
 *
 * Returns the code object (borrowed reference), or NULL on error.
 */
PyObject *
menai_convert_code_object(PyObject *code)
{
    /* Convert code.constants list in-place */
    PyObject *constants = PyObject_GetAttrString(code, "constants");
    if (!constants) return NULL;
    Py_ssize_t n = PyList_GET_SIZE(constants);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *orig = PyList_GET_ITEM(constants, i);
        PyObject *fast = menai_convert_value(orig);
        if (!fast) { Py_DECREF(constants); return NULL; }
        PyList_SET_ITEM(constants, i, fast);
        Py_DECREF(orig);
    }
    Py_DECREF(constants);

    /* Recurse into any zero-capture MenaiFunction constants */
    constants = PyObject_GetAttrString(code, "constants");
    if (!constants) return NULL;
    n = PyList_GET_SIZE(constants);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *c = PyList_GET_ITEM(constants, i);
        if (Py_TYPE(c) == &MenaiFunction_Type) {
            PyObject *bc = ((MenaiFunction_Object *)c)->bytecode;
            if (bc && bc != Py_None) {
                if (!menai_convert_code_object(bc)) { Py_DECREF(constants); return NULL; }
            }
        }
    }
    Py_DECREF(constants);

    /* Recurse into child code objects */
    PyObject *children = PyObject_GetAttrString(code, "code_objects");
    if (!children) return NULL;
    n = PyList_GET_SIZE(children);
    for (Py_ssize_t i = 0; i < n; i++) {
        if (!menai_convert_code_object(PyList_GET_ITEM(children, i))) {
            Py_DECREF(children); return NULL;
        }
    }
    Py_DECREF(children);
    return code;
}

/*
 * _to_slow_memo — cycle-safe implementation of menai_to_slow.
 */
static PyObject *
_to_slow_memo(PyObject *src, PyObject *memo);

static PyObject *
_to_slow_memo(PyObject *src, PyObject *memo)
{
    /* Already slow — pass through */
    if (!_is_fast(src)) {
        Py_INCREF(src);
        return src;
    }

    PyObject *key = PyLong_FromVoidPtr(src);
    if (!key) return NULL;
    PyObject *cached = PyDict_GetItem(memo, key);
    if (cached) { Py_DECREF(key); Py_INCREF(cached); return cached; }

    PyTypeObject *t = Py_TYPE(src);
    PyObject *mod = PyImport_ImportModule("menai.menai_value");
    if (!mod) { Py_DECREF(key); return NULL; }

    PyObject *result = NULL;

#define GET_SLOW_CLS(name) PyObject_GetAttrString(mod, name)

    if (t == &MenaiNone_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiNone");
        if (cls) { result = PyObject_CallNoArgs(cls); Py_DECREF(cls); }
    }
    else if (t == &MenaiBoolean_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiBoolean");
        if (cls) {
            PyObject *bv = PyBool_FromLong(((MenaiBoolean_Object *)src)->value);
            result = PyObject_CallOneArg(cls, bv);
            Py_DECREF(bv); Py_DECREF(cls);
        }
    }
    else if (t == &MenaiInteger_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiInteger");
        if (cls) {
            result = PyObject_CallOneArg(cls, ((MenaiInteger_Object *)src)->value);
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiFloat_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiFloat");
        if (cls) {
            PyObject *pf = PyFloat_FromDouble(((MenaiFloat_Object *)src)->value);
            result = pf ? PyObject_CallOneArg(cls, pf) : NULL;
            Py_XDECREF(pf); Py_DECREF(cls);
        }
    }
    else if (t == &MenaiComplex_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiComplex");
        if (cls) {
            result = PyObject_CallOneArg(cls, ((MenaiComplex_Object *)src)->value);
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiString_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiString");
        if (cls) {
            result = PyObject_CallOneArg(cls, ((MenaiString_Object *)src)->value);
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiSymbol_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiSymbol");
        if (cls) {
            result = PyObject_CallOneArg(cls, ((MenaiSymbol_Object *)src)->name);
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiList_Type) {
        /* Register placeholder before recursing */
        if (PyDict_SetItem(memo, key, Py_None) < 0) goto done;
        PyObject *elems = ((MenaiList_Object *)src)->elements;
        Py_ssize_t n = PyTuple_GET_SIZE(elems);
        PyObject *slow_tup = PyTuple_New(n);
        if (!slow_tup) goto done;
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *se = _to_slow_memo(PyTuple_GET_ITEM(elems, i), memo);
            if (!se) { Py_DECREF(slow_tup); goto done; }
            PyTuple_SET_ITEM(slow_tup, i, se);
        }
        PyObject *cls = GET_SLOW_CLS("MenaiList");
        if (cls) { result = PyObject_CallOneArg(cls, slow_tup); Py_DECREF(cls); }
        Py_DECREF(slow_tup);
        if (result) PyDict_SetItem(memo, key, result);
    }
    else if (t == &MenaiDict_Type) {
        if (PyDict_SetItem(memo, key, Py_None) < 0) goto done;
        PyObject *pairs = ((MenaiDict_Object *)src)->pairs;
        Py_ssize_t n = PyTuple_GET_SIZE(pairs);
        PyObject *slow_pairs = PyTuple_New(n);
        if (!slow_pairs) goto done;
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *pair = PyTuple_GET_ITEM(pairs, i);
            PyObject *sk = _to_slow_memo(PyTuple_GET_ITEM(pair, 0), memo);
            if (!sk) { Py_DECREF(slow_pairs); goto done; }
            PyObject *sv = _to_slow_memo(PyTuple_GET_ITEM(pair, 1), memo);
            if (!sv) { Py_DECREF(sk); Py_DECREF(slow_pairs); goto done; }
            PyObject *sp = PyTuple_Pack(2, sk, sv);
            Py_DECREF(sk); Py_DECREF(sv);
            if (!sp) { Py_DECREF(slow_pairs); goto done; }
            PyTuple_SET_ITEM(slow_pairs, i, sp);
        }
        PyObject *cls = GET_SLOW_CLS("MenaiDict");
        if (cls) { result = PyObject_CallOneArg(cls, slow_pairs); Py_DECREF(cls); }
        Py_DECREF(slow_pairs);
        if (result) PyDict_SetItem(memo, key, result);
    }
    else if (t == &MenaiSet_Type) {
        if (PyDict_SetItem(memo, key, Py_None) < 0) goto done;
        PyObject *elems = ((MenaiSet_Object *)src)->elements;
        Py_ssize_t n = PyTuple_GET_SIZE(elems);
        PyObject *slow_tup = PyTuple_New(n);
        if (!slow_tup) goto done;
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *se = _to_slow_memo(PyTuple_GET_ITEM(elems, i), memo);
            if (!se) { Py_DECREF(slow_tup); goto done; }
            PyTuple_SET_ITEM(slow_tup, i, se);
        }
        PyObject *cls = GET_SLOW_CLS("MenaiSet");
        if (cls) { result = PyObject_CallOneArg(cls, slow_tup); Py_DECREF(cls); }
        Py_DECREF(slow_tup);
        if (result) PyDict_SetItem(memo, key, result);
    }
    else if (t == &MenaiStructType_Type) {
        MenaiStructType_Object *st = (MenaiStructType_Object *)src;
        PyObject *cls = GET_SLOW_CLS("MenaiStructType");
        if (cls) {
            PyObject *tag = PyLong_FromLong(st->tag);
            result = tag ? PyObject_CallFunctionObjArgs(cls, st->name, tag, st->field_names, NULL) : NULL;
            Py_XDECREF(tag); Py_DECREF(cls);
        }
    }
    else if (t == &MenaiStruct_Type) {
        if (PyDict_SetItem(memo, key, Py_None) < 0) goto done;
        MenaiStruct_Object *s = (MenaiStruct_Object *)src;
        PyObject *slow_st = _to_slow_memo(s->struct_type, memo);
        if (!slow_st) goto done;
        Py_ssize_t n = PyTuple_GET_SIZE(s->fields);
        PyObject *slow_fields = PyTuple_New(n);
        if (!slow_fields) { Py_DECREF(slow_st); goto done; }
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *sf = _to_slow_memo(PyTuple_GET_ITEM(s->fields, i), memo);
            if (!sf) { Py_DECREF(slow_fields); Py_DECREF(slow_st); goto done; }
            PyTuple_SET_ITEM(slow_fields, i, sf);
        }
        PyObject *cls = GET_SLOW_CLS("MenaiStruct");
        if (cls) {
            PyObject *kwargs = Py_BuildValue("{sOsO}", "struct_type", slow_st, "fields", slow_fields);
            if (kwargs) {
                PyObject *empty = PyTuple_New(0);
                if (empty) { result = PyObject_Call(cls, empty, kwargs); Py_DECREF(empty); }
                Py_DECREF(kwargs);
            }
            Py_DECREF(cls);
        }
        Py_DECREF(slow_st); Py_DECREF(slow_fields);
        if (result) PyDict_SetItem(memo, key, result);
    }
    else if (t == &MenaiFunction_Type) {
        MenaiFunction_Object *f = (MenaiFunction_Object *)src;
        PyObject *cls = GET_SLOW_CLS("MenaiFunction");
        if (cls) {
            /* Two-phase: create with empty captures, register, then fill */
            PyObject *empty_list = PyList_New(0);
            if (empty_list) {
                PyObject *kwargs = Py_BuildValue("{sOsOsOsOsi}",
                    "parameters",      f->parameters,
                    "name",            f->name,
                    "bytecode",        f->bytecode,
                    "captured_values", empty_list,
                    "is_variadic",     f->is_variadic);
                Py_DECREF(empty_list);
                if (kwargs) {
                    PyObject *empty_args = PyTuple_New(0);
                    if (empty_args) {
                        result = PyObject_Call(cls, empty_args, kwargs);
                        Py_DECREF(empty_args);
                    }
                    Py_DECREF(kwargs);
                }
            }
            Py_DECREF(cls);
        }
        if (result) {
            if (PyDict_SetItem(memo, key, result) < 0) { Py_DECREF(result); result = NULL; goto done; }
            /* Now fill captured_values */
            Py_ssize_t n = PyList_GET_SIZE(f->captured_values);
            PyObject *slow_caps = PyList_New(n);
            if (!slow_caps) { Py_DECREF(result); result = NULL; goto done; }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *sc = _to_slow_memo(PyList_GET_ITEM(f->captured_values, i), memo);
                if (!sc) { Py_DECREF(slow_caps); Py_DECREF(result); result = NULL; goto done; }
                PyList_SET_ITEM(slow_caps, i, sc);
            }
            if (PyObject_SetAttrString(result, "captured_values", slow_caps) < 0) {
                Py_DECREF(slow_caps); Py_DECREF(result); result = NULL; goto done;
            }
            Py_DECREF(slow_caps);
        }
    }

done:
    Py_DECREF(mod);
    Py_DECREF(key);
    return result;

#undef GET_SLOW_CLS
}

PyObject *
menai_to_slow(PyObject *src)
{
    PyObject *memo = PyDict_New();
    if (!memo) return NULL;
    PyObject *result = _to_slow_memo(src, memo);
    Py_DECREF(memo);
    return result;
}

/* ===========================================================================
 * Python-callable wrappers (exposed on the module for the C VM's shim init)
 * =========================================================================*/

static PyObject *
py_convert_value(PyObject *self, PyObject *arg)
{
    (void)self;
    return menai_convert_value(arg);
}

static PyObject *
py_convert_code_object(PyObject *self, PyObject *arg)
{
    (void)self;
    PyObject *r = menai_convert_code_object(arg);
    if (!r) return NULL;
    Py_INCREF(r);
    return r;
}

static PyObject *
py_to_slow(PyObject *self, PyObject *arg)
{
    (void)self;
    return menai_to_slow(arg);
}

/* ===========================================================================
 * Module init
 * =========================================================================*/

static int
fetch_slow_type(PyObject *mod, const char *name, PyTypeObject **dst)
{
    PyObject *obj = PyObject_GetAttrString(mod, name);
    if (!obj) return -1;
    *dst = (PyTypeObject *)obj;
    return 0;
}

static PyMethodDef module_methods[] = {
    {"convert_value",       py_convert_value,       METH_O, NULL},
    {"convert_code_object", py_convert_code_object, METH_O, NULL},
    {"to_slow",             py_to_slow,             METH_O, NULL},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef module_def = {
    PyModuleDef_HEAD_INIT,
    "menai.menai_value_c",
    NULL,
    -1,
    module_methods
};

PyMODINIT_FUNC
PyInit_menai_value_c(void)
{
    /* Fetch slow-world types */
    PyObject *slow_mod = PyImport_ImportModule("menai.menai_value");
    if (!slow_mod) return NULL;

    if (fetch_slow_type(slow_mod, "MenaiNone",       &Slow_NoneType)       < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiBoolean",    &Slow_BooleanType)    < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiInteger",    &Slow_IntegerType)    < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiFloat",      &Slow_FloatType)      < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiComplex",    &Slow_ComplexType)    < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiString",     &Slow_StringType)     < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiSymbol",     &Slow_SymbolType)     < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiList",       &Slow_ListType)       < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiDict",       &Slow_DictType)       < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiSet",        &Slow_SetType)        < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiFunction",   &Slow_FunctionType)   < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiStructType", &Slow_StructTypeType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiStruct",     &Slow_StructType)     < 0) goto fail;
    Py_DECREF(slow_mod);
    slow_mod = NULL;

    /* Fetch MenaiEvalError */
    PyObject *err_mod = PyImport_ImportModule("menai.menai_error");
    if (!err_mod) return NULL;
    MenaiEvalError_type = PyObject_GetAttrString(err_mod, "MenaiEvalError");
    Py_DECREF(err_mod);
    if (!MenaiEvalError_type) return NULL;

    /* Ready all types */
    PyTypeObject *types[] = {
        &MenaiNone_Type, &MenaiBoolean_Type, &MenaiInteger_Type,
        &MenaiFloat_Type, &MenaiComplex_Type, &MenaiString_Type,
        &MenaiSymbol_Type, &MenaiList_Type, &MenaiDict_Type,
        &MenaiSet_Type, &MenaiFunction_Type, &MenaiStructType_Type,
        &MenaiStruct_Type
    };
    for (int i = 0; i < (int)(sizeof(types)/sizeof(types[0])); i++) {
        if (PyType_Ready(types[i]) < 0) return NULL;
    }

    /* Create module */
    PyObject *module = PyModule_Create(&module_def);
    if (!module) return NULL;

    /* Add types */
    const char *type_names[] = {
        "MenaiNone", "MenaiBoolean", "MenaiInteger", "MenaiFloat",
        "MenaiComplex", "MenaiString", "MenaiSymbol", "MenaiList",
        "MenaiDict", "MenaiSet", "MenaiFunction", "MenaiStructType",
        "MenaiStruct"
    };
    for (int i = 0; i < (int)(sizeof(types)/sizeof(types[0])); i++) {
        Py_INCREF(types[i]);
        if (PyModule_AddObject(module, type_names[i], (PyObject *)types[i]) < 0) {
            Py_DECREF(types[i]); Py_DECREF(module); return NULL;
        }
    }

    /* Create singletons */
    _Menai_NONE = PyObject_CallNoArgs((PyObject *)&MenaiNone_Type);
    if (!_Menai_NONE) { Py_DECREF(module); return NULL; }

    PyObject *true_args  = Py_BuildValue("(i)", 1);
    PyObject *false_args = Py_BuildValue("(i)", 0);
    _Menai_TRUE  = true_args  ? MenaiBoolean_new(&MenaiBoolean_Type, true_args,  NULL) : NULL;
    _Menai_FALSE = false_args ? MenaiBoolean_new(&MenaiBoolean_Type, false_args, NULL) : NULL;
    Py_XDECREF(true_args); Py_XDECREF(false_args);
    if (!_Menai_TRUE || !_Menai_FALSE) { Py_DECREF(module); return NULL; }

    PyObject *empty_tup = PyTuple_New(0);
    if (!empty_tup) { Py_DECREF(module); return NULL; }
    _Menai_EMPTY_LIST = MenaiList_new(&MenaiList_Type, empty_tup, NULL);
    _Menai_EMPTY_DICT = MenaiDict_new(&MenaiDict_Type, empty_tup, NULL);
    _Menai_EMPTY_SET  = MenaiSet_new(&MenaiSet_Type,  empty_tup, NULL);
    Py_DECREF(empty_tup);
    if (!_Menai_EMPTY_LIST || !_Menai_EMPTY_DICT || !_Menai_EMPTY_SET) {
        Py_DECREF(module); return NULL;
    }

    /* Add singletons to module */
    struct { const char *name; PyObject **obj; } singletons[] = {
        {"Menai_NONE",          &_Menai_NONE},
        {"Menai_BOOLEAN_TRUE",  &_Menai_TRUE},
        {"Menai_BOOLEAN_FALSE", &_Menai_FALSE},
        {"Menai_LIST_EMPTY",    &_Menai_EMPTY_LIST},
        {"Menai_DICT_EMPTY",    &_Menai_EMPTY_DICT},
        {"Menai_SET_EMPTY",     &_Menai_EMPTY_SET},
    };
    for (int i = 0; i < (int)(sizeof(singletons)/sizeof(singletons[0])); i++) {
        Py_INCREF(*singletons[i].obj);
        if (PyModule_AddObject(module, singletons[i].name, *singletons[i].obj) < 0) {
            Py_DECREF(*singletons[i].obj); Py_DECREF(module); return NULL;
        }
    }

    return module;

fail:
    Py_XDECREF(slow_mod);
    return NULL;
}
