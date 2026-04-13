/*
 * menai_vm_function.c — MenaiFunction type implementation.
 *
 * MenaiFunction represents a Menai closure.  It holds parameters, bytecode,
 * an inline C array of captured values, and a frame-setup cache that
 * eliminates PyObject_GetAttrString calls from the hot call path.
 *
 * Also provides menai_function_alloc(), the direct C constructor used by
 * OP_MAKE_CLOSURE in the VM, and menai_function_new_from_kwargs(), used by
 * menai_convert_value() in menai_vm_value.c.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>

#include "menai_vm_function.h"

/*
 * _cache_frame_fields — populate the frame-setup cache fields on self from
 * a bytecode CodeObject.  All refs are borrowed from bytecode (which self
 * owns).  Errors during GetAttrString are suppressed via PyErr_Clear so that
 * a missing optional attribute (e.g. _code_caches not yet set) does not fail
 * construction.
 */
static void
_cache_frame_fields(MenaiFunction_Object *self, PyObject *bytecode)
{
    PyObject *instrs_obj = PyObject_GetAttrString(bytecode, "instructions");
    if (instrs_obj) {
        Py_buffer view;
        if (PyObject_GetBuffer(instrs_obj, &view, PyBUF_SIMPLE) == 0) {
            self->instrs = (uint64_t *)view.buf;
            self->instrs_obj = instrs_obj;
            self->code_len = (int)(view.len / sizeof(uint64_t));
            PyBuffer_Release(&view);
        }
        /* Do not Py_DECREF — borrowed from bytecode */
    }
    PyObject *constants = PyObject_GetAttrString(bytecode, "constants");
    if (constants) {
        self->constants = constants;
        self->constants_items = ((PyListObject *)constants)->ob_item;
    }
    PyObject *names = PyObject_GetAttrString(bytecode, "names");
    if (names) {
        self->names = names;
        self->names_items = ((PyListObject *)names)->ob_item;
    }
    PyObject *lc = PyObject_GetAttrString(bytecode, "local_count");
    if (lc) {
        self->local_count = (int)PyLong_AsLong(lc);
        Py_DECREF(lc);
    }
    PyObject *pc = PyObject_GetAttrString(bytecode, "param_count");
    if (pc) {
        self->param_count = (int)PyLong_AsLong(pc);
        Py_DECREF(pc);
    }
    PyObject *cc = PyObject_GetAttrString(bytecode, "_code_caches");
    self->closure_caches = (cc && PyList_Check(cc)) ? cc : NULL;
    Py_XDECREF(cc);
    PyErr_Clear();
}

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

    /* Determine capture count from captured_values argument. */
    Py_ssize_t ncap = 0;
    if (captured_values && captured_values != Py_None) {
        ncap = PySequence_Size(captured_values);
        if (ncap < 0) {
            Py_DECREF(params_tup);
            return NULL;
        }
    }

    MenaiFunction_Object *self = (MenaiFunction_Object *)type->tp_alloc(type, ncap);
    if (!self) {
        Py_DECREF(params_tup);
        return NULL;
    }

    self->parameters = params_tup;
    Py_INCREF(name);
    self->name = name;
    Py_INCREF(bytecode);
    self->bytecode = bytecode;
    self->is_variadic = is_variadic;
    self->instrs = NULL;
    self->instrs_obj = NULL;
    self->constants = NULL;
    self->constants_items = NULL;
    self->names = NULL;
    self->names_items = NULL;
    self->code_len = 0;
    self->local_count = 0;
    self->closure_caches = NULL;
    self->param_count = 0;

    /* Populate capture slots. */
    if (captured_values && captured_values != Py_None) {
        for (Py_ssize_t i = 0; i < ncap; i++) {
            PyObject *cv = PySequence_GetItem(captured_values, i);
            if (!cv) {
                /* Partially initialised — zero remaining slots so dealloc is safe. */
                for (Py_ssize_t j = i; j < ncap; j++) self->captures[j] = NULL;
                Py_DECREF((PyObject *)self);
                return NULL;
            }
            self->captures[i] = cv;  /* owned */
        }
    } else {
        for (Py_ssize_t i = 0; i < ncap; i++) self->captures[i] = NULL;
    }

    if (bytecode != Py_None) {
        _cache_frame_fields(self, bytecode);
    } else {
        self->param_count = (int)PyTuple_GET_SIZE(params_tup);
    }

    return (PyObject *)self;
}

static void
MenaiFunction_dealloc(PyObject *self)
{
    PyObject_GC_UnTrack(self);
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    Py_XDECREF(f->parameters);
    Py_XDECREF(f->name);
    Py_XDECREF(f->bytecode);
    Py_ssize_t ncap = Py_SIZE(f);
    for (Py_ssize_t i = 0; i < ncap; i++) Py_XDECREF(f->captures[i]);
    Py_TYPE(self)->tp_free(self);
}

static int
MenaiFunction_traverse(PyObject *self, visitproc visit, void *arg)
{
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    Py_VISIT(f->parameters);
    Py_VISIT(f->name);
    Py_VISIT(f->bytecode);
    Py_ssize_t ncap = Py_SIZE(f);
    for (Py_ssize_t i = 0; i < ncap; i++) Py_VISIT(f->captures[i]);
    return 0;
}

static int
MenaiFunction_clear(PyObject *self)
{
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    Py_CLEAR(f->bytecode);
    f->instrs = NULL;
    f->instrs_obj = NULL;
    f->constants = NULL;
    f->constants_items = NULL;
    f->names = NULL;
    f->names_items = NULL;
    f->closure_caches = NULL;
    f->code_len = 0;
    Py_ssize_t ncap = Py_SIZE(f);
    for (Py_ssize_t i = 0; i < ncap; i++) Py_CLEAR(f->captures[i]);
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
    Py_INCREF(p);
    return p;
}

static PyObject *
MenaiFunction_get_name(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *n = ((MenaiFunction_Object *)self)->name;
    Py_INCREF(n);
    return n;
}

static PyObject *
MenaiFunction_get_bytecode(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *b = ((MenaiFunction_Object *)self)->bytecode;
    Py_INCREF(b);
    return b;
}

/*
 * captured_values getter — builds a Python list on demand from the inline
 * capture array.  This is only used by the Python-facing API (to_slow,
 * tests); the VM hot path reads captures[] directly.
 */
static PyObject *
MenaiFunction_get_captured_values(PyObject *self, void *closure)
{
    (void)closure;
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    Py_ssize_t ncap = Py_SIZE(f);
    PyObject *lst = PyList_New(ncap);
    if (!lst) return NULL;
    for (Py_ssize_t i = 0; i < ncap; i++) {
        PyObject *cv = f->captures[i] ? f->captures[i] : Py_None;
        Py_INCREF(cv);
        PyList_SET_ITEM(lst, i, cv);
    }
    return lst;
}

/*
 * captured_values setter — copies values from a list into the inline array.
 * The list must have exactly ob_size elements.  Used by to_slow's two-phase
 * cycle-safe pattern when converting fast→slow.
 */
static int
MenaiFunction_set_captured_values(PyObject *self, PyObject *value, void *closure)
{
    (void)closure;
    if (!PyList_Check(value)) {
        PyErr_SetString(PyExc_TypeError, "captured_values must be a list");
        return -1;
    }
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    Py_ssize_t ncap = Py_SIZE(f);
    if (PyList_GET_SIZE(value) != ncap) {
        PyErr_SetString(PyExc_ValueError,
                        "captured_values length does not match function capture count");
        return -1;
    }
    for (Py_ssize_t i = 0; i < ncap; i++) {
        PyObject *nv = PyList_GET_ITEM(value, i);
        Py_INCREF(nv);
        Py_XDECREF(f->captures[i]);
        f->captures[i] = nv;
    }
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

PyTypeObject MenaiFunction_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name        = "menai.menai_vm_value.MenaiFunction",
    .tp_basicsize   = sizeof(MenaiFunction_Object) - sizeof(PyObject *),
    .tp_itemsize    = sizeof(PyObject *),
    .tp_flags       = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC,
    .tp_new         = MenaiFunction_new,
    .tp_dealloc     = MenaiFunction_dealloc,
    .tp_traverse    = MenaiFunction_traverse,
    .tp_clear       = MenaiFunction_clear,
    .tp_methods     = MenaiFunction_methods,
    .tp_getset      = MenaiFunction_getset,
    .tp_richcompare = MenaiFunction_richcompare,
    .tp_hash        = MenaiFunction_hash,
};

/*
 * menai_function_alloc — direct C constructor for MenaiFunction.
 *
 * Allocates a function with ncap capture slots, all initialised to none_val
 * (the Menai_NONE singleton).  cache, bytecode, and none_val are all
 * borrowed; the function takes its own references.
 *
 * cache tuple layout:
 *   [0]  param_names_tup  (tuple of str)
 *   [1]  name             (str)
 *   [2]  is_variadic      (int: 0 or 1)
 *   [3]  ncap             (int — must equal the ncap argument)
 *   [4]  instrs_obj       (array.array)
 *   [5]  constants        (list)
 *   [6]  names_list       (list)
 *   [7]  param_count      (int)
 *   [8]  local_count      (int)
 *   [9]  child_code       (CodeObject)
 *  [10]  child _code_caches (list or None)
 *  [11]  instrs raw pointer (int via PyLong_FromVoidPtr)
 *  [12]  code_len         (int)
 */
PyObject *
menai_function_alloc(PyObject *cache, PyObject *bytecode,
                     Py_ssize_t ncap, PyObject *none_val)
{
    MenaiFunction_Object *self = (MenaiFunction_Object *)
        MenaiFunction_Type.tp_alloc(&MenaiFunction_Type, ncap);
    if (!self) return NULL;

    PyObject *parameters = PyTuple_GET_ITEM(cache, 0);
    PyObject *name = PyTuple_GET_ITEM(cache, 1);
    int is_variadic = (int)PyLong_AsLong(PyTuple_GET_ITEM(cache, 2));
    PyObject *instrs_obj = PyTuple_GET_ITEM(cache, 4);
    PyObject *constants = PyTuple_GET_ITEM(cache, 5);
    PyObject *names_obj = PyTuple_GET_ITEM(cache, 6);
    int param_count = (int)PyLong_AsLong(PyTuple_GET_ITEM(cache, 7));
    int local_count = (int)PyLong_AsLong(PyTuple_GET_ITEM(cache, 8));

    Py_INCREF(parameters);
    self->parameters = parameters;
    Py_INCREF(name);
    self->name = name;
    Py_INCREF(bytecode);
    self->bytecode = bytecode;
    self->is_variadic = is_variadic;
    self->param_count = param_count;
    self->local_count = local_count;
    self->constants = constants;
    self->constants_items = PyList_Check(constants) ? ((PyListObject *)constants)->ob_item : NULL;
    self->names = names_obj;
    self->names_items = PyList_Check(names_obj) ? ((PyListObject *)names_obj)->ob_item : NULL;
    PyObject *_cc = PyTuple_GET_ITEM(cache, 10);
    self->closure_caches = (_cc != Py_None && PyList_Check(_cc)) ? _cc : NULL;
    self->instrs = (uint64_t *)PyLong_AsVoidPtr(PyTuple_GET_ITEM(cache, 11));
    self->instrs_obj = instrs_obj;
    self->code_len = (int)PyLong_AsLong(PyTuple_GET_ITEM(cache, 12));

    for (Py_ssize_t i = 0; i < ncap; i++) {
        Py_INCREF(none_val);
        self->captures[i] = none_val;
    }

    return (PyObject *)self;
}

PyObject *
menai_function_new_from_kwargs(PyObject *args, PyObject *kwargs)
{
    return MenaiFunction_new(&MenaiFunction_Type, args, kwargs);
}

int
menai_vm_function_init(void)
{
    return PyType_Ready(&MenaiFunction_Type);
}
