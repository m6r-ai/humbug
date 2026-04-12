/*
 * menai_vm_function.c — MenaiFunction type implementation.
 *
 * MenaiFunction represents a Menai closure.  It holds parameters, bytecode,
 * captured values, and a frame-setup cache that eliminates PyObject_GetAttrString
 * calls from the hot call path.
 *
 * Also provides menai_function_alloc(), the direct C constructor used by
 * OP_MAKE_CLOSURE in the VM, and menai_function_new_from_kwargs(), used by
 * menai_convert_value() in menai_vm_value.c.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>

#include "menai_vm_function.h"

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

    PyObject *cap_list = captured_values ? (Py_INCREF(captured_values), captured_values) : PyList_New(0);
    if (!cap_list) {
        Py_DECREF(params_tup);
        return NULL;
    }
    if (!PyList_Check(cap_list)) {
        PyObject *tmp = PySequence_List(cap_list);
        Py_DECREF(cap_list);
        cap_list = tmp;
        if (!cap_list) {
            Py_DECREF(params_tup);
            return NULL;
        }
    }

    MenaiFunction_Object *self = (MenaiFunction_Object *)type->tp_alloc(type, 0);
    if (self) {
        self->parameters = params_tup;
        Py_INCREF(name);
        self->name = name;
        Py_INCREF(bytecode);
        self->bytecode = bytecode;
        self->captured_values = cap_list;
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
        /* Cache param_count from the bytecode object to avoid repeated
         * PyObject_GetAttrString calls in call_setup on every function call. */
        if (bytecode != Py_None) {
            PyObject *pc = PyObject_GetAttrString(bytecode, "param_count");
            self->param_count = pc ? (int)PyLong_AsLong(pc) : 0;
            Py_XDECREF(pc);

            /* Populate the frame setup cache from the bytecode object.
             * All refs are borrowed from bytecode, which we own. */
            PyObject *instrs_obj = PyObject_GetAttrString(bytecode, "instructions");
            if (instrs_obj) {
                Py_buffer view;
                if (PyObject_GetBuffer(instrs_obj, &view, PyBUF_SIMPLE) == 0) {
                    self->instrs     = (uint64_t *)view.buf;
                    self->instrs_obj = instrs_obj;  /* borrowed — bytecode owns it */
                    self->code_len   = (int)(view.len / sizeof(uint64_t));
                    PyBuffer_Release(&view);
                }
                /* Do not Py_DECREF instrs_obj — it is borrowed from bytecode */
            }
            PyObject *constants = PyObject_GetAttrString(bytecode, "constants");
            if (constants) {
                self->constants = constants;  /* borrowed from bytecode */
                self->constants_items = ((PyListObject *)constants)->ob_item;
                /* Do not Py_DECREF — borrowed */
            }
            PyObject *names = PyObject_GetAttrString(bytecode, "names");
            if (names) {
                self->names = names;  /* borrowed from bytecode */
                self->names_items = ((PyListObject *)names)->ob_item;
                /* Do not Py_DECREF — borrowed */
            }
            PyObject *lc = PyObject_GetAttrString(bytecode, "local_count");
            if (lc) {
                self->local_count = (int)PyLong_AsLong(lc);
                Py_DECREF(lc);
            }
            PyObject *cc = PyObject_GetAttrString(bytecode, "_code_caches");
            self->closure_caches = (cc && PyList_Check(cc)) ? cc : NULL;
            Py_XDECREF(cc);  /* drop owned ref — bytecode (which we own) keeps it alive */
            PyErr_Clear();
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
    PyObject_GC_UnTrack(self);
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
    /* All of these are borrowed from bytecode — NULL them together so they
     * never dangle after bytecode is cleared. */
    f->instrs = NULL;
    f->instrs_obj = NULL;
    f->constants = NULL;
    f->constants_items = NULL;
    f->names = NULL;
    f->names_items = NULL;
    f->closure_caches = NULL;
    f->code_len = 0;
    Py_CLEAR(f->captured_values);
    return 0;
}

static PyObject *
MenaiFunction_type_name(PyObject *self, PyObject *args)
{
    (void)self;
    (void)args;
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

static PyObject *
MenaiFunction_get_captured_values(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *c = ((MenaiFunction_Object *)self)->captured_values;
    Py_INCREF(c);
    return c;
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

PyTypeObject MenaiFunction_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name        = "menai.menai_vm_value.MenaiFunction",
    .tp_basicsize   = sizeof(MenaiFunction_Object),
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
 * Bypasses PyObject_Call and argument parsing entirely.  All reference
 * counting: bytecode and captured_values are borrowed — we INCREF them.
 * All other fields are read from cache (borrowed from the tuple).
 *
 * cache is the _closure_cache tuple built by menai_convert_code_object:
 *   [0] param_names_tup  (tuple of str)
 *   [1] name             (str)
 *   [2] is_variadic      (int: 0 or 1)
 *   [3] ncap             (int: number of captures, unused here)
 *   [4] instrs_obj       (array.array — borrowed ref kept alive by bytecode)
 *   [5] constants        (list — borrowed ref kept alive by bytecode)
 *   [6] names_list       (list — borrowed ref kept alive by bytecode)
 *   [7] param_count      (int)
 *   [8] local_count      (int)
 *   [9] child_code       (CodeObject — used by OP_MAKE_CLOSURE, not here)
 *  [10] child _code_caches (list or None — borrowed ref kept alive by bytecode)
 *  [11] instrs raw pointer (int via PyLong_FromVoidPtr)
 *  [12] code_len          (int)
 *
 * bytecode is stored as an owned ref so it keeps instrs_obj/constants/names
 * alive for the lifetime of the function.
 */
PyObject *
menai_function_alloc(PyObject *cache, PyObject *bytecode, PyObject *captured_values)
{
    MenaiFunction_Object *self = (MenaiFunction_Object *)MenaiFunction_Type.tp_alloc(&MenaiFunction_Type, 0);
    if (!self)
        return NULL;

    PyObject *parameters = PyTuple_GET_ITEM(cache, 0);
    PyObject *name = PyTuple_GET_ITEM(cache, 1);
    int is_variadic = (int)PyLong_AsLong(PyTuple_GET_ITEM(cache, 2));
    /* cache[3] is ncap — used by the caller, not needed here */
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
    Py_INCREF(captured_values);
    self->captured_values = captured_values;
    self->is_variadic = is_variadic;
    self->param_count = param_count;
    self->local_count = local_count;
    self->constants = constants;  /* borrowed — bytecode keeps alive */
    self->constants_items = PyList_Check(constants) ? ((PyListObject *)constants)->ob_item : NULL;
    self->names = names_obj;  /* borrowed — bytecode keeps alive */
    self->names_items = PyList_Check(names_obj) ? ((PyListObject *)names_obj)->ob_item : NULL;
    PyObject *_cc = PyTuple_GET_ITEM(cache, 10);
    self->closure_caches = (_cc != Py_None && PyList_Check(_cc)) ? _cc : NULL;
    /* borrowed — bytecode (which we own) keeps child._code_caches alive */

    self->instrs = (uint64_t *)PyLong_AsVoidPtr(PyTuple_GET_ITEM(cache, 11));
    self->instrs_obj = instrs_obj;  /* borrowed — bytecode keeps alive */
    self->code_len = (int)PyLong_AsLong(PyTuple_GET_ITEM(cache, 12));

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
