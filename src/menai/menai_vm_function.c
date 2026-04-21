/*
 * menai_vm_function.c — MenaiFunction type implementation.
 *
 * MenaiFunction represents a Menai closure.  It holds parameters, bytecode,
 * an inline C array of captured MenaiValues, and a frame-setup cache that
 * eliminates PyObject_GetAttrString calls from the hot call path.
 *
 * The parameters, name, bytecode, instrs_obj, constants, names, and
 * closure_caches fields remain as PyObject * because they originate from
 * the Python CodeObject layer.  The captures array holds MenaiValue —
 * live runtime values owned entirely by the C VM.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdlib.h>
#include <stdint.h>

#include "menai_vm_function.h"
#include "menai_vm_memory.h"

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
    self->closure_caches_items = self->closure_caches
        ? ((PyListObject *)self->closure_caches)->ob_item : NULL;
    Py_XDECREF(cc);
    PyErr_Clear();
}

static void
MenaiFunction_dealloc(PyObject *self)
{
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    Py_XDECREF(f->parameters);
    Py_XDECREF(f->name);
    Py_XDECREF(f->bytecode);
    Py_ssize_t ncap = f->ncap;
    for (Py_ssize_t i = 0; i < ncap; i++) menai_xrelease(f->captures[i]);
    free(self);
}

PyTypeObject MenaiFunction_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiFunction",          /* tp_name */
    sizeof(MenaiFunction_Object) - sizeof(MenaiValue),   /* tp_basicsize */
    0,                             /* tp_itemsize */
    MenaiFunction_dealloc,                  /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue
menai_function_alloc(const ClosureCache *cache, MenaiValue none_val)
{
    Py_ssize_t ncap = cache->ncap;
    MenaiFunction_Object *self = (MenaiFunction_Object *)malloc(
        sizeof(MenaiFunction_Object) + (size_t)ncap * sizeof(MenaiValue));
    if (!self) return NULL;

    self->ob_refcnt = 1;
    self->ob_type = &MenaiFunction_Type;
    self->ncap = ncap;

    Py_INCREF(cache->parameters);
    self->parameters = cache->parameters;
    Py_INCREF(cache->name);
    self->name = cache->name;
    Py_INCREF(cache->bytecode);
    self->bytecode = cache->bytecode;
    self->is_variadic = cache->is_variadic;
    self->param_count = cache->param_count;
    self->local_count = cache->local_count;
    self->constants = cache->constants;
    self->constants_items = cache->constants
        ? ((PyListObject *)cache->constants)->ob_item : NULL;
    self->names = cache->names_list;
    self->names_items = cache->names_list
        ? ((PyListObject *)cache->names_list)->ob_item : NULL;
    self->closure_caches = cache->closure_caches;
    self->closure_caches_items = cache->closure_caches_items;
    self->instrs = cache->instrs;
    self->instrs_obj = cache->instrs_obj;
    self->code_len = cache->code_len;

    for (Py_ssize_t i = 0; i < ncap; i++) {
        menai_retain(none_val);
        self->captures[i] = none_val;
    }

    return (MenaiValue)self;
}

MenaiValue
menai_function_alloc_from_slow(PyObject *parameters, PyObject *name,
                               PyObject *bytecode, MenaiValue *cap_items,
                               Py_ssize_t ncap, int is_variadic)
{
    PyObject *params_tup = PySequence_Tuple(parameters);
    if (!params_tup) return NULL;

    MenaiFunction_Object *self = (MenaiFunction_Object *)malloc(
        sizeof(MenaiFunction_Object) + (size_t)ncap * sizeof(MenaiValue));
    if (!self) {
        for (Py_ssize_t i = 0; i < ncap; i++) menai_xrelease(cap_items[i]);
        Py_DECREF(params_tup);
        return NULL;
    }

    self->ob_refcnt = 1;
    self->ob_type = &MenaiFunction_Type;
    self->ncap = ncap;
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
    self->closure_caches_items = NULL;
    self->param_count = 0;

    /* Steal the caller's references directly into the inline captures array. */
    for (Py_ssize_t i = 0; i < ncap; i++)
        self->captures[i] = cap_items[i];

    if (bytecode != Py_None) {
        _cache_frame_fields(self, bytecode);
    } else {
        self->param_count = (int)PyTuple_GET_SIZE(params_tup);
    }

    return (MenaiValue)self;
}

int
menai_vm_function_init(void)
{
    if (PyType_Ready(&MenaiFunction_Type) < 0) return -1;
    return 0;
}
