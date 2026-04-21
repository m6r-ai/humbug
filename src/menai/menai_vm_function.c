/*
 * menai_vm_function.c — MenaiFunction type implementation.
 *
 * MenaiFunction represents a Menai closure.  It holds parameters, bytecode,
 * an inline C array of captured MenaiValues, and a frame-setup cache that
 * eliminates PyObject_GetAttrString calls from the hot call path.
 *
 * The parameters, name, bytecode, and instrs_obj fields remain as PyObject *
 * because they originate from the Python CodeObject layer.  constants_items
 * is borrowed from the ClosureCache, which is pinned to the CodeObject for
 * the duration of execution.  The captures array holds MenaiValue — live
 * runtime values owned by the C VM.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdlib.h>
#include <stdint.h>

#include "menai_vm_function.h"
#include "menai_vm_memory.h"

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
    /* Read ob_item from the live Python list — by the time any MenaiFunction
     * is constructed from this cache, menai_convert_code_constants has already
     * replaced all slow constants with fast MenaiValues in-place. */
    self->constants_items = cache->constants
        ? (MenaiValue *)((PyListObject *)cache->constants)->ob_item : NULL;
    self->nconst = cache->constants ? PyList_GET_SIZE(cache->constants) : 0;
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

int
menai_vm_function_init(void)
{
    if (PyType_Ready(&MenaiFunction_Type) < 0) return -1;
    return 0;
}
