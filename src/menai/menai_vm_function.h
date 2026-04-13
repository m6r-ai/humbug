/*
 * menai_vm_function.h — MenaiFunction type definition and API.
 *
 * MenaiFunction represents a Menai closure.  It holds parameters, bytecode,
 * an inline C array of captured values, and a frame-setup cache that
 * eliminates PyObject_GetAttrString calls from the hot call path.
 */

#ifndef MENAI_VM_FUNCTION_H
#define MENAI_VM_FUNCTION_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>

typedef struct {
    PyObject_VAR_HEAD              /* ob_size == number of captured values */
    PyObject *parameters;          /* Python tuple of str */
    PyObject *name;                /* Python str or Py_None */
    PyObject *bytecode;            /* CodeObject or Py_None */
    int is_variadic;               /* C int: 0 or 1 */
    int param_count;               /* C int: number of fixed parameters */

    /* Frame setup cache — populated once in MenaiFunction_new /
     * menai_function_alloc when bytecode is not None.  All borrowed from
     * bytecode (which we own), so they live as long as we do. */
    uint64_t *instrs;
    PyObject *instrs_obj;          /* array.array — borrowed ref, keeps buffer valid */
    PyObject *constants;           /* borrowed ref to bytecode.constants list */
    PyObject **constants_items;    /* raw pointer into constants ob_item array */
    PyObject *names;               /* borrowed ref to bytecode.names list */
    PyObject **names_items;        /* raw pointer into names ob_item array */
    PyObject *closure_caches;      /* borrowed ref to bytecode._code_caches, or NULL */
    int code_len;
    int local_count;

    /* Inline capture array — ob_size elements follow immediately. */
    PyObject *captures[1];         /* flexible array member (C99 [1] for MSVC compat) */
} MenaiFunction_Object;

extern PyTypeObject MenaiFunction_Type;

/*
 * menai_function_alloc — direct C constructor for MenaiFunction.
 *
 * Allocates a function with ncap capture slots, all initialised to
 * Menai_NONE.  cache, bytecode, and none_val are borrowed; the function
 * takes its own references.  See menai_vm_function.c for the cache tuple
 * layout.
 */
PyObject *menai_function_alloc(PyObject *cache, PyObject *bytecode,
                               Py_ssize_t ncap, PyObject *none_val);

/*
 * menai_function_new_from_kwargs — public wrapper around MenaiFunction_new,
 * used by menai_convert_value in menai_vm_value.c.
 */
PyObject *menai_function_new_from_kwargs(PyObject *args, PyObject *kwargs);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_function_init(void);

#endif /* MENAI_VM_FUNCTION_H */
