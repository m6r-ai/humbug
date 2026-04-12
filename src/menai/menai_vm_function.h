/*
 * menai_vm_function.h — MenaiFunction type definition and API.
 *
 * MenaiFunction represents a Menai closure.  It holds parameters, bytecode,
 * captured values, and a frame-setup cache that eliminates PyObject_GetAttrString
 * calls from the hot call path.
 */

#ifndef MENAI_VM_FUNCTION_H
#define MENAI_VM_FUNCTION_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>

typedef struct {
    PyObject_HEAD
    PyObject *parameters;      /* Python tuple of str */
    PyObject *name;            /* Python str or Py_None */
    PyObject *bytecode;        /* CodeObject or Py_None */
    PyObject *captured_values; /* Python list of MenaiValue* */
    int is_variadic;           /* C int: 0 or 1 */
    int param_count;           /* C int: number of fixed parameters */

    /* Frame setup cache — populated once in MenaiFunction_new / menai_function_alloc
     * when bytecode is not None.  Eliminates all PyObject_GetAttrString calls
     * from the hot call_setup / frame_setup path.
     *
     * instrs_obj is a borrowed reference: bytecode (owned by this struct)
     * owns the array.array, so instrs_obj lives at least as long as we do.
     * constants, names, and closure_caches are likewise borrowed from bytecode.
     *
     * constants_items and names_items are raw pointers into the internal
     * ob_item arrays of the constants and names Python lists respectively.
     * They are valid for as long as constants/names are alive (i.e. for the
     * lifetime of this function object). */
    uint64_t *instrs;          /* raw pointer into bytecode.instructions buffer */
    PyObject *instrs_obj;      /* array.array — borrowed ref, keeps buffer valid */
    PyObject *constants;       /* borrowed ref to bytecode.constants list */
    PyObject **constants_items; /* raw pointer into constants ob_item array */
    PyObject *names;           /* borrowed ref to bytecode.names list */
    PyObject **names_items;    /* raw pointer into names ob_item array */
    PyObject *closure_caches;  /* borrowed ref to bytecode._code_caches list, or NULL */
    int code_len;              /* number of instructions */
    int local_count;           /* number of local variable slots */
} MenaiFunction_Object;

extern PyTypeObject MenaiFunction_Type;

/*
 * menai_function_alloc — direct C constructor for MenaiFunction.
 *
 * Bypasses PyObject_Call and argument parsing entirely.  All arguments are
 * borrowed.  See menai_vm_function.c for the cache tuple layout.
 */
PyObject *menai_function_alloc(PyObject *cache, PyObject *bytecode,
                               PyObject *captured_values);

/*
 * menai_function_new_from_kwargs — public wrapper around MenaiFunction_new,
 * used by menai_convert_value in menai_vm_value.c.  args and kwargs are
 * passed through to PyArg_ParseTupleAndKeywords unchanged.
 */
PyObject *menai_function_new_from_kwargs(PyObject *args, PyObject *kwargs);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_function_init(void);

#endif /* MENAI_VM_FUNCTION_H */
