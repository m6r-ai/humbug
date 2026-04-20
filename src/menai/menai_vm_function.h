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

/*
 * ClosureCache — pre-extracted metadata for a child CodeObject.
 *
 * Built once by menai_convert_code_object() for each child code object and
 * stored as a PyCapsule on the parent's _code_caches list.  menai_function_alloc()
 * reads directly from this struct, eliminating all PyTuple_GET_ITEM and
 * PyLong_AsLong/PyLong_AsVoidPtr calls from the OP_MAKE_CLOSURE hot path.
 *
 * parameters, name, instrs_obj, constants, names_list, and closure_caches are
 * owned references held by the ClosureCache; the capsule destructor DECREFs them.
 * bytecode (the child CodeObject) is a borrowed reference — kept alive by the
 * parent's code_objects list for the duration of execution.
 */
typedef struct {
    PyObject *parameters;   /* tuple of str — param names */
    PyObject *name;         /* str or None */
    PyObject *bytecode;     /* child CodeObject */
    PyObject *instrs_obj;   /* array.array — keeps instruction buffer alive */
    PyObject *constants;    /* list of fast MenaiValues */
    PyObject *names_list;   /* list of global name strings */
    PyObject *closure_caches; /* list of grandchild ClosureCache capsules, or NULL */
    uint64_t *instrs;       /* raw pointer into instrs_obj buffer */
    int param_count;
    int local_count;
    int is_variadic;
    Py_ssize_t ncap;        /* number of capture slots */
    int code_len;
} ClosureCache;

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
 * Allocates a function with cache->ncap capture slots, all initialised to
 * none_val.  cache is a borrowed pointer; none_val is a borrowed reference.
 * The function takes its own references to the PyObject* fields it needs.
 */
PyObject *menai_function_alloc(const ClosureCache *cache, PyObject *none_val);

/*
 * CLOSURE_CACHE_CAPSULE_NAME — the PyCapsule name used to store ClosureCache
 * pointers on code objects.  Checked on retrieval to prevent type confusion.
 */
#define CLOSURE_CACHE_CAPSULE_NAME "menai.ClosureCache"

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
