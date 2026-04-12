/*
 * menai_vm_value.h — C struct definitions for all Menai runtime value types.
 *
 * Included by menai_vm_shim.h and menai_value_c.c.  Provides the concrete
 * PyObject struct layouts so that the C VM can access fields directly by cast
 * rather than via PyObject_GetAttrString.
 *
 * All types are defined in menai_value_c.c and exposed via the
 * menai_value_c module.  The VM imports that module at init time to obtain
 * the type objects and singleton values; after that it uses these structs
 * directly.
 */

#ifndef MENAI_VM_VALUE_H
#define MENAI_VM_VALUE_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_boolean.h"
#include "menai_vm_none.h"
#include "menai_vm_string.h"

/* ---------------------------------------------------------------------------
 * Scalar types — fields stored as C primitives (MenaiBoolean is in menai_vm_boolean.h)
 * ------------------------------------------------------------------------- */

typedef struct {
    PyObject_HEAD
    double value;       /* float */
} MenaiFloat_Object;

/* ---------------------------------------------------------------------------
 * Types whose payload is a single Python object (MenaiString is in menai_vm_string.h)
 * ------------------------------------------------------------------------- */

typedef struct {
    PyObject_HEAD
    PyObject *value;    /* Python int (arbitrary precision) */
} MenaiInteger_Object;

typedef struct {
    PyObject_HEAD
    PyObject *value;    /* Python complex */
} MenaiComplex_Object;

typedef struct {
    PyObject_HEAD
    PyObject *name;     /* Python str */
} MenaiSymbol_Object;

/* ---------------------------------------------------------------------------
 * Collection types
 * ------------------------------------------------------------------------- */

typedef struct {
    PyObject_HEAD
    PyObject **elements; /* C array of MenaiValue* */
    Py_ssize_t length;   /* number of elements */
} MenaiList_Object;

typedef struct {
    PyObject_HEAD
    PyObject *pairs;    /* Python tuple of (key, value) 2-tuples */
    PyObject *lookup;   /* Python dict: hashable_key -> (key, value) */
    Py_ssize_t length;  /* number of key-value pairs */
} MenaiDict_Object;

typedef struct {
    PyObject_HEAD
    PyObject *elements; /* Python tuple of MenaiValue* (ordered, deduplicated) */
    PyObject *members;  /* Python frozenset of hashable keys */
    Py_ssize_t length;  /* number of elements */
} MenaiSet_Object;

/* ---------------------------------------------------------------------------
 * Function type
 * ------------------------------------------------------------------------- */

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

/* ---------------------------------------------------------------------------
 * Struct types
 * ------------------------------------------------------------------------- */

typedef struct {
    PyObject_HEAD
    PyObject *name;         /* Python str */
    int tag;                /* unique integer tag */
    PyObject *field_names;  /* Python tuple of str */
    PyObject *_field_index; /* Python dict: str -> int */
} MenaiStructType_Object;

typedef struct {
    PyObject_HEAD
    PyObject *struct_type;  /* MenaiStructType_Object* */
    PyObject *fields;       /* Python tuple of MenaiValue* */
} MenaiStruct_Object;

/* ---------------------------------------------------------------------------
 * MenaiList C-level constructors — defined in menai_value_c.c.
 *
 * menai_list_from_array:       copy items, INCREF each element
 * menai_list_from_array_steal: take ownership without INCREFing
 * menai_list_from_tuple:       copy from tuple, INCREF each, DECREF tuple
 * ------------------------------------------------------------------------- */

PyObject *menai_list_from_array(PyObject **items, Py_ssize_t n);
PyObject *menai_list_from_array_steal(PyObject **items, Py_ssize_t n);
PyObject *menai_list_from_tuple(PyObject *tup);

/* ---------------------------------------------------------------------------
 * MenaiFunction C-level constructor — defined in menai_value_c.c.
 * Bypasses PyObject_Call and argument parsing.  All arguments are borrowed.
 * ------------------------------------------------------------------------- */

PyObject *menai_function_alloc(PyObject *cache, PyObject *bytecode,
                               PyObject *captured_values);

/* ---------------------------------------------------------------------------
 * MenaiStruct C-level constructor — defined in menai_value_c.c.
 * struct_type is borrowed (INCREF'd internally); fields_tup is stolen.
 * Returns a new reference, or NULL on error.
 * ------------------------------------------------------------------------- */

PyObject *menai_struct_alloc(PyObject *struct_type, PyObject *fields_tup);

/* ---------------------------------------------------------------------------
 * menai_hashable_key — convert a MenaiValue key to a hashable Python tuple.
 * Defined in menai_value_c.c (was static _hashable_key).
 * Returns a new reference, or NULL on error (MenaiEvalError set).
 * ------------------------------------------------------------------------- */

PyObject *menai_hashable_key(PyObject *key);

/* ---------------------------------------------------------------------------
 * MenaiList inline accessors
 * ------------------------------------------------------------------------- */

static inline PyObject *
menai_list_get(MenaiList_Object *list, Py_ssize_t i)
{
    return list->elements[i];
}

static inline PyObject **
menai_list_elements(PyObject *list_obj)
{
    return ((MenaiList_Object *)list_obj)->elements;
}

static inline Py_ssize_t
menai_list_length(PyObject *list_obj)
{
    return ((MenaiList_Object *)list_obj)->length;
}

/* ---------------------------------------------------------------------------
 * Conversion functions — defined in menai_value_c.c, called by the C VM
 *
 * convert_value: translate one slow menai_value.py object to a fast C type.
 *   Returns a new reference, or NULL on error.
 *
 * convert_code_object: walk a CodeObject tree, converting all constants
 *   in-place.  Returns the same code object (borrowed), or NULL on error.
 *
 * to_slow: translate one fast C value back to a slow menai_value.py object.
 *   Returns a new reference, or NULL on error.
 * ------------------------------------------------------------------------- */

PyObject *menai_convert_value(PyObject *src);
PyObject *menai_convert_code_object(PyObject *code);
PyObject *menai_to_slow(PyObject *src);

#endif /* MENAI_VM_VALUE_H */
