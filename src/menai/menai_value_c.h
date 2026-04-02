/*
 * menai_value_c.h — C struct definitions for all Menai runtime value types.
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

#ifndef MENAI_VALUE_C_H
#define MENAI_VALUE_C_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

/* ---------------------------------------------------------------------------
 * Scalar types — fields stored as C primitives
 * ------------------------------------------------------------------------- */

typedef struct {
    PyObject_HEAD
    int value;          /* boolean: 0 or 1 */
} MenaiBoolean_Object;

typedef struct {
    PyObject_HEAD
    double value;       /* float */
} MenaiFloat_Object;

/* ---------------------------------------------------------------------------
 * Types whose payload is a single Python object
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
    PyObject *value;    /* Python str */
} MenaiString_Object;

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
} MenaiDict_Object;

typedef struct {
    PyObject_HEAD
    PyObject *elements; /* Python tuple of MenaiValue* (ordered, deduplicated) */
    PyObject *members;  /* Python frozenset of hashable keys */
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
    int       is_variadic;     /* C int: 0 or 1 */
    int       param_count;     /* C int: number of fixed parameters */
    /* Frame setup cache — populated once in MenaiFunction_new when bytecode
     * is not None.  Eliminates all PyObject_GetAttrString calls from the
     * hot call_setup / frame_setup path.
     *
     * instrs_obj is a borrowed reference: bytecode (owned by this struct)
     * owns the array.array, so instrs_obj lives at least as long as we do.
     * constants and names are likewise borrowed from bytecode. */
    uint64_t *instrs;          /* raw pointer into bytecode.instructions buffer */
    PyObject *instrs_obj;      /* array.array — borrowed ref, keeps buffer valid */
    PyObject *constants;       /* borrowed ref to bytecode.constants list */
    PyObject *names;           /* borrowed ref to bytecode.names list */
    int       code_len;        /* number of instructions */
    int       local_count;     /* number of local variable slots */
} MenaiFunction_Object;

/* ---------------------------------------------------------------------------
 * Struct types
 * ------------------------------------------------------------------------- */

typedef struct {
    PyObject_HEAD
    PyObject *name;         /* Python str */
    int       tag;          /* unique integer tag */
    PyObject *field_names;  /* Python tuple of str */
    PyObject *_field_index; /* Python dict: str -> int */
} MenaiStructType_Object;

typedef struct {
    PyObject_HEAD
    PyObject *struct_type;  /* MenaiStructType_Object* */
    PyObject *fields;       /* Python tuple of MenaiValue* */
} MenaiStruct_Object;

/* ---------------------------------------------------------------------------
 * MenaiNone has no fields beyond the PyObject header
 * ------------------------------------------------------------------------- */

typedef struct {
    PyObject_HEAD
} MenaiNone_Object;

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

#endif /* MENAI_VALUE_C_H */
