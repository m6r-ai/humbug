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

#include "menai_vm_float.h"
#include "menai_vm_boolean.h"
#include "menai_vm_complex.h"
#include "menai_vm_function.h"
#include "menai_vm_integer.h"
#include "menai_vm_list.h"
#include "menai_vm_none.h"
#include "menai_vm_string.h"
#include "menai_vm_symbol.h"

/* ---------------------------------------------------------------------------
 * Collection types (MenaiList is in menai_vm_list.h)
 * ------------------------------------------------------------------------- */

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
