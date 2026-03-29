/*
 * menai_vm_shim.h — stable C interface to menai_value_fast Cython types.
 *
 * Provides type-check macros, singleton references, and field-access helpers
 * for the C VM without depending on Cython-generated headers.  All pointers
 * are populated at module init by menai_vm_shim_init().
 *
 * Field access for scalar types (MenaiBoolean, MenaiFloat) uses byte offsets
 * computed at runtime by menai_value_fast.get_field_offsets(), so the layout
 * is always correct regardless of Cython version or internal padding.
 *
 * Field access for Python-object fields (MenaiInteger.value, MenaiString.value,
 * MenaiSymbol.name, MenaiList.elements, MenaiDict.pairs, MenaiDict.lookup,
 * MenaiFunction.*, etc.) uses PyObject_GetAttrString — safe and correct, with
 * the expectation that hot paths will be profiled and promoted to offset-based
 * access if needed.
 */

#ifndef MENAI_VM_SHIM_H
#define MENAI_VM_SHIM_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stddef.h>

/* ---------------------------------------------------------------------------
 * Type object pointers — populated by menai_vm_shim_init()
 * ------------------------------------------------------------------------- */

extern PyTypeObject *Menai_NoneType;
extern PyTypeObject *Menai_BooleanType;
extern PyTypeObject *Menai_IntegerType;
extern PyTypeObject *Menai_FloatType;
extern PyTypeObject *Menai_ComplexType;
extern PyTypeObject *Menai_StringType;
extern PyTypeObject *Menai_SymbolType;
extern PyTypeObject *Menai_ListType;
extern PyTypeObject *Menai_DictType;
extern PyTypeObject *Menai_SetType;
extern PyTypeObject *Menai_FunctionType;
extern PyTypeObject *Menai_StructTypeType;
extern PyTypeObject *Menai_StructType;

/* ---------------------------------------------------------------------------
 * Singleton value references — populated by menai_vm_shim_init()
 * ------------------------------------------------------------------------- */

extern PyObject *Menai_NONE;
extern PyObject *Menai_TRUE;
extern PyObject *Menai_FALSE;
extern PyObject *Menai_EMPTY_LIST;
extern PyObject *Menai_EMPTY_DICT;
extern PyObject *Menai_EMPTY_SET;

/* ---------------------------------------------------------------------------
 * Scalar field offsets — populated by menai_vm_shim_init()
 *
 * These are byte offsets from the start of the PyObject* to the C-level
 * field, computed at runtime from live instances via get_field_offsets().
 * ------------------------------------------------------------------------- */

extern size_t Menai_offset_boolean_value;  /* MenaiBoolean.value  (int)    */
extern size_t Menai_offset_float_value;    /* MenaiFloat.value    (double) */

/* ---------------------------------------------------------------------------
 * Fast type-check macros
 *
 * Py_TYPE(o) is a single pointer dereference — no Python call overhead.
 * ------------------------------------------------------------------------- */

#define IS_MENAI_NONE(o)       (Py_TYPE(o) == Menai_NoneType)
#define IS_MENAI_BOOLEAN(o)    (Py_TYPE(o) == Menai_BooleanType)
#define IS_MENAI_INTEGER(o)    (Py_TYPE(o) == Menai_IntegerType)
#define IS_MENAI_FLOAT(o)      (Py_TYPE(o) == Menai_FloatType)
#define IS_MENAI_COMPLEX(o)    (Py_TYPE(o) == Menai_ComplexType)
#define IS_MENAI_STRING(o)     (Py_TYPE(o) == Menai_StringType)
#define IS_MENAI_SYMBOL(o)     (Py_TYPE(o) == Menai_SymbolType)
#define IS_MENAI_LIST(o)       (Py_TYPE(o) == Menai_ListType)
#define IS_MENAI_DICT(o)       (Py_TYPE(o) == Menai_DictType)
#define IS_MENAI_SET(o)        (Py_TYPE(o) == Menai_SetType)
#define IS_MENAI_FUNCTION(o)   (Py_TYPE(o) == Menai_FunctionType)
#define IS_MENAI_STRUCTTYPE(o) (Py_TYPE(o) == Menai_StructTypeType)
#define IS_MENAI_STRUCT(o)     (Py_TYPE(o) == Menai_StructType)

/* ---------------------------------------------------------------------------
 * Direct scalar field access via runtime-computed offsets
 *
 * These read C-level fields directly from the object's memory layout.
 * Only safe after menai_vm_shim_init() has populated the offsets.
 * ------------------------------------------------------------------------- */

/* MenaiBoolean.value — C int (bint) */
static inline int menai_boolean_value(PyObject *o) {
    return *(int *)((char *)o + Menai_offset_boolean_value);
}

/* MenaiFloat.value — C double */
static inline double menai_float_value(PyObject *o) {
    return *(double *)((char *)o + Menai_offset_float_value);
}

/* ---------------------------------------------------------------------------
 * Python-object field access via attribute lookup
 *
 * Used for complex types (MenaiList, MenaiDict, MenaiFunction, etc.) where
 * the fields are Python objects and offset-based access is not yet needed.
 * Callers are responsible for Py_DECREF on the returned object.
 * ------------------------------------------------------------------------- */

static inline PyObject *menai_get_attr(PyObject *o, const char *name) {
    return PyObject_GetAttrString(o, name);
}

/*
 * New-reference accessors for Python-object fields.
 *
 * Each returns a new reference.  Callers are responsible for Py_DECREF.
 * Returns NULL on error (Python exception set).
 */
static inline PyObject *menai_integer_value(PyObject *o) {
    return PyObject_GetAttrString(o, "value");
}

static inline PyObject *menai_symbol_name(PyObject *o) {
    return PyObject_GetAttrString(o, "name");
}

static inline PyObject *menai_string_value(PyObject *o) {
    return PyObject_GetAttrString(o, "value");
}

/* ---------------------------------------------------------------------------
 * Register array helpers
 *
 * All register reads and writes must go through these helpers to ensure
 * correct reference counting.  reg_set decrements the old value and
 * increments the new one atomically.
 * ------------------------------------------------------------------------- */

static inline PyObject *reg_get(PyObject **regs, int slot) {
    return regs[slot];  /* borrowed reference — do not Py_DECREF */
}

static inline void reg_set(PyObject **regs, int slot, PyObject *val) {
    PyObject *old = regs[slot];
    Py_XINCREF(val);
    regs[slot] = val;
    Py_XDECREF(old);
}

/* ---------------------------------------------------------------------------
 * Error helpers
 * ------------------------------------------------------------------------- */

/*
 * Raise a MenaiEvalError with a plain C string message.
 * Returns NULL so callers can write: return menai_type_error("...");
 */
PyObject *menai_raise_eval_error(const char *message);

/*
 * Raise a MenaiEvalError with a formatted message built from a Python
 * format string and arguments.  Returns NULL.
 */
PyObject *menai_raise_eval_errorf(const char *fmt, ...);

/* ---------------------------------------------------------------------------
 * Type-requirement guards
 *
 * Each function checks that val is of the expected Menai type.  Returns 1 if
 * the check passes, 0 if it fails (with a MenaiEvalError already set).
 *
 * Usage pattern in the dispatch loop:
 *
 *   if (!require_integer(a, "integer+")) goto error;
 *
 * The typed wrappers all delegate to require_type_impl, which holds the
 * single copy of the error-formatting logic.
 * ------------------------------------------------------------------------- */

static inline int
require_type_impl(int ok, PyObject *val, const char *op_name, const char *noun)
{
    if (ok)
        return 1;
    PyObject *tn = PyObject_CallMethod(val, "type_name", NULL);
    menai_raise_eval_errorf("Function '%s' requires %s, got %s",
        op_name, noun, tn ? PyUnicode_AsUTF8(tn) : "?");
    Py_XDECREF(tn);
    return 0;
}

static inline int require_integer(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_INTEGER(val), val, op_name, "integer arguments");
}

static inline int require_float(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_FLOAT(val), val, op_name, "float arguments");
}

static inline int require_complex(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_COMPLEX(val), val, op_name, "complex arguments");
}

static inline int require_string(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_STRING(val), val, op_name, "string arguments");
}

static inline int require_list(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_LIST(val), val, op_name, "list arguments");
}

static inline int require_list_singular(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_LIST(val), val, op_name, "a list argument");
}

static inline int require_dict(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_DICT(val), val, op_name, "dict arguments");
}

static inline int require_set(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_SET(val), val, op_name, "set arguments");
}

static inline int require_set_singular(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_SET(val), val, op_name, "a set argument");
}

/* ---------------------------------------------------------------------------
 * Init
 * ------------------------------------------------------------------------- */

/*
 * Populate all type pointers, singleton references, and field offsets.
 * Must be called once from PyInit_menai_vm_c() before any VM operation.
 * Returns 0 on success, -1 on failure (Python exception is set).
 */
int menai_vm_shim_init(void);

#endif /* MENAI_VM_SHIM_H */
