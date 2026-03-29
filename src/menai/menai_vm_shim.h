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
 * Value constructors
 *
 * Each function wraps a C or Python primitive in the corresponding Menai
 * type and returns a new reference, or NULL on failure.
 *
 * make_integer_value() additionally consumes (Py_DECREF) the Python int
 * it is given, matching the ownership transfer that INT_STORE previously
 * performed implicitly.
 * ------------------------------------------------------------------------- */

static inline PyObject *make_integer(PyObject *py_int) {
    return PyObject_CallOneArg((PyObject *)Menai_IntegerType, py_int);
}

static inline PyObject *make_float(double v) {
    PyObject *pf = PyFloat_FromDouble(v);
    if (pf == NULL) return NULL;
    PyObject *r = PyObject_CallOneArg((PyObject *)Menai_FloatType, pf);
    Py_DECREF(pf);
    return r;
}

static inline PyObject *make_complex_val(double real, double imag) {
    PyObject *pc = PyComplex_FromDoubles(real, imag);
    if (pc == NULL) return NULL;
    PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ComplexType, pc);
    Py_DECREF(pc);
    return r;
}

static inline PyObject *make_string_from_pyobj(PyObject *py_str) {
    return PyObject_CallOneArg((PyObject *)Menai_StringType, py_str);
}

/*
 * make_integer_value — consume a Python int ref, wrap it in MenaiInteger.
 *
 * Takes ownership of py_int (calls Py_DECREF on it).  Returns a new
 * MenaiInteger reference, or NULL on failure.  Handles a NULL input
 * gracefully (returns NULL without crashing).
 */
static inline PyObject *make_integer_value(PyObject *py_int) {
    if (py_int == NULL) return NULL;
    PyObject *r = make_integer(py_int);
    Py_DECREF(py_int);
    return r;
}

/*
 * make_complex_value — consume a Python complex ref, wrap it in MenaiComplex.
 *
 * Takes ownership of py_complex (calls Py_DECREF on it).  Returns a new
 * MenaiComplex reference, or NULL on failure.  Handles a NULL input
 * gracefully.
 */
static inline PyObject *make_complex_value(PyObject *py_complex) {
    if (py_complex == NULL) return NULL;
    PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ComplexType, py_complex);
    Py_DECREF(py_complex);
    return r;
}

/*
 * bool_store — write Menai_TRUE or Menai_FALSE into a register.
 */
static inline void bool_store(PyObject **regs, int slot, int cond) {
    reg_set(regs, slot, cond ? Menai_TRUE : Menai_FALSE);
}

/* ---------------------------------------------------------------------------
 * Integer arithmetic helpers
 *
 * These replace the INT_CMP, INT_BINOP, and INT_UNOP macros.  Each returns
 * 0 on success or -1 on failure (Python exception set).  The function-pointer
 * parameters accept any PyNumber_* function with the matching signature.
 * ------------------------------------------------------------------------- */

typedef PyObject *(*menai_unaryfunc)(PyObject *);
typedef PyObject *(*menai_binaryfunc)(PyObject *, PyObject *);

static inline int
int_cmp(PyObject **regs, int slot, PyObject *a, PyObject *b, int op)
{
    PyObject *av = menai_integer_value(a);
    if (av == NULL) return -1;
    PyObject *bv = menai_integer_value(b);
    if (bv == NULL) { Py_DECREF(av); return -1; }
    int r = PyObject_RichCompareBool(av, bv, op);
    Py_DECREF(av); Py_DECREF(bv);
    if (r < 0) return -1;
    bool_store(regs, slot, r);
    return 0;
}

static inline int
int_unop(PyObject **regs, int slot, PyObject *a, menai_unaryfunc fn)
{
    PyObject *av = menai_integer_value(a);
    if (av == NULL) return -1;
    PyObject *res = fn(av);
    Py_DECREF(av);
    PyObject *r = make_integer_value(res);
    if (r == NULL) return -1;
    reg_set(regs, slot, r);
    Py_DECREF(r);
    return 0;
}

static inline int
int_binop(PyObject **regs, int slot, PyObject *a, PyObject *b,
          menai_binaryfunc fn)
{
    PyObject *av = menai_integer_value(a);
    if (av == NULL) return -1;
    PyObject *bv = menai_integer_value(b);
    if (bv == NULL) { Py_DECREF(av); return -1; }
    PyObject *res = fn(av, bv);
    Py_DECREF(av); Py_DECREF(bv);
    PyObject *r = make_integer_value(res);
    if (r == NULL) return -1;
    reg_set(regs, slot, r);
    Py_DECREF(r);
    return 0;
}

/* ---------------------------------------------------------------------------
 * String comparison helper
 *
 * Replaces the STR_CMP macro.  Returns 0 on success, -1 on failure.
 * ------------------------------------------------------------------------- */

static inline int
str_cmp(PyObject **regs, int slot, PyObject *a, PyObject *b, int op)
{
    PyObject *sa = menai_string_value(a);
    if (sa == NULL) return -1;
    PyObject *sb = menai_string_value(b);
    if (sb == NULL) { Py_DECREF(sa); return -1; }
    PyObject *cmp = PyUnicode_RichCompare(sa, sb, op);
    Py_DECREF(sa); Py_DECREF(sb);
    if (cmp == NULL) return -1;
    int r = PyObject_IsTrue(cmp);
    Py_DECREF(cmp);
    if (r < 0) return -1;
    bool_store(regs, slot, r);
    return 0;
}

/* ---------------------------------------------------------------------------
 * List elements accessor
 *
 * Replaces LIST_ELEMENTS(obj, var, error).  Returns a new reference to the
 * elements tuple, or NULL on failure (Python exception set).
 * ------------------------------------------------------------------------- */

static inline PyObject *
menai_list_elements(PyObject *list_obj)
{
    return PyObject_GetAttrString(list_obj, "elements");
}

/* ---------------------------------------------------------------------------
 * Complex value accessor
 *
 * Replaces CPX_VAL(obj, var).  Returns a new reference to the Python complex
 * .value field, or NULL on failure.
 * ------------------------------------------------------------------------- */

static inline PyObject *
menai_complex_value(PyObject *obj)
{
    return PyObject_GetAttrString(obj, "value");
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

static inline int require_boolean(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_BOOLEAN(val), val, op_name, "boolean arguments");
}

static inline int require_function(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_FUNCTION(val), val, op_name, "function arguments");
}

static inline int require_struct(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_STRUCT(val), val, op_name, "a struct argument");
}

static inline int require_structtype(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_STRUCTTYPE(val), val, op_name, "a struct type argument");
}

/*
 * Symbol type-check helpers.
 *
 * The symbol ops use a different error phrasing from the rest ("must be a
 * symbol" / "must be symbols") which is what the tests assert on.  These
 * helpers produce that exact phrasing rather than delegating to
 * require_type_impl.
 */
static inline int
require_symbol(PyObject *val, const char *op_name)
{
    if (IS_MENAI_SYMBOL(val))
        return 1;
    menai_raise_eval_errorf("%s: argument must be a symbol", op_name);
    return 0;
}

static inline int
require_symbol_pair(PyObject *a, PyObject *b, const char *op_name)
{
    if (IS_MENAI_SYMBOL(a) && IS_MENAI_SYMBOL(b))
        return 1;
    menai_raise_eval_errorf("%s: arguments must be symbols", op_name);
    return 0;
}

static inline int
require_function_singular(PyObject *val, const char *op_name)
{
    return require_type_impl(IS_MENAI_FUNCTION(val), val, op_name, "a function argument");
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
