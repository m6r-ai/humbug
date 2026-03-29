/*
 * menai_vm_shim.h — C interface to the Menai native value types.
 *
 * Provides type-check macros, singleton references, and field-access helpers
 * for the C VM.  All type pointers and singleton references are populated at
 * module init by menai_vm_shim_init(), which imports menai_value_c.
 *
 * Because the value types are defined in menai_value_c.c (which we own),
 * field access uses direct struct casts via the definitions in
 * menai_value_c.h — no runtime offset computation needed.
 */

#ifndef MENAI_VM_SHIM_H
#define MENAI_VM_SHIM_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stddef.h>

#include "menai_value_c.h"

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
 * Direct field access via struct cast
 *
 * These read C-level fields directly using the known struct layout from
 * menai_value_c.h.  No runtime offset computation required.
 * ------------------------------------------------------------------------- */

static inline int menai_boolean_value(PyObject *o) {
    return ((MenaiBoolean_Object *)o)->value;
}

static inline double menai_float_value(PyObject *o) {
    return ((MenaiFloat_Object *)o)->value;
}

static inline PyObject *menai_integer_pyobj(PyObject *o) {
    /* Borrowed reference to the Python int inside MenaiInteger. */
    return ((MenaiInteger_Object *)o)->value;
}

static inline PyObject *menai_string_pyobj(PyObject *o) {
    /* Borrowed reference to the Python str inside MenaiString. */
    return ((MenaiString_Object *)o)->value;
}

static inline PyObject *menai_symbol_name_pyobj(PyObject *o) {
    /* Borrowed reference to the Python str inside MenaiSymbol. */
    return ((MenaiSymbol_Object *)o)->name;
}

static inline PyObject *menai_list_elements_pyobj(PyObject *o) {
    /* Borrowed reference to the elements tuple inside MenaiList. */
    return ((MenaiList_Object *)o)->elements;
}

static inline PyObject *menai_complex_pyobj(PyObject *o) {
    /* Borrowed reference to the Python complex inside MenaiComplex. */
    return ((MenaiComplex_Object *)o)->value;
}

/* ---------------------------------------------------------------------------
 * New-reference accessors
 *
 * These return new references (Py_INCREF'd) for callers that need to own
 * the reference.  Callers are responsible for Py_DECREF.
 * ------------------------------------------------------------------------- */

static inline PyObject *menai_get_attr(PyObject *o, const char *name) {
    return PyObject_GetAttrString(o, name);
}

static inline PyObject *menai_integer_value(PyObject *o) {
    PyObject *v = ((MenaiInteger_Object *)o)->value;
    Py_INCREF(v);
    return v;
}

static inline PyObject *menai_symbol_name(PyObject *o) {
    PyObject *n = ((MenaiSymbol_Object *)o)->name;
    Py_INCREF(n);
    return n;
}

static inline PyObject *menai_string_value(PyObject *o) {
    PyObject *v = ((MenaiString_Object *)o)->value;
    Py_INCREF(v);
    return v;
}

/* ---------------------------------------------------------------------------
 * Register array helpers
 * ------------------------------------------------------------------------- */

static inline PyObject *reg_get(PyObject **regs, int slot) {
    return regs[slot];
}

static inline void reg_set(PyObject **regs, int slot, PyObject *val) {
    PyObject *old = regs[slot];
    Py_XINCREF(val);
    regs[slot] = val;
    Py_XDECREF(old);
}

/* ---------------------------------------------------------------------------
 * Value constructors
 * ------------------------------------------------------------------------- */

static inline PyObject *make_integer(PyObject *py_int) {
    MenaiInteger_Object *r = (MenaiInteger_Object *)
        Menai_IntegerType->tp_alloc(Menai_IntegerType, 0);
    if (r) { Py_INCREF(py_int); r->value = py_int; }
    return (PyObject *)r;
}

static inline PyObject *make_float(double v) {
    MenaiFloat_Object *r = (MenaiFloat_Object *)
        Menai_FloatType->tp_alloc(Menai_FloatType, 0);
    if (r) r->value = v;
    return (PyObject *)r;
}

static inline PyObject *make_complex_from_doubles(double real, double imag) {
    PyObject *pc = PyComplex_FromDoubles(real, imag);
    if (!pc) return NULL;
    MenaiComplex_Object *r = (MenaiComplex_Object *)
        Menai_ComplexType->tp_alloc(Menai_ComplexType, 0);
    if (r) { r->value = pc; }
    else   { Py_DECREF(pc); }
    return (PyObject *)r;
}

static inline PyObject *make_string_from_pyobj(PyObject *py_str) {
    MenaiString_Object *r = (MenaiString_Object *)
        Menai_StringType->tp_alloc(Menai_StringType, 0);
    if (r) { Py_INCREF(py_str); r->value = py_str; }
    return (PyObject *)r;
}

static inline PyObject *make_integer_value(PyObject *py_int) {
    if (!py_int) return NULL;
    PyObject *r = make_integer(py_int);
    Py_DECREF(py_int);
    return r;
}

static inline PyObject *make_complex_value(PyObject *py_complex) {
    if (!py_complex) return NULL;
    MenaiComplex_Object *r = (MenaiComplex_Object *)
        Menai_ComplexType->tp_alloc(Menai_ComplexType, 0);
    if (r) { r->value = py_complex; }  /* steals py_complex */
    else   { Py_DECREF(py_complex); }
    return (PyObject *)r;
}

static inline void bool_store(PyObject **regs, int slot, int cond) {
    reg_set(regs, slot, cond ? Menai_TRUE : Menai_FALSE);
}

/* ---------------------------------------------------------------------------
 * Integer arithmetic helpers
 * ------------------------------------------------------------------------- */

typedef PyObject *(*menai_unaryfunc)(PyObject *);
typedef PyObject *(*menai_binaryfunc)(PyObject *, PyObject *);

static inline int
int_cmp(PyObject **regs, int slot, PyObject *a, PyObject *b, int op)
{
    PyObject *av = menai_integer_value(a);
    if (!av) return -1;
    PyObject *bv = menai_integer_value(b);
    if (!bv) { Py_DECREF(av); return -1; }
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
    if (!av) return -1;
    PyObject *res = fn(av);
    Py_DECREF(av);
    PyObject *r = make_integer_value(res);
    if (!r) return -1;
    reg_set(regs, slot, r);
    Py_DECREF(r);
    return 0;
}

static inline int
int_binop(PyObject **regs, int slot, PyObject *a, PyObject *b,
          menai_binaryfunc fn)
{
    PyObject *av = menai_integer_value(a);
    if (!av) return -1;
    PyObject *bv = menai_integer_value(b);
    if (!bv) { Py_DECREF(av); return -1; }
    PyObject *res = fn(av, bv);
    Py_DECREF(av); Py_DECREF(bv);
    PyObject *r = make_integer_value(res);
    if (!r) return -1;
    reg_set(regs, slot, r);
    Py_DECREF(r);
    return 0;
}

/* ---------------------------------------------------------------------------
 * String comparison helper
 * ------------------------------------------------------------------------- */

static inline int
str_cmp(PyObject **regs, int slot, PyObject *a, PyObject *b, int op)
{
    /* Direct access — no attribute lookup */
    PyObject *sa = ((MenaiString_Object *)a)->value;
    PyObject *sb = ((MenaiString_Object *)b)->value;
    PyObject *cmp = PyUnicode_RichCompare(sa, sb, op);
    if (!cmp) return -1;
    int r = PyObject_IsTrue(cmp);
    Py_DECREF(cmp);
    if (r < 0) return -1;
    bool_store(regs, slot, r);
    return 0;
}

/* ---------------------------------------------------------------------------
 * List elements accessor — returns borrowed reference
 * ------------------------------------------------------------------------- */

static inline PyObject *
menai_list_elements(PyObject *list_obj)
{
    /* Borrowed reference — no Py_INCREF, no Py_DECREF by caller */
    return ((MenaiList_Object *)list_obj)->elements;
}

/* ---------------------------------------------------------------------------
 * Complex value accessor — returns new reference
 * ------------------------------------------------------------------------- */

static inline PyObject *
menai_complex_value(PyObject *obj)
{
    PyObject *v = ((MenaiComplex_Object *)obj)->value;
    Py_INCREF(v);
    return v;
}

/* ---------------------------------------------------------------------------
 * Error helpers
 * ------------------------------------------------------------------------- */

PyObject *menai_raise_eval_error(const char *message);
PyObject *menai_raise_eval_errorf(const char *fmt, ...);

/* ---------------------------------------------------------------------------
 * Type-requirement guards
 * ------------------------------------------------------------------------- */

static inline int
require_type_impl(int ok, PyObject *val, const char *op_name, const char *noun)
{
    if (ok) return 1;
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
static inline int require_symbol(PyObject *val, const char *op_name) {
    if (IS_MENAI_SYMBOL(val)) return 1;
    menai_raise_eval_errorf("%s: argument must be a symbol", op_name);
    return 0;
}
static inline int require_symbol_pair(PyObject *a, PyObject *b, const char *op_name) {
    if (IS_MENAI_SYMBOL(a) && IS_MENAI_SYMBOL(b)) return 1;
    menai_raise_eval_errorf("%s: arguments must be symbols", op_name);
    return 0;
}
static inline int require_function_singular(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_FUNCTION(val), val, op_name, "a function argument");
}

/* ---------------------------------------------------------------------------
 * Init
 * ------------------------------------------------------------------------- */

int menai_vm_shim_init(void);

#endif /* MENAI_VM_SHIM_H */
