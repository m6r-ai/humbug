/*
 * menai_vm_c.c — C implementation of the Menai VM execute loop.
 *
 * Exposes a single Python-callable function:
 *
 *   menai_vm_c.execute(code, globals_dict, prelude_dict) -> MenaiValue
 *
 * The MenaiVM Python class in menai_vm.py calls this in place of its Python
 * execute loop when this extension is available.
 *
 * Build:
 *   python setup.py build_ext --inplace
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <math.h>
#include <complex.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "menai_value_c.h"

/* ---------------------------------------------------------------------------
 * Limits
 * ------------------------------------------------------------------------- */

#define MAX_FRAME_DEPTH 1024

/* Cancellation check interval — matches the Python VM default. */
#define CANCEL_CHECK_INTERVAL 1000

/* ---------------------------------------------------------------------------
 * Instruction encoding constants — must match menai_bytecode.py
 * ------------------------------------------------------------------------- */

#define OPCODE_SHIFT 48
#define DEST_SHIFT   36
#define SRC0_SHIFT   24
#define SRC1_SHIFT   12
#define FIELD_MASK   0xFFFu
#define OPCODE_MASK  0xFFFFu

/* ---------------------------------------------------------------------------
 * Opcode values — must match menai_bytecode.py Opcode enum
 * ------------------------------------------------------------------------- */

#define OP_LOAD_NONE        0
#define OP_LOAD_TRUE        1
#define OP_LOAD_FALSE       2
#define OP_LOAD_EMPTY_LIST  3
#define OP_LOAD_EMPTY_DICT  4
#define OP_LOAD_EMPTY_SET   5
#define OP_LOAD_CONST       6
#define OP_LOAD_NAME        7
#define OP_MOVE             8
#define OP_JUMP             20
#define OP_JUMP_IF_FALSE    21
#define OP_JUMP_IF_TRUE     22
#define OP_RAISE_ERROR      23
#define OP_MAKE_CLOSURE     30
#define OP_PATCH_CLOSURE    31
#define OP_CALL             32
#define OP_TAIL_CALL        33
#define OP_APPLY            34
#define OP_TAIL_APPLY       35
#define OP_RETURN           37
#define OP_EMIT_TRACE       40

/* Layer 3 — none/boolean/symbol/integer/float opcodes */
#define OP_NONE_P                    50
#define OP_FUNCTION_P                60
#define OP_FUNCTION_EQ_P             61
#define OP_FUNCTION_NEQ_P            62
#define OP_FUNCTION_MIN_ARITY        63
#define OP_FUNCTION_VARIADIC_P       64
#define OP_FUNCTION_ACCEPTS_P        65
#define OP_SYMBOL_P                  80
#define OP_SYMBOL_EQ_P               81
#define OP_SYMBOL_NEQ_P              82
#define OP_SYMBOL_TO_STRING          83
#define OP_BOOLEAN_P                100
#define OP_BOOLEAN_EQ_P             101
#define OP_BOOLEAN_NEQ_P            102
#define OP_BOOLEAN_NOT              103
#define OP_INTEGER_P                120
#define OP_INTEGER_EQ_P             121
#define OP_INTEGER_NEQ_P            122
#define OP_INTEGER_LT_P             123
#define OP_INTEGER_GT_P             124
#define OP_INTEGER_LTE_P            125
#define OP_INTEGER_GTE_P            126
#define OP_INTEGER_ABS              127
#define OP_INTEGER_ADD              128
#define OP_INTEGER_SUB              129
#define OP_INTEGER_MUL              130
#define OP_INTEGER_DIV              131
#define OP_INTEGER_MOD              132
#define OP_INTEGER_NEG              133
#define OP_INTEGER_EXPN             134
#define OP_INTEGER_BIT_NOT          135
#define OP_INTEGER_BIT_SHIFT_LEFT   136
#define OP_INTEGER_BIT_SHIFT_RIGHT  137
#define OP_INTEGER_BIT_OR           138
#define OP_INTEGER_BIT_AND          139
#define OP_INTEGER_BIT_XOR          140
#define OP_INTEGER_MIN              141
#define OP_INTEGER_MAX              142
#define OP_INTEGER_TO_FLOAT         143
#define OP_INTEGER_TO_COMPLEX       144
#define OP_INTEGER_TO_STRING        145
#define OP_INTEGER_CODEPOINT_TO_STRING 146
#define OP_FLOAT_P                  160
#define OP_FLOAT_EQ_P               161
#define OP_FLOAT_NEQ_P              162
#define OP_FLOAT_LT_P               163
#define OP_FLOAT_GT_P               164
#define OP_FLOAT_LTE_P              165
#define OP_FLOAT_GTE_P              166
#define OP_FLOAT_NEG                167
#define OP_FLOAT_ADD                168
#define OP_FLOAT_SUB                169
#define OP_FLOAT_MUL                170
#define OP_FLOAT_DIV                171
#define OP_FLOAT_FLOOR_DIV          172
#define OP_FLOAT_MOD                173
#define OP_FLOAT_EXP                174
#define OP_FLOAT_EXPN               175
#define OP_FLOAT_LOG                176
#define OP_FLOAT_LOG10              177
#define OP_FLOAT_LOG2               178
#define OP_FLOAT_LOGN               179
#define OP_FLOAT_SIN                180
#define OP_FLOAT_COS                181
#define OP_FLOAT_TAN                182
#define OP_FLOAT_SQRT               183
#define OP_FLOAT_ABS                184
#define OP_FLOAT_TO_INTEGER         185
#define OP_FLOAT_TO_COMPLEX         186
#define OP_FLOAT_TO_STRING          187
#define OP_FLOAT_FLOOR              188
#define OP_FLOAT_CEIL               189
#define OP_FLOAT_ROUND              190
#define OP_FLOAT_MIN                191
#define OP_FLOAT_MAX                192

/* Layer 5 — structs, closures, apply */
#define OP_MAKE_STRUCT       360
#define OP_STRUCT_P          361
#define OP_STRUCT_TYPE_P     362
#define OP_STRUCT_GET        363
#define OP_STRUCT_GET_IMM    364
#define OP_STRUCT_SET        365
#define OP_STRUCT_SET_IMM    366
#define OP_STRUCT_EQ_P       367
#define OP_STRUCT_NEQ_P      368
#define OP_STRUCT_TYPE       369
#define OP_STRUCT_TYPE_NAME  370
#define OP_STRUCT_FIELDS     371

/* Layer 4 — complex, string, list, dict, set, range */
#define OP_COMPLEX_P                200
#define OP_COMPLEX_EQ_P             201
#define OP_COMPLEX_NEQ_P            202
#define OP_COMPLEX_REAL             203
#define OP_COMPLEX_IMAG             204
#define OP_COMPLEX_ABS              205
#define OP_COMPLEX_ADD              206
#define OP_COMPLEX_SUB              207
#define OP_COMPLEX_MUL              208
#define OP_COMPLEX_DIV              209
#define OP_COMPLEX_NEG              210
#define OP_COMPLEX_EXP              211
#define OP_COMPLEX_EXPN             212
#define OP_COMPLEX_LOG              213
#define OP_COMPLEX_LOG10            214
#define OP_COMPLEX_LOGN             215
#define OP_COMPLEX_SIN              216
#define OP_COMPLEX_COS              217
#define OP_COMPLEX_TAN              218
#define OP_COMPLEX_SQRT             219
#define OP_COMPLEX_TO_STRING        220
#define OP_STRING_P                 240
#define OP_STRING_EQ_P              241
#define OP_STRING_NEQ_P             242
#define OP_STRING_LT_P              243
#define OP_STRING_GT_P              244
#define OP_STRING_LTE_P             245
#define OP_STRING_GTE_P             246
#define OP_STRING_LENGTH            247
#define OP_STRING_UPCASE            248
#define OP_STRING_DOWNCASE          249
#define OP_STRING_TRIM              250
#define OP_STRING_TRIM_LEFT         251
#define OP_STRING_TRIM_RIGHT        252
#define OP_STRING_TO_INTEGER        253
#define OP_STRING_TO_NUMBER         254
#define OP_STRING_TO_LIST           255
#define OP_STRING_REF               256
#define OP_STRING_PREFIX_P          257
#define OP_STRING_SUFFIX_P          258
#define OP_STRING_CONCAT            259
#define OP_STRING_SLICE             260
#define OP_STRING_REPLACE           261
#define OP_STRING_INDEX             262
#define OP_STRING_TO_INTEGER_CODEPOINT 263
#define OP_DICT_P                   280
#define OP_DICT_EQ_P                281
#define OP_DICT_NEQ_P               282
#define OP_DICT_KEYS                283
#define OP_DICT_VALUES              284
#define OP_DICT_LENGTH              285
#define OP_DICT_HAS_P               286
#define OP_DICT_REMOVE              287
#define OP_DICT_MERGE               288
#define OP_DICT_SET                 289
#define OP_DICT_GET                 290
#define OP_LIST_P                   300
#define OP_LIST_EQ_P                301
#define OP_LIST_NEQ_P               302
#define OP_LIST_PREPEND             303
#define OP_LIST_APPEND              304
#define OP_LIST_REVERSE             305
#define OP_LIST_FIRST               306
#define OP_LIST_REST                307
#define OP_LIST_LAST                308
#define OP_LIST_LENGTH              309
#define OP_LIST_REF                 310
#define OP_LIST_NULL_P              311
#define OP_LIST_MEMBER_P            312
#define OP_LIST_INDEX               313
#define OP_LIST_SLICE               314
#define OP_LIST_REMOVE              315
#define OP_LIST_CONCAT              316
#define OP_LIST_TO_STRING           317
#define OP_LIST_TO_SET              318
#define OP_SET_P                    340
#define OP_SET_EQ_P                 341
#define OP_SET_NEQ_P                342
#define OP_SET_MEMBER_P             343
#define OP_SET_ADD                  344
#define OP_SET_REMOVE               345
#define OP_SET_LENGTH               346
#define OP_SET_UNION                347
#define OP_SET_INTERSECTION         348
#define OP_SET_DIFFERENCE           349
#define OP_SET_SUBSET_P             350
#define OP_SET_TO_LIST              351
#define OP_RANGE                    380

/* ---------------------------------------------------------------------------
 * Shim state — definitions of the externs declared in menai_vm_shim.h
 * ------------------------------------------------------------------------- */

PyTypeObject *Menai_NoneType       = NULL;
PyTypeObject *Menai_BooleanType    = NULL;
PyTypeObject *Menai_IntegerType    = NULL;
PyTypeObject *Menai_FloatType      = NULL;
PyTypeObject *Menai_ComplexType    = NULL;
PyTypeObject *Menai_StringType     = NULL;
PyTypeObject *Menai_SymbolType     = NULL;
PyTypeObject *Menai_ListType       = NULL;
PyTypeObject *Menai_DictType       = NULL;
PyTypeObject *Menai_SetType        = NULL;
PyTypeObject *Menai_FunctionType   = NULL;
PyTypeObject *Menai_StructTypeType = NULL;
PyTypeObject *Menai_StructType     = NULL;

PyObject *Menai_NONE       = NULL;
PyObject *Menai_TRUE       = NULL;
PyObject *Menai_FALSE      = NULL;
PyObject *Menai_EMPTY_LIST = NULL;
PyObject *Menai_EMPTY_DICT = NULL;
PyObject *Menai_EMPTY_SET  = NULL;

/* ---------------------------------------------------------------------------
 * Module-level state fetched at init
 * ------------------------------------------------------------------------- */

static PyObject *MenaiEvalError_type      = NULL;
static PyObject *MenaiCancelledException_type = NULL;
static PyObject *fn_convert_code_object   = NULL;
static PyObject *fn_convert_value         = NULL;
static PyObject *fn_to_slow               = NULL;
static PyObject *empty_tuple              = NULL;  /* Cached PyTuple_New(0) — used for struct construction */

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

/* ---------------------------------------------------------------------------
 * Borrowed-reference field accessors
 *
 * These return borrowed references — the caller must not Py_DECREF the result.
 * The outer Menai object keeps the inner Python object alive for the duration
 * of any operation that holds a reference to the outer object.
 * ------------------------------------------------------------------------- */

static inline PyObject *menai_get_attr(PyObject *o, const char *name) {
    return PyObject_GetAttrString(o, name);
}

static inline PyObject *menai_integer_value(PyObject *o) {
    return ((MenaiInteger_Object *)o)->value;
}

static inline PyObject *menai_symbol_name(PyObject *o) {
    return ((MenaiSymbol_Object *)o)->name;
}

static inline PyObject *menai_string_value(PyObject *o) {
    return ((MenaiString_Object *)o)->value;
}

/* ---------------------------------------------------------------------------
 * Register array helpers
 * ------------------------------------------------------------------------- */

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
    MenaiInteger_Object *r = (MenaiInteger_Object *)Menai_IntegerType->tp_alloc(Menai_IntegerType, 0);
    if (r) {
        Py_INCREF(py_int);
        r->value = py_int;
    }
    return (PyObject *)r;
}

static inline PyObject *make_float(double v) {
    MenaiFloat_Object *r = (MenaiFloat_Object *)Menai_FloatType->tp_alloc(Menai_FloatType, 0);
    if (r) r->value = v;
    return (PyObject *)r;
}

static inline PyObject *make_complex_from_doubles(double real, double imag) {
    PyObject *pc = PyComplex_FromDoubles(real, imag);
    if (!pc) return NULL;
    MenaiComplex_Object *r = (MenaiComplex_Object *)Menai_ComplexType->tp_alloc(Menai_ComplexType, 0);
    if (r) {
        r->value = pc;
    } else {
        Py_DECREF(pc);
    }
    return (PyObject *)r;
}

static inline PyObject *make_string_from_pyobj(PyObject *py_str) {
    MenaiString_Object *r = (MenaiString_Object *)Menai_StringType->tp_alloc(Menai_StringType, 0);
    if (r) {
        Py_INCREF(py_str);
        r->value = py_str;
    }
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
    MenaiComplex_Object *r = (MenaiComplex_Object *)Menai_ComplexType->tp_alloc(Menai_ComplexType, 0);
    if (r) {
        r->value = py_complex;
    } else {
        Py_DECREF(py_complex);
    }
    return (PyObject *)r;
}

static inline void bool_store(PyObject **regs, int slot, int cond) {
    reg_set(regs, slot, cond ? Menai_TRUE : Menai_FALSE);
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
 * Complex value accessor — returns borrowed reference
 * ------------------------------------------------------------------------- */

static inline PyObject *
menai_complex_value(PyObject *obj)
{
    return ((MenaiComplex_Object *)obj)->value;
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
    menai_raise_eval_errorf("Function '%s' requires %s, got %s", op_name, noun, tn ? PyUnicode_AsUTF8(tn) : "?");
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
static inline int require_function_singular(PyObject *val, const char *op_name) {
    return require_type_impl(IS_MENAI_FUNCTION(val), val, op_name, "a function argument");
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

/* ---------------------------------------------------------------------------
 * Error helpers
 * ------------------------------------------------------------------------- */

PyObject *
menai_raise_eval_error(const char *message)
{
    PyErr_SetString(MenaiEvalError_type, message);
    return NULL;
}

PyObject *
menai_raise_eval_errorf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    PyObject *msg = PyUnicode_FromFormatV(fmt, args);
    va_end(args);
    if (msg == NULL) return NULL;
    PyErr_SetObject(MenaiEvalError_type, msg);
    Py_DECREF(msg);
    return NULL;
}

/* ---------------------------------------------------------------------------
 * Shim init
 * ------------------------------------------------------------------------- */

static int
fetch_type(PyObject *module, const char *name, PyTypeObject **dst)
{
    PyObject *obj = PyObject_GetAttrString(module, name);
    if (obj == NULL) return -1;
    if (!PyType_Check(obj)) {
        PyErr_Format(PyExc_TypeError,
                     "menai_vm_shim_init: %s is not a type", name);
        Py_DECREF(obj);
        return -1;
    }
    *dst = (PyTypeObject *)obj;
    /* Keep the reference alive in the module-level global. */
    return 0;
}

static int
fetch_singleton(PyObject *module, const char *name, PyObject **dst)
{
    PyObject *obj = PyObject_GetAttrString(module, name);
    if (obj == NULL) return -1;
    *dst = obj;
    /* Keep the reference alive in the module-level global. */
    return 0;
}

static int
fetch_callable(PyObject *module, const char *name, PyObject **dst)
{
    PyObject *obj = PyObject_GetAttrString(module, name);
    if (obj == NULL) return -1;
    if (!PyCallable_Check(obj)) {
        PyErr_Format(PyExc_TypeError,
                     "menai_vm_shim_init: %s is not callable", name);
        Py_DECREF(obj);
        return -1;
    }
    *dst = obj;
    return 0;
}

int
menai_vm_shim_init(void)
{
    PyObject *vc = PyImport_ImportModule("menai.menai_value_c");
    if (vc == NULL) return -1;

    if (fetch_type(vc, "MenaiNone",       &Menai_NoneType)       < 0) goto fail;
    if (fetch_type(vc, "MenaiBoolean",    &Menai_BooleanType)    < 0) goto fail;
    if (fetch_type(vc, "MenaiInteger",    &Menai_IntegerType)    < 0) goto fail;
    if (fetch_type(vc, "MenaiFloat",      &Menai_FloatType)      < 0) goto fail;
    if (fetch_type(vc, "MenaiComplex",    &Menai_ComplexType)    < 0) goto fail;
    if (fetch_type(vc, "MenaiString",     &Menai_StringType)     < 0) goto fail;
    if (fetch_type(vc, "MenaiSymbol",     &Menai_SymbolType)     < 0) goto fail;
    if (fetch_type(vc, "MenaiList",       &Menai_ListType)       < 0) goto fail;
    if (fetch_type(vc, "MenaiDict",       &Menai_DictType)       < 0) goto fail;
    if (fetch_type(vc, "MenaiSet",        &Menai_SetType)        < 0) goto fail;
    if (fetch_type(vc, "MenaiFunction",   &Menai_FunctionType)   < 0) goto fail;
    if (fetch_type(vc, "MenaiStructType", &Menai_StructTypeType) < 0) goto fail;
    if (fetch_type(vc, "MenaiStruct",     &Menai_StructType)     < 0) goto fail;

    if (fetch_singleton(vc, "Menai_NONE",          &Menai_NONE)       < 0) goto fail;
    if (fetch_singleton(vc, "Menai_BOOLEAN_TRUE",  &Menai_TRUE)       < 0) goto fail;
    if (fetch_singleton(vc, "Menai_BOOLEAN_FALSE", &Menai_FALSE)      < 0) goto fail;
    if (fetch_singleton(vc, "Menai_LIST_EMPTY",    &Menai_EMPTY_LIST) < 0) goto fail;
    if (fetch_singleton(vc, "Menai_DICT_EMPTY",    &Menai_EMPTY_DICT) < 0) goto fail;
    if (fetch_singleton(vc, "Menai_SET_EMPTY",     &Menai_EMPTY_SET)  < 0) goto fail;

    if (fetch_callable(vc, "convert_code_object", &fn_convert_code_object) < 0) goto fail;
    if (fetch_callable(vc, "convert_value",       &fn_convert_value)       < 0) goto fail;
    if (fetch_callable(vc, "to_slow",             &fn_to_slow)             < 0) goto fail;

    empty_tuple = PyTuple_New(0);
    if (empty_tuple == NULL) goto fail;

    PyObject *err_mod = PyImport_ImportModule("menai.menai_error");
    if (err_mod == NULL) goto fail;
    MenaiEvalError_type = PyObject_GetAttrString(err_mod, "MenaiEvalError");
    MenaiCancelledException_type = PyObject_GetAttrString(err_mod, "MenaiCancelledException");
    Py_DECREF(err_mod);
    if (MenaiEvalError_type == NULL || MenaiCancelledException_type == NULL) goto fail;

    Py_DECREF(vc);
    return 0;

fail:
    Py_DECREF(vc);
    return -1;
}

/* ---------------------------------------------------------------------------
 * Frame struct
 *
 * The C VM maintains a fixed-size stack of Frame structs.  All fields are
 * plain C — no Python objects except code_obj and instructions_obj, which
 * are kept alive by the frame stack but never dereferenced in the hot loop.
 * ------------------------------------------------------------------------- */

typedef struct {
    PyObject       *code_obj;         /* CodeObject — kept alive, not dereferenced in loop */
    PyObject       *instructions_obj; /* array.array — kept alive for instrs pointer */
    uint64_t       *instrs;           /* raw C pointer into the array.array buffer */
    int             code_len;
    int             ip;
    int             base;
    int             return_dest;
    int             is_sentinel;
} Frame;

/* ---------------------------------------------------------------------------
 * CodeObject attribute helpers
 *
 * All read once at frame-setup time, not in the hot loop.
 * Return -1 and set exception on failure.
 * ------------------------------------------------------------------------- */

/*
 * Fill frame fields from a CodeObject.
 * Gets the buffer pointer from code.instructions (an array.array('Q')).
 */
static int
frame_setup(Frame *f, PyObject *code_obj, int base, int return_dest)
{
    /* instructions — array.array('Q') */
    PyObject *instrs_obj = PyObject_GetAttrString(code_obj, "instructions");
    if (instrs_obj == NULL) return -1;

    Py_buffer view;
    if (PyObject_GetBuffer(instrs_obj, &view, PyBUF_SIMPLE) < 0) {
        Py_DECREF(instrs_obj);
        return -1;
    }

    Py_INCREF(code_obj);
    Py_XDECREF(f->code_obj);          /* release any previous owned reference */
    f->code_obj         = code_obj;   /* owned reference (INCREF'd above) */
    f->instructions_obj = instrs_obj; /* we own this ref */
    f->instrs           = (uint64_t *)view.buf;
    f->code_len         = (int)(view.len / sizeof(uint64_t));
    f->ip               = 0;
    f->base             = base;
    f->return_dest      = return_dest;
    f->is_sentinel      = 0;

    PyBuffer_Release(&view);
    /* instrs_obj still alive — buffer backed by it */
    return 0;
}

static void
frame_release(Frame *f)
{
    Py_XDECREF(f->code_obj);
    Py_XDECREF(f->instructions_obj);
    f->instructions_obj = NULL;
    f->instrs           = NULL;
    f->code_obj         = NULL;
}

/* ---------------------------------------------------------------------------
 * CodeObject integer attribute helper
 * ------------------------------------------------------------------------- */

static int
code_get_int(PyObject *code, const char *name, int *out)
{
    PyObject *v = PyObject_GetAttrString(code, name);
    if (v == NULL) return -1;
    long val = PyLong_AsLong(v);
    Py_DECREF(v);
    if (val == -1 && PyErr_Occurred()) return -1;
    *out = (int)val;
    return 0;
}

/* ---------------------------------------------------------------------------
 * Register array helpers
 *
 * The register array is a flat PyObject* array:
 *   regs[depth * max_locals + slot]
 * All slots are initialised to Menai_NONE (borrowed — the singleton is
 * kept alive by the module).  reg_set() manages reference counts correctly.
 * ------------------------------------------------------------------------- */

/*
 * Allocate and initialise the register array.
 * Returns NULL and sets MemoryError on failure.
 */
static PyObject **
regs_alloc(int max_depth, int max_locals)
{
    Py_ssize_t n = (Py_ssize_t)(max_depth + 1) * max_locals;
    PyObject **regs = (PyObject **)PyMem_Malloc(n * sizeof(PyObject *));
    if (regs == NULL) {
        PyErr_NoMemory();
        return NULL;
    }
    for (Py_ssize_t i = 0; i < n; i++) {
        Py_INCREF(Menai_NONE);
        regs[i] = Menai_NONE;  /* owned reference */
    }
    return regs;
}

/*
 * Release all owned references in the register array and free it.
 * Slots that hold something other than Menai_NONE were set via reg_set()
 * and have an owned reference.
 */
static void
regs_free(PyObject **regs, int max_depth, int max_locals)
{
    if (regs == NULL) return;
    Py_ssize_t n = (Py_ssize_t)(max_depth + 1) * max_locals;
    for (Py_ssize_t i = 0; i < n; i++)
        Py_DECREF(regs[i]);  /* every slot is an owned reference */
    PyMem_Free(regs);
}

/* ---------------------------------------------------------------------------
 * max_local_count — mirrors MenaiVM._max_local_count()
 *
 * Walks the code_objects tree and returns the maximum
 * (local_count + outgoing_arg_slots) across all code objects.
 * ------------------------------------------------------------------------- */

static int
max_local_count(PyObject *code)
{
    int local_count = 0, outgoing = 0;
    if (code_get_int(code, "local_count", &local_count) < 0)        return -1;
    if (code_get_int(code, "outgoing_arg_slots", &outgoing) < 0)    return -1;
    int best = local_count + outgoing;

    PyObject *children = PyObject_GetAttrString(code, "code_objects");
    if (children == NULL)
        return -1;

    /* Iterative DFS using a Python list as a stack. */
    PyObject *stack = PyList_New(0);
    if (stack == NULL) {
        Py_DECREF(children);
        return -1;
    }

    Py_ssize_t n = PyList_GET_SIZE(children);
    for (Py_ssize_t i = 0; i < n; i++) {
        if (PyList_Append(stack, PyList_GET_ITEM(children, i)) < 0) {
            Py_DECREF(children);
            Py_DECREF(stack);
            return -1;
        }
    }
    Py_DECREF(children);

    while (PyList_GET_SIZE(stack) > 0) {
        Py_ssize_t last = PyList_GET_SIZE(stack) - 1;
        PyObject *co = PyList_GET_ITEM(stack, last);
        Py_INCREF(co);
        if (PyList_SetSlice(stack, last, last + 1, NULL) < 0) {
            Py_DECREF(co);
            Py_DECREF(stack);
            return -1;
        }

        int lc = 0, oa = 0;
        if (code_get_int(co, "local_count", &lc) < 0 ||
            code_get_int(co, "outgoing_arg_slots", &oa) < 0) {
            Py_DECREF(co);
            Py_DECREF(stack);
            return -1;
        }
        if (lc + oa > best) best = lc + oa;

        PyObject *sub = PyObject_GetAttrString(co, "code_objects");
        Py_DECREF(co);
        if (sub == NULL) {
            Py_DECREF(stack);
            return -1;
        }
        Py_ssize_t m = PyList_GET_SIZE(sub);
        for (Py_ssize_t i = 0; i < m; i++) {
            if (PyList_Append(stack, PyList_GET_ITEM(sub, i)) < 0) {
                Py_DECREF(sub);
                Py_DECREF(stack);
                return -1;
            }
        }
        Py_DECREF(sub);
    }
    Py_DECREF(stack);
    return best;
}

/* ---------------------------------------------------------------------------
 * build_globals — merge constants and prelude into a flat PyObject* dict
 *
 * The C VM looks up globals by name on LOAD_NAME.  We build a Python dict
 * once at the start of execute() and keep it for the duration.
 * ------------------------------------------------------------------------- */

static PyObject *
build_globals(PyObject *constants_dict, PyObject *prelude_dict)
{
    PyObject *globals = PyDict_Copy(constants_dict);
    if (globals == NULL) return NULL;

    if (prelude_dict != Py_None && PyDict_Size(prelude_dict) > 0) {
        if (PyDict_Merge(globals, prelude_dict, 1) < 0) {
            Py_DECREF(globals);
            return NULL;
        }
    }
    return globals;
}

/* ---------------------------------------------------------------------------
 * call_setup — shared logic for CALL and APPLY
 *
 * Sets up new_frame for a call to func_obj with arity arguments already
 * written into regs[callee_base .. callee_base+arity-1].
 *
 * Handles:
 *   - arity checking (fixed and variadic)
 *   - variadic rest-list packing
 *   - capture slot population
 *
 * Returns 0 on success, -1 on error (Python exception set).
 * ------------------------------------------------------------------------- */

static int
call_setup(Frame *new_frame, PyObject *func_obj,
           PyObject **regs, int callee_base, int arity,
           int return_dest)
{
    /* bytecode = func.bytecode */
    PyObject *bytecode = PyObject_GetAttrString(func_obj, "bytecode");
    if (bytecode == NULL) return -1;

    int param_count = 0, is_variadic_int = 0;
    if (code_get_int(bytecode, "param_count", &param_count) < 0) goto fail;

    PyObject *iv = PyObject_GetAttrString(bytecode, "is_variadic");
    if (iv == NULL) goto fail;
    is_variadic_int = PyObject_IsTrue(iv);
    Py_DECREF(iv);
    if (is_variadic_int < 0) goto fail;

    if (is_variadic_int) {
        int min_arity = param_count - 1;
        if (arity < min_arity) {
            PyObject *name = PyObject_GetAttrString(func_obj, "name");
            const char *fname = (name != NULL && name != Py_None)
                                ? PyUnicode_AsUTF8(name) : "<lambda>";
            menai_raise_eval_errorf(
                "Function '%s' expects at least %d argument%s, got %d",
                fname, min_arity, min_arity == 1 ? "" : "s", arity);
            Py_XDECREF(name);
            goto fail;
        }
        /* Pack excess args into a MenaiList for the rest parameter. */
        int rest_count = arity - min_arity;
        PyObject *rest_tuple = PyTuple_New(rest_count);
        if (rest_tuple == NULL) goto fail;
        for (int k = 0; k < rest_count; k++) {
            PyObject *elem = regs[callee_base + min_arity + k];
            Py_INCREF(elem);
            PyTuple_SET_ITEM(rest_tuple, k, elem);
        }
        PyObject *rest_list = PyObject_CallOneArg(
            (PyObject *)Menai_ListType, rest_tuple);
        Py_DECREF(rest_tuple);
        if (rest_list == NULL) goto fail;
        reg_set(regs, callee_base + min_arity, rest_list);
        Py_DECREF(rest_list);

    } else if (arity != param_count) {
        PyObject *name = PyObject_GetAttrString(func_obj, "name");
        const char *fname = (name != NULL && name != Py_None)
                            ? PyUnicode_AsUTF8(name) : "<lambda>";
        menai_raise_eval_errorf(
            "Function '%s' expects %d argument%s, got %d",
            fname, param_count, param_count == 1 ? "" : "s", arity);
        Py_XDECREF(name);
        goto fail;
    }

    /* Populate capture slots: regs[callee_base + param_count + i] */
    {
        PyObject *captured = PyObject_GetAttrString(func_obj, "captured_values");
        if (captured == NULL) goto fail;
        Py_ssize_t ncap = PyList_GET_SIZE(captured);
        for (Py_ssize_t i = 0; i < ncap; i++) {
            PyObject *cv = PyList_GET_ITEM(captured, i);
            PyObject *fast_cv = PyObject_CallOneArg(fn_convert_value, cv);
            if (fast_cv == NULL) {
                Py_DECREF(captured);
                goto fail;
            }
            reg_set(regs, callee_base + param_count + (int)i, fast_cv);
            Py_DECREF(fast_cv);
        }
        Py_DECREF(captured);
    }

    /* Set up the new frame. */
    if (frame_setup(new_frame, bytecode, callee_base, return_dest) < 0)
        goto fail;

    Py_DECREF(bytecode);  /* frame now owns its own ref; drop ours */
    return 0;

fail:
    Py_DECREF(bytecode);
    return -1;
}

static PyObject *execute_loop(PyObject *code, PyObject *globals,
                              PyObject **regs, int max_locals);

/* ---------------------------------------------------------------------------
 * execute_loop — the main dispatch loop
 * ------------------------------------------------------------------------- */

/*
 * Internal execute — called by menai_vm_c_execute after setup.
 * Returns the result value (new reference) or NULL on error.
 * Caller is responsible for calling to_slow() on the result.
 */
static PyObject *
execute_loop(PyObject *code, PyObject *globals,
             PyObject **regs, int max_locals)
{
    /* Frame stack — depth 0 is the sentinel. */
    Frame frames[MAX_FRAME_DEPTH + 1];
    memset(frames, 0, sizeof(frames));
    frames[0].is_sentinel = 1;

    /* Set up frame at depth 1 for the top-level code object. */
    if (frame_setup(&frames[1], code, 0, 0) < 0)
        return NULL;
    frames[1].is_sentinel = 0;

    int frame_depth = 1;
    Frame *frame    = &frames[1];
    int instr_count = 0;

    while (1) {
        /* Cancellation check */
        if (++instr_count >= CANCEL_CHECK_INTERVAL) {
            instr_count = 0;
            if (PyErr_CheckSignals() < 0)
                goto error;
        }

        if (frame->ip >= frame->code_len) {
            menai_raise_eval_error(
                "Frame execution ended without RETURN instruction");
            goto error;
        }

        /* Fetch and decode instruction */
        uint64_t word = frame->instrs[frame->ip++];
        int opcode = (int)((word >> OPCODE_SHIFT) & OPCODE_MASK);
        int dest   = (int)((word >> DEST_SHIFT)   & FIELD_MASK);
        int src0   = (int)((word >> SRC0_SHIFT)   & FIELD_MASK);
        int src1   = (int)((word >> SRC1_SHIFT)   & FIELD_MASK);
        int src2   = (int)( word                  & FIELD_MASK);
        int base   = frame->base;

        switch (opcode) {

        /* ----------------------------------------------------------------- */
        case OP_LOAD_NONE:
            reg_set(regs, base + dest, Menai_NONE);
            break;

        case OP_LOAD_TRUE:
            reg_set(regs, base + dest, Menai_TRUE);
            break;

        case OP_LOAD_FALSE:
            reg_set(regs, base + dest, Menai_FALSE);
            break;

        case OP_LOAD_EMPTY_LIST:
            reg_set(regs, base + dest, Menai_EMPTY_LIST);
            break;

        case OP_LOAD_EMPTY_DICT:
            reg_set(regs, base + dest, Menai_EMPTY_DICT);
            break;

        case OP_LOAD_EMPTY_SET:
            reg_set(regs, base + dest, Menai_EMPTY_SET);
            break;

        case OP_LOAD_CONST: {
            PyObject *constants = PyObject_GetAttrString(frame->code_obj, "constants");
            if (constants == NULL) goto error;
            PyObject *val = PyList_GET_ITEM(constants, src0);
            reg_set(regs, base + dest, val);
            Py_DECREF(constants);
            break;
        }

        case OP_LOAD_NAME: {
            PyObject *names = PyObject_GetAttrString(frame->code_obj, "names");
            if (names == NULL) goto error;
            PyObject *name = PyList_GET_ITEM(names, src0);
            PyObject *val  = PyDict_GetItem(globals, name);
            if (val == NULL) {
                /* Build a rich error with available variable names, matching
                 * the Python VM's error format. */
                PyObject *keys = PyDict_Keys(globals);
                const char *name_str = PyUnicode_AsUTF8(name);
                if (keys != NULL) {
                    Py_ssize_t nk = PyList_GET_SIZE(keys);
                    Py_ssize_t show = nk < 10 ? nk : 10;
                    PyObject *parts = PyList_New(show);
                    if (parts != NULL) {
                        for (Py_ssize_t i = 0; i < show; i++) {
                            PyObject *k = PyList_GET_ITEM(keys, i);
                            Py_INCREF(k);
                            PyList_SET_ITEM(parts, i, k);
                        }
                        PyObject *sep = PyUnicode_FromString(", ");
                        PyObject *joined = PyUnicode_Join(sep, parts);
                        Py_DECREF(sep);
                        Py_DECREF(parts);
                        if (joined != NULL) {
                            menai_raise_eval_errorf(
                                "Undefined variable: '%s'\n  Available variables: %s%s",
                                name_str, PyUnicode_AsUTF8(joined),
                                nk > 10 ? "..." : "");
                            Py_DECREF(joined);
                        } else {
                            menai_raise_eval_errorf("Undefined variable: '%s'", name_str);
                        }
                    } else {
                        menai_raise_eval_errorf("Undefined variable: '%s'", name_str);
                    }
                    Py_DECREF(keys);
                } else {
                    menai_raise_eval_errorf("Undefined variable: '%s'", name_str);
                }
                Py_DECREF(names);
                goto error;
            }
            Py_DECREF(names);
            reg_set(regs, base + dest, val);
            break;
        }

        case OP_MOVE:
            reg_set(regs, base + dest, regs[base + src0]);
            break;

        case OP_JUMP:
            frame->ip = src0;
            break;

        case OP_JUMP_IF_FALSE: {
            PyObject *cond = regs[base + src0];
            if (!IS_MENAI_BOOLEAN(cond)) {
                menai_raise_eval_error("If condition must be boolean");
                goto error;
            }
            if (!menai_boolean_value(cond)) frame->ip = src1;
            break;
        }

        case OP_JUMP_IF_TRUE: {
            PyObject *cond = regs[base + src0];
            if (!IS_MENAI_BOOLEAN(cond)) {
                menai_raise_eval_error("If condition must be boolean");
                goto error;
            }
            if (menai_boolean_value(cond)) frame->ip = src1;
            break;
        }

        case OP_RAISE_ERROR: {
            PyObject *msg = regs[base + src0];
            if (!IS_MENAI_STRING(msg)) {
                menai_raise_eval_error("error: message must be a string");
                goto error;
            }
            PyObject *s = menai_get_attr(msg, "value");
            if (s == NULL) goto error;
            PyErr_SetObject(MenaiEvalError_type, s);
            Py_DECREF(s);
            goto error;
        }

        case OP_RETURN: {
            PyObject *retval = regs[base + src0];
            Py_INCREF(retval);

            int saved_return_dest = frame->return_dest;
            frame_release(frame);
            frame_depth--;
            Frame *caller = &frames[frame_depth];

            if (caller->is_sentinel) {
                /* Top-level return — exit the loop. */
                return retval;
            }

            /* Store result into caller's register window. */
            reg_set(regs, caller->base + saved_return_dest, retval);
            Py_DECREF(retval);

            frame = caller;
            break;
        }

        case OP_CALL: {
            PyObject *raw = regs[base + src0];
            int arity     = src1;

            /* src1 holds arity; arguments are already in the outgoing zone
             * at regs[base + local_count .. base + local_count + arity - 1].
             * We need local_count to find callee_base. */
            int local_count = 0;
            if (code_get_int(frame->code_obj, "local_count", &local_count) < 0) goto error;
            int callee_base = base + local_count;

            if (IS_MENAI_FUNCTION(raw)) {
                if (frame_depth >= MAX_FRAME_DEPTH) {
                    menai_raise_eval_error("Maximum call depth exceeded");
                    goto error;
                }
                frame_depth++;
                Frame *new_frame = &frames[frame_depth];
                new_frame->return_dest = dest;

                if (call_setup(new_frame, raw, regs, callee_base,
                               arity, dest) < 0) {
                    frame_depth--;
                    goto error;
                }
                frame = new_frame;

            } else if (IS_MENAI_STRUCTTYPE(raw)) {
                /* Struct constructor call */
                PyObject *field_names = PyObject_GetAttrString(raw, "field_names");
                if (field_names == NULL) goto error;
                Py_ssize_t n_fields = PyTuple_GET_SIZE(field_names);
                Py_DECREF(field_names);
                if (arity != (int)n_fields) {
                    PyObject *sname = PyObject_GetAttrString(raw, "name");
                    menai_raise_eval_errorf(
                        "Struct constructor '%s' called with wrong number of arguments",
                        sname ? PyUnicode_AsUTF8(sname) : "?");
                    Py_XDECREF(sname);
                    goto error;
                }
                PyObject *fields = PyTuple_New(n_fields);
                if (fields == NULL) goto error;
                for (int i = 0; i < (int)n_fields; i++) {
                    PyObject *fv = regs[callee_base + i];
                    Py_INCREF(fv);
                    PyTuple_SET_ITEM(fields, i, fv);
                }
                PyObject *kwargs = Py_BuildValue("{sOsO}", "struct_type", raw, "fields", fields);
                Py_DECREF(fields);
                if (kwargs == NULL) goto error;
                PyObject *instance = PyObject_Call((PyObject *)Menai_StructType, empty_tuple, kwargs);
                Py_DECREF(kwargs);
                if (instance == NULL) goto error;
                reg_set(regs, base + dest, instance);
                Py_DECREF(instance);

            } else {
                menai_raise_eval_error("Cannot call non-function value");
                goto error;
            }
            break;
        }

        case OP_TAIL_CALL: {
            PyObject *raw = regs[base + src0];
            int n_args = src1;
            /* Take an owned reference before the arg-moving loop.
             * The loop may overwrite regs[base+src0] if src0 < n_args,
             * which would decrement raw's refcount to zero and free it. */
            Py_INCREF(raw);

            int local_count = 0;
            if (code_get_int(frame->code_obj, "local_count", &local_count) < 0) {
                Py_DECREF(raw);
                goto error;
            }

            if (IS_MENAI_FUNCTION(raw)) {
                /* Move outgoing args down to base+0..n_args-1 in place. */
                for (int i = 0; i < n_args; i++) {
                    PyObject *v = regs[base + local_count + i];
                    reg_set(regs, base + i, v);
                }

                /* Reuse current frame — release old instructions first. */
                Py_XDECREF(frame->instructions_obj);
                frame->instructions_obj = NULL;
                frame->instrs           = NULL;

                int saved_return_dest = frame->return_dest;
                if (call_setup(frame, raw, regs, base, n_args, saved_return_dest) < 0) {
                    Py_DECREF(raw);
                    goto error;
                }
                Py_DECREF(raw);
            } else if (IS_MENAI_STRUCTTYPE(raw)) {
                PyObject *field_names = PyObject_GetAttrString(raw, "field_names");
                if (field_names == NULL) {
                    Py_DECREF(raw);
                    goto error;
                }
                Py_ssize_t n_fields = PyTuple_GET_SIZE(field_names);
                Py_DECREF(field_names);
                if (n_args != (int)n_fields) {
                    PyObject *sname = PyObject_GetAttrString(raw, "name");
                    menai_raise_eval_errorf(
                        "Struct constructor '%s' called with wrong number of arguments",
                        sname ? PyUnicode_AsUTF8(sname) : "?");
                    Py_XDECREF(sname);
                    Py_DECREF(raw);
                    goto error;
                }
                PyObject *fields = PyTuple_New(n_fields);
                if (fields == NULL) {
                    Py_DECREF(raw);
                    goto error;
                }
                for (int i = 0; i < (int)n_fields; i++) {
                    PyObject *fv = regs[base + local_count + i];
                    Py_INCREF(fv);
                    PyTuple_SET_ITEM(fields, i, fv);
                }
                PyObject *kwargs = Py_BuildValue("{sOsO}", "struct_type", raw, "fields", fields);
                Py_DECREF(fields);
                if (kwargs == NULL) goto error;
                PyObject *instance = PyObject_Call(
                    (PyObject *)Menai_StructType, empty_tuple, kwargs);
                Py_DECREF(kwargs);
                if (instance == NULL) {
                    Py_DECREF(raw);
                    goto error;
                }

                /* Tail-return the struct: pop frame and deliver to caller. */
                PyObject *retval = instance;
                int saved_return_dest = frame->return_dest;
                frame_release(frame);
                frame_depth--;
                Frame *caller = &frames[frame_depth];
                if (caller->is_sentinel) {
                    Py_DECREF(raw);
                    return retval;
                }
                reg_set(regs, caller->base + saved_return_dest, retval);
                Py_DECREF(retval);
                Py_DECREF(raw);
                frame = caller;
            } else {
                Py_DECREF(raw);
                menai_raise_eval_error("Cannot call non-function value");
                goto error;
            }
            break;
        }

        case OP_NONE_P:
            bool_store(regs, base + dest, IS_MENAI_NONE(regs[base + src0]));
            break;

        case OP_BOOLEAN_P:
            bool_store(regs, base + dest, IS_MENAI_BOOLEAN(regs[base + src0]));
            break;

        case OP_BOOLEAN_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_boolean(a, "boolean=?")) goto error;
            if (!require_boolean(b, "boolean=?")) goto error;
            bool_store(regs, base + dest, menai_boolean_value(a) == menai_boolean_value(b));
            break;
        }

        case OP_BOOLEAN_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_boolean(a, "boolean!=?")) goto error;
            if (!require_boolean(b, "boolean!=?")) goto error;
            bool_store(regs, base + dest, menai_boolean_value(a) != menai_boolean_value(b));
            break;
        }

        case OP_BOOLEAN_NOT: {
            PyObject *a = regs[base + src0];
            if (!require_boolean(a, "boolean-not")) goto error;
            bool_store(regs, base + dest, !menai_boolean_value(a));
            break;
        }

        case OP_SYMBOL_P:
            bool_store(regs, base + dest, IS_MENAI_SYMBOL(regs[base + src0]));
            break;

        case OP_SYMBOL_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_symbol_pair(a, b, "symbol=?")) goto error;
            PyObject *na = menai_symbol_name(a);
            if (na == NULL) goto error;
            PyObject *nb = menai_symbol_name(b);
            if (nb == NULL) goto error;
            int eq = PyObject_RichCompareBool(na, nb, Py_EQ);
            if (eq < 0) goto error;
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_SYMBOL_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_symbol_pair(a, b, "symbol!=?")) goto error;
            PyObject *na = menai_symbol_name(a);
            if (na == NULL) goto error;
            PyObject *nb = menai_symbol_name(b);
            if (nb == NULL) goto error;
            int neq = PyObject_RichCompareBool(na, nb, Py_NE);
            if (neq < 0) goto error;
            bool_store(regs, base + dest, neq);
            break;
        }

        case OP_SYMBOL_TO_STRING: {
            PyObject *a = regs[base + src0];
            if (!require_symbol(a, "symbol->string")) goto error;
            PyObject *name = menai_symbol_name(a);
            if (name == NULL) goto error;
            PyObject *r = make_string_from_pyobj(name);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_FUNCTION_P:
            bool_store(regs, base + dest, IS_MENAI_FUNCTION(regs[base + src0]));
            break;

        case OP_FUNCTION_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_function(a, "function=?")) goto error;
            if (!require_function(b, "function=?")) goto error;
            bool_store(regs, base + dest, a == b);
            break;
        }

        case OP_FUNCTION_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_function(a, "function!=?")) goto error;
            if (!require_function(b, "function!=?")) goto error;
            bool_store(regs, base + dest, a != b);
            break;
        }

        case OP_FUNCTION_MIN_ARITY: {
            PyObject *f = regs[base + src0];
            if (!require_function_singular(f, "function-min-arity")) goto error;
            PyObject *bc = PyObject_GetAttrString(f, "bytecode");
            if (bc == NULL) goto error;
            int pc = 0, is_var = 0;
            int ok = (code_get_int(bc, "param_count", &pc) == 0);
            if (ok) {
                PyObject *iv = PyObject_GetAttrString(bc, "is_variadic");
                if (iv) {
                    is_var = PyObject_IsTrue(iv);
                    Py_DECREF(iv);
                    if (is_var < 0) ok = 0;
                }
                else ok = 0;
            }
            Py_DECREF(bc);
            if (!ok) goto error;
            int min_a = is_var ? pc - 1 : pc;
            PyObject *r = PyLong_FromLong(min_a);
            if (r == NULL) goto error;
            PyObject *_r = make_integer_value(r);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FUNCTION_VARIADIC_P: {
            PyObject *f = regs[base + src0];
            if (!require_function_singular(f, "function-variadic?")) goto error;
            PyObject *bc = PyObject_GetAttrString(f, "bytecode");
            if (bc == NULL) goto error;
            PyObject *iv = PyObject_GetAttrString(bc, "is_variadic");
            Py_DECREF(bc);
            if (iv == NULL) goto error;
            int is_var = PyObject_IsTrue(iv);
            Py_DECREF(iv);
            if (is_var < 0) goto error;
            bool_store(regs, base + dest, is_var);
            break;
        }

        case OP_FUNCTION_ACCEPTS_P: {
            PyObject *f = regs[base + src0];
            PyObject *n_obj = regs[base + src1];
            if (!require_function_singular(f, "function-accepts?")) goto error;
            if (!require_integer(n_obj, "function-accepts?")) goto error;
            PyObject *bc = PyObject_GetAttrString(f, "bytecode");
            if (bc == NULL) goto error;
            int pc = 0, is_var = 0;
            int ok = (code_get_int(bc, "param_count", &pc) == 0);
            if (ok) {
                PyObject *iv = PyObject_GetAttrString(bc, "is_variadic");
                if (iv) {
                    is_var = PyObject_IsTrue(iv);
                    Py_DECREF(iv);
                    if (is_var < 0) ok = 0;
                }
                else ok = 0;
            }
            Py_DECREF(bc);
            if (!ok) goto error;
            PyObject *n_py = menai_integer_value(n_obj);
            if (n_py == NULL) goto error;
            long n = PyLong_AsLong(n_py);
            if (n == -1 && PyErr_Occurred()) goto error;
            int accepts = is_var ? (n >= pc - 1) : (n == pc);
            bool_store(regs, base + dest, accepts);
            break;
        }

        case OP_INTEGER_P:
            bool_store(regs, base + dest, IS_MENAI_INTEGER(regs[base + src0]));
            break;


        case OP_INTEGER_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer=?")) goto error;
            if (!require_integer(b, "integer=?")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(av, bv, Py_EQ));
            break;
        }

        case OP_INTEGER_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer!=?")) goto error;
            if (!require_integer(b, "integer!=?")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(av, bv, Py_NE));
            break;
        }

        case OP_INTEGER_LT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer<?")) goto error;
            if (!require_integer(b, "integer<?")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(av, bv, Py_LT));
            break;
        }

        case OP_INTEGER_GT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer>?")) goto error;
            if (!require_integer(b, "integer>?")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(av, bv, Py_GT));
            break;
        }

        case OP_INTEGER_LTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer<=?")) goto error;
            if (!require_integer(b, "integer<=?")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(av, bv, Py_LE));
            break;
        }

        case OP_INTEGER_GTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer>=?")) goto error;
            if (!require_integer(b, "integer>=?")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(av, bv, Py_GE));
            break;
        }

        case OP_INTEGER_ABS: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-abs")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *_r = make_integer_value(PyNumber_Absolute(av));
            if (!_r) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_NEG: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-neg")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *_r = make_integer_value(PyNumber_Negative(av));
            if (!_r) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_BIT_NOT: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-bit-not")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *_r = make_integer_value(PyNumber_Invert(av));
            if (!_r) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_ADD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer+")) goto error;
            if (!require_integer(b, "integer+")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            PyObject *_r = make_integer_value(PyNumber_Add(av, bv));
            if (!_r) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_SUB: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-")) goto error;
            if (!require_integer(b, "integer-")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            PyObject *_r = make_integer_value(PyNumber_Subtract(av, bv));
            if (!_r) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_MUL: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer*")) goto error;
            if (!require_integer(b, "integer*")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            PyObject *_r = make_integer_value(PyNumber_Multiply(av, bv));
            if (!_r) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_DIV: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer/")) goto error;
            if (!require_integer(b, "integer/")) goto error;
            PyObject *bv = menai_integer_value(b);
            if (bv == NULL) goto error;
            PyObject *_zero = PyLong_FromLong(0);
            if (_zero == NULL) goto error;
            int _is_zero = PyObject_RichCompareBool(bv, _zero, Py_EQ);
            Py_DECREF(_zero);
            if (_is_zero < 0) goto error;
            if (_is_zero) {
                menai_raise_eval_error("Division by zero in 'integer/'");
                goto error;
            }
            PyObject *av = menai_integer_value(a);
            if (av == NULL) goto error;
            PyObject *_res = PyNumber_FloorDivide(av, bv);
            PyObject *_r = make_integer_value(_res);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_MOD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer%")) goto error;
            if (!require_integer(b, "integer%")) goto error;
            PyObject *bv = menai_integer_value(b);
            if (bv == NULL) goto error;
            PyObject *_zero = PyLong_FromLong(0);
            if (_zero == NULL) goto error;
            int _is_zero = PyObject_RichCompareBool(bv, _zero, Py_EQ);
            Py_DECREF(_zero);
            if (_is_zero < 0) goto error;
            if (_is_zero) {
                menai_raise_eval_error("Modulo by zero in 'integer%'");
                goto error;
            }
            PyObject *av = menai_integer_value(a);
            if (av == NULL) goto error;
            PyObject *_res = PyNumber_Remainder(av, bv);
            PyObject *_r = make_integer_value(_res);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_EXPN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-expn")) goto error;
            if (!require_integer(b, "integer-expn")) goto error;
            PyObject *bv = menai_integer_value(b);
            if (bv == NULL) goto error;
            PyObject *_zero = PyLong_FromLong(0);
            if (_zero == NULL) goto error;
            int _is_neg = PyObject_RichCompareBool(bv, _zero, Py_LT);
            Py_DECREF(_zero);
            if (_is_neg < 0) goto error;
            if (_is_neg) {
                menai_raise_eval_error("Function 'integer-expn' requires a non-negative exponent");
                goto error;
            }
            PyObject *av = menai_integer_value(a);
            if (av == NULL) goto error;
            PyObject *_res = PyNumber_Power(av, bv, Py_None);
            PyObject *_r = make_integer_value(_res);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_BIT_OR: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-or")) goto error;
            if (!require_integer(b, "integer-bit-or")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            PyObject *_r = make_integer_value(PyNumber_Or(av, bv));
            if (!_r) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_BIT_AND: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-and")) goto error;
            if (!require_integer(b, "integer-bit-and")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            PyObject *_r = make_integer_value(PyNumber_And(av, bv));
            if (!_r) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_BIT_XOR: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-xor")) goto error;
            if (!require_integer(b, "integer-bit-xor")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            PyObject *_r = make_integer_value(PyNumber_Xor(av, bv));
            if (!_r) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_BIT_SHIFT_LEFT: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-shift-left")) goto error;
            if (!require_integer(b, "integer-bit-shift-left")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            PyObject *_r = make_integer_value(PyNumber_Lshift(av, bv));
            if (!_r) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_BIT_SHIFT_RIGHT: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-shift-right")) goto error;
            if (!require_integer(b, "integer-bit-shift-right")) goto error;
            PyObject *av = menai_integer_value(a);
            if (!av) goto error;
            PyObject *bv = menai_integer_value(b);
            if (!bv) goto error;
            PyObject *_r = make_integer_value(PyNumber_Rshift(av, bv));
            if (!_r) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_MIN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-min")) goto error;
            if (!require_integer(b, "integer-min")) goto error;
            PyObject *_av = menai_integer_value(a);
            if (_av == NULL) goto error;
            PyObject *_bv = menai_integer_value(b);
            if (_bv == NULL) goto error;
            int lt = PyObject_RichCompareBool(_av, _bv, Py_LE);
            if (lt < 0) goto error;
            reg_set(regs, base + dest, lt ? a : b);
            break;
        }

        case OP_INTEGER_MAX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-max")) goto error;
            if (!require_integer(b, "integer-max")) goto error;
            PyObject *_av = menai_integer_value(a);
            if (_av == NULL) goto error;
            PyObject *_bv = menai_integer_value(b);
            if (_bv == NULL) goto error;
            int gt = PyObject_RichCompareBool(_av, _bv, Py_GE);
            if (gt < 0) goto error;
            reg_set(regs, base + dest, gt ? a : b);
            break;
        }

        case OP_INTEGER_TO_FLOAT: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer->float")) goto error;
            PyObject *_av = menai_integer_value(a);
            if (_av == NULL) goto error;
            double d = PyLong_AsDouble(_av);
            if (d == -1.0 && PyErr_Occurred()) goto error;
            PyObject *_r = make_float(d);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_INTEGER_TO_COMPLEX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer->complex")) goto error;
            if (!require_integer(b, "integer->complex")) goto error;
            PyObject *_av = menai_integer_value(a);
            if (_av == NULL) goto error;
            double re = PyLong_AsDouble(_av);
            if (re == -1.0 && PyErr_Occurred()) goto error;
            PyObject *_bv = menai_integer_value(b);
            if (_bv == NULL) goto error;
            double im = PyLong_AsDouble(_bv);
            if (im == -1.0 && PyErr_Occurred()) goto error;
            PyObject *r = make_complex_from_doubles(re, im);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_INTEGER_TO_STRING: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer->string")) goto error;
            if (!require_integer(b, "integer->string")) goto error;
            PyObject *_bv = menai_integer_value(b);
            if (_bv == NULL) goto error;
            long radix = PyLong_AsLong(_bv);
            if (radix == -1 && PyErr_Occurred()) goto error;
            if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
                menai_raise_eval_errorf("integer->string: radix must be 2, 8, 10, or 16, got %ld", radix);
                goto error;
            }
            PyObject *av = menai_integer_value(a);
            if (av == NULL) goto error;
            PyObject *py_str;
            if (radix == 10) {
                py_str = PyObject_Str(av);
            } else {
                /* Use Python's built-in format for other bases */
                const char *fmt = (radix == 2) ? "b" : (radix == 8) ? "o" : "x";
                PyObject *_fmt_str = PyUnicode_FromString(fmt);
                py_str = PyObject_Format(av, _fmt_str);
                Py_DECREF(_fmt_str);
            }
            if (py_str == NULL) goto error;
            PyObject *r = make_string_from_pyobj(py_str);
            Py_DECREF(py_str);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_INTEGER_CODEPOINT_TO_STRING: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-codepoint->string")) goto error;
            PyObject *_av = menai_integer_value(a);
            if (_av == NULL) goto error;
            long cp = PyLong_AsLong(_av);
            if (cp == -1 && PyErr_Occurred()) goto error;
            if (cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF)) {
                menai_raise_eval_errorf(
                    "integer-codepoint->string: invalid Unicode scalar value %ld", cp);
                goto error;
            }
            PyObject *py_str = PyUnicode_FromOrdinal((int)cp);
            if (py_str == NULL) goto error;
            PyObject *r = make_string_from_pyobj(py_str);
            Py_DECREF(py_str);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_FLOAT_P:
            bool_store(regs, base + dest, IS_MENAI_FLOAT(regs[base + src0]));
            break;

        case OP_FLOAT_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float=?")) goto error;
            if (!require_float(b, "float=?")) goto error;
            bool_store(regs, base + dest, menai_float_value(a) == menai_float_value(b));
            break;
        }

        case OP_FLOAT_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float!=?")) goto error;
            if (!require_float(b, "float!=?")) goto error;
            bool_store(regs, base + dest, menai_float_value(a) != menai_float_value(b));
            break;
        }

        case OP_FLOAT_LT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float<?")) goto error;
            if (!require_float(b, "float<?")) goto error;
            bool_store(regs, base + dest, menai_float_value(a) < menai_float_value(b));
            break;
        }

        case OP_FLOAT_GT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float>?")) goto error;
            if (!require_float(b, "float>?")) goto error;
            bool_store(regs, base + dest, menai_float_value(a) > menai_float_value(b));
            break;
        }

        case OP_FLOAT_LTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float<=?")) goto error;
            if (!require_float(b, "float<=?")) goto error;
            bool_store(regs, base + dest, menai_float_value(a) <= menai_float_value(b));
            break;
        }

        case OP_FLOAT_GTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float>=?")) goto error;
            if (!require_float(b, "float>=?")) goto error;
            bool_store(regs, base + dest, menai_float_value(a) >= menai_float_value(b));
            break;
        }

        case OP_FLOAT_NEG: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-neg")) goto error;
            PyObject *_r = make_float(-menai_float_value(a));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_ABS: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-abs")) goto error;
            double v = menai_float_value(a);
            {
                PyObject *_r = make_float(fabs(v));
                if (_r == NULL) goto error;
                reg_set(regs, base + dest, _r);
                Py_DECREF(_r);
            }
            break;
        }

        case OP_FLOAT_ADD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float+")) goto error;
            if (!require_float(b, "float+")) goto error;
            PyObject *_r = make_float(menai_float_value(a) + menai_float_value(b));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_SUB: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float-")) goto error;
            if (!require_float(b, "float-")) goto error;
            PyObject *_r = make_float(menai_float_value(a) - menai_float_value(b));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_MUL: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float*")) goto error;
            if (!require_float(b, "float*")) goto error;
            PyObject *_r = make_float(menai_float_value(a) * menai_float_value(b));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_DIV: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float/")) goto error;
            if (!require_float(b, "float/")) goto error;
            double bv = menai_float_value(b);
            if (bv == 0.0) {
                menai_raise_eval_error("Division by zero in 'float/'");
                goto error;
            }
            PyObject *_r = make_float(menai_float_value(a) / bv);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_FLOOR_DIV: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float//")) goto error;
            if (!require_float(b, "float//")) goto error;
            double bv = menai_float_value(b);
            if (bv == 0.0) {
                menai_raise_eval_error("Division by zero in 'float//'");
                goto error;
            }
            PyObject *_r = make_float(floor(menai_float_value(a) / bv));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_MOD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float%")) goto error;
            if (!require_float(b, "float%")) goto error;
            double bv = menai_float_value(b);
            if (bv == 0.0) {
                menai_raise_eval_error("Modulo by zero in 'float%'");
                goto error;
            }
            PyObject *_r = make_float(fmod(menai_float_value(a), bv));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_EXP: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-exp")) goto error;
            PyObject *_r = make_float(exp(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_EXPN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float-expn")) goto error;
            if (!require_float(b, "float-expn")) goto error;
            PyObject *_r = make_float(pow(menai_float_value(a), menai_float_value(b)));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_LOG: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-log")) goto error;
            double v = menai_float_value(a);
            if (v < 0.0) {
                menai_raise_eval_error("float-log: argument must be non-negative");
                goto error;
            }
            PyObject *_r = make_float(v == 0.0 ? -INFINITY : log(v));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_LOG10: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-log10")) goto error;
            double v = menai_float_value(a);
            if (v < 0.0) {
                menai_raise_eval_error("float-log10: argument must be non-negative");
                goto error;
            }
            PyObject *_r = make_float(v == 0.0 ? -INFINITY : log10(v));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_LOG2: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-log2")) goto error;
            double v = menai_float_value(a);
            if (v < 0.0) {
                menai_raise_eval_error("float-log2: argument must be non-negative");
                goto error;
            }
            PyObject *_r = make_float(v == 0.0 ? -INFINITY : log2(v));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_LOGN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float-logn")) goto error;
            if (!require_float(b, "float-logn")) goto error;
            double av = menai_float_value(a), bv = menai_float_value(b);
            if (bv <= 0.0 || bv == 1.0) {
                menai_raise_eval_error("Function 'float-logn' requires a positive base not equal to 1");
                goto error;
            }
            if (av < 0.0) {
                menai_raise_eval_error("float-logn: argument must be non-negative");
                goto error;
            }
            PyObject *_r = make_float(av == 0.0 ? -INFINITY : log(av) / log(bv));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_SIN: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-sin")) goto error;
            PyObject *_r = make_float(sin(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_COS: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-cos")) goto error;
            PyObject *_r = make_float(cos(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_TAN: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-tan")) goto error;
            PyObject *_r = make_float(tan(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_SQRT: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-sqrt")) goto error;
            double v = menai_float_value(a);
            if (v < 0.0) {
                menai_raise_eval_error("float-sqrt: argument must be non-negative");
                goto error;
            }
            PyObject *_r = make_float(sqrt(v));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_FLOOR: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-floor")) goto error;
            PyObject *_r = make_float(floor(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_CEIL: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-ceil")) goto error;
            PyObject *_r = make_float(ceil(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_ROUND: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-round")) goto error;
            PyObject *_r = make_float(round(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_MIN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float-min")) goto error;
            if (!require_float(b, "float-min")) goto error;
            double av = menai_float_value(a), bv = menai_float_value(b);
            PyObject *_r = make_float(av <= bv ? av : bv);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_MAX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float-max")) goto error;
            if (!require_float(b, "float-max")) goto error;
            double av = menai_float_value(a), bv = menai_float_value(b);
            PyObject *_r = make_float(av >= bv ? av : bv);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_TO_INTEGER: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float->integer")) goto error;
            double v = menai_float_value(a);
            PyObject *py_int = PyLong_FromDouble(trunc(v));
            if (py_int == NULL) goto error;
            PyObject *_r = make_integer_value(py_int);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_FLOAT_TO_COMPLEX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float->complex")) goto error;
            if (!require_float(b, "float->complex")) goto error;
            PyObject *r = make_complex_from_doubles(menai_float_value(a), menai_float_value(b));
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_FLOAT_TO_STRING: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float->string")) goto error;
            PyObject *_pf = PyFloat_FromDouble(menai_float_value(a));
            if (_pf == NULL) goto error;
            PyObject *py_str = PyObject_Str(_pf);
            Py_DECREF(_pf);
            if (py_str == NULL) goto error;
            PyObject *r = make_string_from_pyobj(py_str);
            Py_DECREF(py_str);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_MAKE_CLOSURE: {
            /*
             * MAKE_CLOSURE dest, src0:
             * src0 is the index into code_objects of the child CodeObject.
             * Creates a MenaiFunction with captured_values pre-allocated to
             * None, ready for PATCH_CLOSURE to fill in.
             */
            PyObject *code_objects = PyObject_GetAttrString(frame->code_obj, "code_objects");
            if (code_objects == NULL) goto error;
            PyObject *child_code = PyList_GET_ITEM(code_objects, src0);
            /* child_code is borrowed from code_objects list */

            PyObject *param_names = PyObject_GetAttrString(child_code, "param_names");
            if (param_names == NULL) {
                Py_DECREF(code_objects);
                goto error;
            }
            PyObject *name = PyObject_GetAttrString(child_code, "name");
            if (name == NULL) {
                Py_DECREF(param_names);
                Py_DECREF(code_objects);
                goto error;
            }
            PyObject *is_var = PyObject_GetAttrString(child_code, "is_variadic");
            if (is_var == NULL) {
                Py_DECREF(name);
                Py_DECREF(param_names);
                Py_DECREF(code_objects);
                goto error;
            }
            PyObject *free_vars = PyObject_GetAttrString(child_code, "free_vars");
            if (free_vars == NULL) {
                Py_DECREF(is_var);
                Py_DECREF(name);
                Py_DECREF(param_names);
                Py_DECREF(code_objects);
                goto error;
            }

            Py_ssize_t ncap = PyList_GET_SIZE(free_vars);
            Py_DECREF(free_vars);

            /* Build captured_values list pre-filled with None */
            PyObject *cap_list = PyList_New(ncap);
            if (cap_list == NULL) {
                Py_DECREF(is_var);
                Py_DECREF(name);
                Py_DECREF(param_names);
                Py_DECREF(code_objects);
                goto error;
            }
            for (Py_ssize_t i = 0; i < ncap; i++) {
                Py_INCREF(Py_None);
                PyList_SET_ITEM(cap_list, i, Py_None);
            }

            /* MenaiFunction(parameters, name, bytecode, captured_values, is_variadic) */
            PyObject *func = PyObject_CallFunctionObjArgs(
                (PyObject *)Menai_FunctionType,
                param_names, name, child_code, cap_list, is_var, NULL);
            Py_DECREF(cap_list);
            Py_DECREF(is_var);
            Py_DECREF(name);
            Py_DECREF(param_names);
            Py_DECREF(code_objects);
            if (func == NULL) goto error;
            reg_set(regs, base + dest, func);
            Py_DECREF(func);
            break;
        }

        case OP_PATCH_CLOSURE: {
            /*
             * PATCH_CLOSURE src0, src1, src2:
             * src0 = closure register, src1 = capture slot index, src2 = value register.
             */
            PyObject *closure = regs[base + src0];
            if (!IS_MENAI_FUNCTION(closure)) {
                menai_raise_eval_error("PATCH_CLOSURE requires a function");
                goto error;
            }
            PyObject *cap_list = PyObject_GetAttrString(closure, "captured_values");
            if (cap_list == NULL) goto error;
            PyObject *val = regs[base + src2];
            Py_INCREF(val);
            int set_ok = PyList_SetItem(cap_list, src1, val); /* steals val ref */
            Py_DECREF(cap_list);
            if (set_ok < 0) goto error;
            break;
        }

        case OP_APPLY: {
            /*
             * APPLY dest, src0, src1:
             * src0 = function register, src1 = arg_list register.
             * Scatters the list into the callee's register window and pushes a frame.
             */
            PyObject *raw_func = regs[base + src0];
            PyObject *raw_args = regs[base + src1];

            if (!IS_MENAI_LIST(raw_args)) {
                menai_raise_eval_error("apply: second argument must be a list");
                goto error;
            }

            PyObject *elements = ((MenaiList_Object *)raw_args)->elements;
            int arity = (int)PyTuple_GET_SIZE(elements);

            if (IS_MENAI_FUNCTION(raw_func)) {
                if (frame_depth >= MAX_FRAME_DEPTH) {
                    menai_raise_eval_error("Maximum call depth exceeded");
                    goto error;
                }

                int local_count = 0;
                if (code_get_int(frame->code_obj, "local_count", &local_count) < 0) {
                    goto error;
                }
                int callee_base = base + local_count;

                /* Scatter list elements into the callee window */
                for (int i = 0; i < arity; i++)
                    reg_set(regs, callee_base + i, PyTuple_GET_ITEM(elements, i));

                frame_depth++;
                Frame *new_frame = &frames[frame_depth];
                new_frame->return_dest = dest;
                if (call_setup(new_frame, raw_func, regs, callee_base, arity, dest) < 0) {
                    frame_depth--;
                    goto error;
                }
                frame = new_frame;

            } else if (IS_MENAI_STRUCTTYPE(raw_func)) {
                PyObject *field_names = PyObject_GetAttrString(raw_func, "field_names");
                if (field_names == NULL) goto error;
                Py_ssize_t n_fields = PyTuple_GET_SIZE(field_names);
                Py_DECREF(field_names);
                if (arity != (int)n_fields) {
                    menai_raise_eval_error("Struct constructor called with wrong number of arguments");
                    goto error;
                }
                PyObject *fields = PyTuple_New(n_fields);
                if (fields == NULL) goto error;
                for (int i = 0; i < (int)n_fields; i++) {
                    PyObject *fv = PyTuple_GET_ITEM(elements, i);
                    Py_INCREF(fv);
                    PyTuple_SET_ITEM(fields, i, fv);
                }
                PyObject *kwargs = Py_BuildValue("{sOsO}", "struct_type", raw_func, "fields", fields);
                Py_DECREF(fields);
                if (kwargs == NULL) goto error;
                PyObject *instance = PyObject_Call((PyObject *)Menai_StructType, empty_tuple, kwargs);
                Py_DECREF(kwargs);
                if (instance == NULL) goto error;
                reg_set(regs, base + dest, instance);
                Py_DECREF(instance);
            } else {
                menai_raise_eval_error("apply: first argument must be a function");
                goto error;
            }
            break;
        }

        case OP_TAIL_APPLY: {
            /*
             * TAIL_APPLY src0, src1:
             * src0 = function register, src1 = arg_list register.
             * Reuses current frame (tail position).
             */
            PyObject *raw_func = regs[base + src0];
            PyObject *raw_args = regs[base + src1];
            /* Own raw_func before the scatter loop which may overwrite its slot. */
            /* Own raw_args for the same reason — src1 may be < arity. */
            Py_INCREF(raw_func);
            Py_INCREF(raw_args);

            if (!IS_MENAI_LIST(raw_args)) {
                Py_DECREF(raw_func);
                Py_DECREF(raw_args);
                menai_raise_eval_error("apply: second argument must be a list");
                goto error;
            }

            PyObject *elements = ((MenaiList_Object *)raw_args)->elements;
            int arity = (int)PyTuple_GET_SIZE(elements);

            if (IS_MENAI_FUNCTION(raw_func)) {
                /* Scatter args into base+0..arity-1 (reusing current frame's base) */
                for (int i = 0; i < arity; i++) reg_set(regs, base + i, PyTuple_GET_ITEM(elements, i));
                Py_DECREF(raw_args);

                /* Release old frame instructions, reuse frame */
                Py_XDECREF(frame->instructions_obj);
                frame->instructions_obj = NULL;
                frame->instrs = NULL;

                int saved_return_dest = frame->return_dest;
                if (call_setup(frame, raw_func, regs, base, arity, saved_return_dest) < 0) {
                    Py_DECREF(raw_func);
                    goto error;
                }
                Py_DECREF(raw_func);

            } else if (IS_MENAI_STRUCTTYPE(raw_func)) {
                PyObject *field_names = PyObject_GetAttrString(raw_func, "field_names");
                if (field_names == NULL) {
                    Py_DECREF(raw_args);
                    Py_DECREF(raw_func);
                    goto error;
                }
                Py_ssize_t n_fields = PyTuple_GET_SIZE(field_names);
                Py_DECREF(field_names);
                if (arity != (int)n_fields) {
                    Py_DECREF(raw_func);
                    Py_DECREF(raw_args);
                    menai_raise_eval_error("Struct constructor called with wrong number of arguments");
                    goto error;
                }
                PyObject *fields = PyTuple_New(n_fields);
                if (fields == NULL) {
                    Py_DECREF(raw_args);
                    Py_DECREF(raw_func);
                    goto error;
                }
                for (int i = 0; i < (int)n_fields; i++) {
                    PyObject *fv = PyTuple_GET_ITEM(elements, i);
                    Py_INCREF(fv);
                    PyTuple_SET_ITEM(fields, i, fv);
                }
                PyObject *kwargs = Py_BuildValue("{sOsO}", "struct_type", raw_func, "fields", fields);
                Py_DECREF(fields);
                if (kwargs == NULL) {
                    Py_DECREF(raw_args);
                    Py_DECREF(raw_func);
                    goto error;
                }
                PyObject *retval = PyObject_Call((PyObject *)Menai_StructType, empty_tuple, kwargs);
                Py_DECREF(kwargs);
                if (retval == NULL) {
                    Py_DECREF(raw_args);
                    Py_DECREF(raw_func);
                    goto error;
                }
                int saved_return_dest = frame->return_dest;
                frame_release(frame);
                frame_depth--;
                Frame *caller = &frames[frame_depth];
                if (caller->is_sentinel) {
                    Py_DECREF(raw_args);
                    Py_DECREF(raw_func);
                    return retval;
                }
                reg_set(regs, caller->base + saved_return_dest, retval);
                Py_DECREF(retval);
                Py_DECREF(raw_args);
                Py_DECREF(raw_func);
                frame = caller;
            } else {
                Py_DECREF(raw_func);
                Py_DECREF(raw_args);
                menai_raise_eval_error("apply: first argument must be a function");
                goto error;
            }
            break;
        }

        case OP_EMIT_TRACE:
            /* Trace is a no-op in the C VM — no watcher support yet. */
            break;


        case OP_COMPLEX_P:
            bool_store(regs, base + dest, IS_MENAI_COMPLEX(regs[base + src0]));
            break;

        case OP_COMPLEX_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex=?")) goto error;
            if (!require_complex(b, "complex=?")) goto error;
            int eq = PyObject_RichCompareBool(a, b, Py_EQ);
            if (eq < 0) goto error;
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_COMPLEX_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex!=?")) goto error;
            if (!require_complex(b, "complex!=?")) goto error;
            int neq = PyObject_RichCompareBool(a, b, Py_NE);
            if (neq < 0) goto error;
            bool_store(regs, base + dest, neq);
            break;
        }

        case OP_COMPLEX_REAL: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-real")) goto error;
            PyObject *cv = menai_complex_value(a);
            if (cv == NULL) goto error;
            double r = PyComplex_RealAsDouble(cv);
            PyObject *_fr = make_float(r);
            if (_fr == NULL) goto error;
            reg_set(regs, base + dest, _fr);
            Py_DECREF(_fr);
            break;
        }

        case OP_COMPLEX_IMAG: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-imag")) goto error;
            PyObject *cv = menai_complex_value(a);
            if (cv == NULL) goto error;
            double i = PyComplex_ImagAsDouble(cv);
            PyObject *_fr = make_float(i);
            if (_fr == NULL) goto error;
            reg_set(regs, base + dest, _fr);
            Py_DECREF(_fr);
            break;
        }

        case OP_COMPLEX_ABS: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-abs")) goto error;
            PyObject *cv = menai_complex_value(a);
            if (cv == NULL) goto error;
            double re = PyComplex_RealAsDouble(cv), im = PyComplex_ImagAsDouble(cv);
            PyObject *_fr = make_float(sqrt(re*re + im*im));
            if (_fr == NULL) goto error;
            reg_set(regs, base + dest, _fr);
            Py_DECREF(_fr);
            break;
        }

        case OP_COMPLEX_NEG: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-neg")) goto error;
            PyObject *cv = menai_complex_value(a);
            if (cv == NULL) goto error;
            PyObject *neg = PyNumber_Negative(cv);
            PyObject *_r = make_complex_value(neg);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_ADD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex+")) goto error;
            if (!require_complex(b, "complex+")) goto error;
            PyObject *av = menai_complex_value(a);
            if (av == NULL) goto error;
            PyObject *bv = menai_complex_value(b);
            if (bv == NULL) goto error;
            PyObject *res = PyNumber_Add(av, bv);
            PyObject *_r = make_complex_value(res);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_SUB: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex-")) goto error;
            if (!require_complex(b, "complex-")) goto error;
            PyObject *av = menai_complex_value(a);
            if (av == NULL) goto error;
            PyObject *bv = menai_complex_value(b);
            if (bv == NULL) goto error;
            PyObject *res = PyNumber_Subtract(av, bv);
            PyObject *_r = make_complex_value(res);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_MUL: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex*")) goto error;
            if (!require_complex(b, "complex*")) goto error;
            PyObject *av = menai_complex_value(a);
            if (av == NULL) goto error;
            PyObject *bv = menai_complex_value(b);
            if (bv == NULL) goto error;
            PyObject *res = PyNumber_Multiply(av, bv);
            PyObject *_r = make_complex_value(res);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_DIV: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex/")) goto error;
            if (!require_complex(b, "complex/")) goto error;
            PyObject *av = menai_complex_value(a);
            if (av == NULL) goto error;
            PyObject *bv = menai_complex_value(b);
            if (bv == NULL) goto error;
            /* Check for zero divisor */
            double br = PyComplex_RealAsDouble(bv), bi = PyComplex_ImagAsDouble(bv);
            if (br == 0.0 && bi == 0.0) {
                menai_raise_eval_error("Division by zero in 'complex/'");
                goto error;
            }
            PyObject *res = PyNumber_TrueDivide(av, bv);
            PyObject *_r = make_complex_value(res);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_EXPN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex-expn")) goto error;
            if (!require_complex(b, "complex-expn")) goto error;
            PyObject *av = menai_complex_value(a);
            if (av == NULL) goto error;
            PyObject *bv = menai_complex_value(b);
            if (bv == NULL) goto error;
            PyObject *res = PyNumber_Power(av, bv, Py_None);
            PyObject *_r = make_complex_value(res);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_EXP: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-exp")) goto error;
            PyObject *cv = menai_complex_value(a);
            if (cv == NULL) goto error;
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = cexp(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_LOG: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-log")) goto error;
            PyObject *cv = menai_complex_value(a);
            if (cv == NULL) goto error;
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = clog(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_LOG10: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-log10")) goto error;
            PyObject *cv = menai_complex_value(a);
            if (cv == NULL) goto error;
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = clog(z) / log(10.0);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_SIN: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-sin")) goto error;
            PyObject *cv = menai_complex_value(a);
            if (cv == NULL) goto error;
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = csin(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_COS: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-cos")) goto error;
            PyObject *cv = menai_complex_value(a);
            if (cv == NULL) goto error;
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = ccos(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_TAN: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-tan")) goto error;
            PyObject *cv = menai_complex_value(a);
            if (cv == NULL) goto error;
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = ctan(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_SQRT: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-sqrt")) goto error;
            PyObject *cv = menai_complex_value(a);
            if (cv == NULL) goto error;
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = csqrt(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_LOGN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex-logn")) goto error;
            if (!require_complex(b, "complex-logn")) goto error;
            PyObject *av = menai_complex_value(a);
            if (av == NULL) goto error;
            PyObject *bv = menai_complex_value(b);
            if (bv == NULL) goto error;
            double complex za = PyComplex_RealAsDouble(av) + PyComplex_ImagAsDouble(av) * I;
            double complex zb = PyComplex_RealAsDouble(bv) + PyComplex_ImagAsDouble(bv) * I;
            if (zb == 0.0) {
                menai_raise_eval_error("Function 'complex-logn' requires a non-zero base");
                goto error;
            }
            double complex cr = clog(za) / clog(zb);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_COMPLEX_TO_STRING: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex->string")) goto error;
            PyObject *desc = PyObject_CallMethod(a, "describe", NULL);
            if (desc == NULL) goto error;
            PyObject *r = make_string_from_pyobj(desc);
            Py_DECREF(desc);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRING_P:
            bool_store(regs, base + dest, IS_MENAI_STRING(regs[base + src0]));
            break;

        case OP_STRING_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string=?")) goto error;
            if (!require_string(b, "string=?")) goto error;
            PyObject *_cmp = PyUnicode_RichCompare(((MenaiString_Object *)a)->value,
                                                   ((MenaiString_Object *)b)->value, Py_EQ);
            if (!_cmp) goto error;
            bool_store(regs, base + dest, PyObject_IsTrue(_cmp));
            Py_DECREF(_cmp);
            break;
        }

        case OP_STRING_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string!=?")) goto error;
            if (!require_string(b, "string!=?")) goto error;
            PyObject *_cmp = PyUnicode_RichCompare(((MenaiString_Object *)a)->value,
                                                   ((MenaiString_Object *)b)->value, Py_NE);
            if (!_cmp) goto error;
            bool_store(regs, base + dest, PyObject_IsTrue(_cmp));
            Py_DECREF(_cmp);
            break;
        }

        case OP_STRING_LT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string<?")) goto error;
            if (!require_string(b, "string<?")) goto error;
            PyObject *_cmp = PyUnicode_RichCompare(((MenaiString_Object *)a)->value,
                                                   ((MenaiString_Object *)b)->value, Py_LT);
            if (!_cmp) goto error;
            bool_store(regs, base + dest, PyObject_IsTrue(_cmp));
            Py_DECREF(_cmp);
            break;
        }

        case OP_STRING_GT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string>?")) goto error;
            if (!require_string(b, "string>?")) goto error;
            PyObject *_cmp = PyUnicode_RichCompare(((MenaiString_Object *)a)->value,
                                                   ((MenaiString_Object *)b)->value, Py_GT);
            if (!_cmp) goto error;
            bool_store(regs, base + dest, PyObject_IsTrue(_cmp));
            Py_DECREF(_cmp);
            break;
        }

        case OP_STRING_LTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string<=?")) goto error;
            if (!require_string(b, "string<=?")) goto error;
            PyObject *_cmp = PyUnicode_RichCompare(((MenaiString_Object *)a)->value,
                                                   ((MenaiString_Object *)b)->value, Py_LE);
            if (!_cmp) goto error;
            bool_store(regs, base + dest, PyObject_IsTrue(_cmp));
            Py_DECREF(_cmp);
            break;
        }

        case OP_STRING_GTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string>=?")) goto error;
            if (!require_string(b, "string>=?")) goto error;
            PyObject *_cmp = PyUnicode_RichCompare(((MenaiString_Object *)a)->value,
                                                   ((MenaiString_Object *)b)->value, Py_GE);
            if (!_cmp) goto error;
            bool_store(regs, base + dest, PyObject_IsTrue(_cmp));
            Py_DECREF(_cmp);
            break;
        }

        case OP_STRING_LENGTH: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-length")) goto error;
            PyObject *sv = menai_string_value(a);
            if (sv == NULL) goto error;
            Py_ssize_t len = PyUnicode_GET_LENGTH(sv);
            PyObject *r = PyLong_FromSsize_t(len);
            if (r == NULL) goto error;
            PyObject *_r = make_integer_value(r);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_STRING_UPCASE: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-upcase")) goto error;
            PyObject *sv = menai_string_value(a);
            if (sv == NULL) goto error;
            PyObject *up = PyObject_CallMethod(sv, "upper", NULL);
            if (up == NULL) goto error;
            PyObject *r = make_string_from_pyobj(up);
            Py_DECREF(up);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRING_DOWNCASE: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-downcase")) goto error;
            PyObject *sv = menai_string_value(a);
            if (sv == NULL) goto error;
            PyObject *lo = PyObject_CallMethod(sv, "lower", NULL);
            if (lo == NULL) goto error;
            PyObject *r = make_string_from_pyobj(lo);
            Py_DECREF(lo);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRING_TRIM: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-trim")) goto error;
            PyObject *sv = menai_string_value(a);
            if (sv == NULL) goto error;
            PyObject *t = PyObject_CallMethod(sv, "strip", NULL);
            if (t == NULL) goto error;
            PyObject *r = make_string_from_pyobj(t);
            Py_DECREF(t);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRING_TRIM_LEFT: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-trim-left")) goto error;
            PyObject *sv = menai_string_value(a);
            if (sv == NULL) goto error;
            PyObject *t = PyObject_CallMethod(sv, "lstrip", NULL);
            if (t == NULL) goto error;
            PyObject *r = make_string_from_pyobj(t);
            Py_DECREF(t);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRING_TRIM_RIGHT: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-trim-right")) goto error;
            PyObject *sv = menai_string_value(a);
            if (sv == NULL) goto error;
            PyObject *t = PyObject_CallMethod(sv, "rstrip", NULL);
            if (t == NULL) goto error;
            PyObject *r = make_string_from_pyobj(t);
            Py_DECREF(t);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRING_CONCAT: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-concat")) goto error;
            if (!require_string(b, "string-concat")) goto error;
            PyObject *sa = menai_string_value(a);
            if (sa == NULL) goto error;
            PyObject *sb = menai_string_value(b);
            if (sb == NULL) goto error;
            PyObject *cat = PyUnicode_Concat(sa, sb);
            if (cat == NULL) goto error;
            PyObject *r = make_string_from_pyobj(cat);
            Py_DECREF(cat);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRING_PREFIX_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-prefix?")) goto error;
            if (!require_string(b, "string-prefix?")) goto error;
            PyObject *sa = menai_string_value(a);
            if (sa == NULL) goto error;
            PyObject *sb = menai_string_value(b);
            if (sb == NULL) goto error;
            int r = PyUnicode_Tailmatch(sa, sb, 0, PY_SSIZE_T_MAX, -1);
            if (r < 0) goto error;
            bool_store(regs, base + dest, r);
            break;
        }

        case OP_STRING_SUFFIX_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-suffix?")) goto error;
            if (!require_string(b, "string-suffix?")) goto error;
            PyObject *sa = menai_string_value(a);
            if (sa == NULL) goto error;
            PyObject *sb = menai_string_value(b);
            if (sb == NULL) goto error;
            int r = PyUnicode_Tailmatch(sa, sb, 0, PY_SSIZE_T_MAX, 1);
            if (r < 0) goto error;
            bool_store(regs, base + dest, r);
            break;
        }

        case OP_STRING_REF: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-ref")) goto error;
            if (!IS_MENAI_INTEGER(b)) {
                menai_raise_eval_error("string-ref: index must be integer");
                goto error;
            }
            PyObject *sa = menai_string_value(a);
            if (sa == NULL) goto error;
            PyObject *iv = menai_integer_value(b);
            if (iv == NULL) goto error;
            Py_ssize_t idx = PyLong_AsSsize_t(iv);
            Py_ssize_t slen = PyUnicode_GET_LENGTH(sa);
            if (idx < 0 || idx >= slen) {
                menai_raise_eval_errorf("string-ref index out of range: %zd", idx);
                goto error;
            }
            Py_UCS4 ch = PyUnicode_ReadChar(sa, idx);
            PyObject *ch_str = PyUnicode_FromOrdinal((int)ch);
            if (ch_str == NULL) goto error;
            PyObject *r = make_string_from_pyobj(ch_str);
            Py_DECREF(ch_str);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRING_SLICE: {
            PyObject *a = regs[base + src0], *b = regs[base + src1], *c = regs[base + src2];
            if (!require_string(a, "string-slice")) goto error;
            if (!IS_MENAI_INTEGER(b) || !IS_MENAI_INTEGER(c)) {
                menai_raise_eval_error("string-slice: indices must be integers");
                goto error;
            }
            PyObject *sa = menai_string_value(a);
            if (sa == NULL) goto error;
            PyObject *bv = menai_integer_value(b);
            if (bv == NULL) goto error;
            PyObject *cv = menai_integer_value(c);
            if (cv == NULL) goto error;
            Py_ssize_t start = PyLong_AsSsize_t(bv), end = PyLong_AsSsize_t(cv);
            Py_ssize_t slen = PyUnicode_GET_LENGTH(sa);
            if (start < 0) {
                menai_raise_eval_errorf("string-slice start index cannot be negative: %zd", start);
                goto error;
            }
            if (end < 0) {
                menai_raise_eval_errorf("string-slice end index cannot be negative: %zd", end);
                goto error;
            }
            if (start > slen) {
                menai_raise_eval_errorf("string-slice start index out of range: %zd (string length: %zd)", start, slen);
                goto error;
            }
            if (end > slen) {
                menai_raise_eval_errorf("string-slice end index out of range: %zd (string length: %zd)", end, slen);
                goto error;
            }
            if (start > end) {
                menai_raise_eval_errorf("string-slice start index (%zd) cannot be greater than end index (%zd)", start, end);
                goto error;
            }
            PyObject *sliced = PyUnicode_Substring(sa, start, end);
            if (sliced == NULL) goto error;
            PyObject *r = make_string_from_pyobj(sliced);
            Py_DECREF(sliced);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRING_REPLACE: {
            PyObject *a = regs[base + src0], *b = regs[base + src1], *c = regs[base + src2];
            if (!require_string(a, "string-replace")) goto error;
            if (!require_string(b, "string-replace")) goto error;
            if (!require_string(c, "string-replace")) goto error;
            PyObject *sa = menai_string_value(a);
            if (sa == NULL) goto error;
            PyObject *sb = menai_string_value(b);
            if (sb == NULL) goto error;
            PyObject *sc = menai_string_value(c);
            if (sc == NULL) goto error;
            PyObject *replaced = PyObject_CallMethod(sa, "replace", "OO", sb, sc);
            if (replaced == NULL) goto error;
            PyObject *r = make_string_from_pyobj(replaced);
            Py_DECREF(replaced);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRING_INDEX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-index")) goto error;
            if (!require_string(b, "string-index")) goto error;
            PyObject *sa = menai_string_value(a);
            if (sa == NULL) goto error;
            PyObject *sb = menai_string_value(b);
            if (sb == NULL) goto error;
            Py_ssize_t idx = PyUnicode_Find(sa, sb, 0, PY_SSIZE_T_MAX, 1);
            if (idx == -2) goto error; /* error */
            if (idx == -1) {
                reg_set(regs, base + dest, Menai_NONE);
            } else {
                PyObject *iv = PyLong_FromSsize_t(idx);
                if (iv == NULL) goto error;
                PyObject *_r = make_integer_value(iv);
                if (_r == NULL) goto error;
                reg_set(regs, base + dest, _r);
                Py_DECREF(_r);
            }
            break;
        }

        case OP_STRING_TO_INTEGER_CODEPOINT: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string->integer-codepoint")) goto error;
            PyObject *sa = menai_string_value(a);
            if (sa == NULL) goto error;
            Py_ssize_t slen = PyUnicode_GET_LENGTH(sa);
            if (slen != 1) {
                menai_raise_eval_error("string->integer-codepoint: requires single-character string");
                goto error;
            }
            Py_UCS4 ch = PyUnicode_ReadChar(sa, 0);
            PyObject *iv = PyLong_FromLong((long)ch);
            if (iv == NULL) goto error;
            PyObject *_r = make_integer_value(iv);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_STRING_TO_INTEGER: {
            /* src0=string, src1=radix(integer) */
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string->integer")) goto error;
            if (!IS_MENAI_INTEGER(b)) {
                menai_raise_eval_error("string->integer: radix must be integer");
                goto error;
            }
            PyObject *bv = menai_integer_value(b);
            if (bv == NULL) goto error;
            long radix = PyLong_AsLong(bv);
            if (radix == -1 && PyErr_Occurred()) goto error;
            if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
                menai_raise_eval_errorf("string->integer radix must be 2, 8, 10, or 16, got %ld", radix);
                goto error;
            }
            PyObject *sa = menai_string_value(a);
            if (sa == NULL) goto error;
            PyObject *stripped = PyObject_CallMethod(sa, "strip", NULL);
            if (stripped == NULL) goto error;
            PyObject *ri = PyLong_FromUnicodeObject(stripped, (int)radix);
            Py_DECREF(stripped);
            if (ri == NULL) {
                PyErr_Clear();
                reg_set(regs, base + dest, Menai_NONE);
            } else {
                PyObject *_r = make_integer_value(ri);
                if (_r == NULL) goto error;
                reg_set(regs, base + dest, _r);
                Py_DECREF(_r);
            }
            break;
        }

        case OP_STRING_TO_NUMBER: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string->number")) goto error;
            /* Delegate to the Menai object's method via Python call */
            PyObject *sa = menai_string_value(a);
            if (sa == NULL) goto error;
            /* Try int, then float, then complex — matching Python VM logic */
            PyObject *result = NULL;
            /* Check for 'j'/'J' → complex */
            PyObject *lower = PyObject_CallMethod(sa, "lower", NULL);
            if (lower == NULL) goto error;
            /* Use PyUnicode_Contains to avoid leaking temporary string objects */
            PyObject *_j = PyUnicode_FromString("j");
            PyObject *_dot = PyUnicode_FromString(".");
            PyObject *_e = PyUnicode_FromString("e");
            if (!_j || !_dot || !_e) {
                Py_XDECREF(_j);
                Py_XDECREF(_dot);
                Py_XDECREF(_e);
                Py_DECREF(lower);
                goto error;
            }
            int has_j = PyUnicode_Find(lower, _j, 0, PY_SSIZE_T_MAX, 1) >= 0;
            int has_dot = PyUnicode_Find(sa, _dot, 0, PY_SSIZE_T_MAX, 1) >= 0;
            int has_e = PyUnicode_Find(lower, _e, 0, PY_SSIZE_T_MAX, 1) >= 0;
            Py_DECREF(_j);
            Py_DECREF(_dot);
            Py_DECREF(_e);
            Py_DECREF(lower);
            if (!has_dot && !has_e && !has_j) {
                result = PyLong_FromUnicodeObject(sa, 10);
                if (result) {
                    PyObject *r = make_integer(result);
                    Py_DECREF(result);
                    if (r == NULL) goto error;
                    reg_set(regs, base + dest, r);
                    Py_DECREF(r);
                    break;
                }
                PyErr_Clear();
            }
            if (has_j) {
                result = PyObject_CallOneArg((PyObject *)&PyComplex_Type, sa);
                if (result != NULL) {
                    PyObject *r = make_complex_from_doubles(PyComplex_RealAsDouble(result),
                                                            PyComplex_ImagAsDouble(result));
                    Py_DECREF(result);
                    if (r == NULL) goto error;
                    reg_set(regs, base + dest, r);
                    Py_DECREF(r);
                    break;
                }
                PyErr_Clear();
            }
            /* Try float */
            result = PyFloat_FromString(sa);
            if (result) {
                double dv = PyFloat_AsDouble(result);
                Py_DECREF(result);
                PyObject *_r = make_float(dv);
                if (_r == NULL) goto error;
                reg_set(regs, base + dest, _r);
                Py_DECREF(_r);
            } else {
                PyErr_Clear();
                reg_set(regs, base + dest, Menai_NONE);
            }
            break;
        }

        case OP_STRING_TO_LIST: {
            /* src0=string, src1=delimiter string */
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string->list")) goto error;
            if (!require_string(b, "string->list")) goto error;
            PyObject *sa = menai_string_value(a);
            if (sa == NULL) goto error;
            PyObject *sb = menai_string_value(b);
            if (sb == NULL) goto error;
            PyObject *parts;
            if (PyUnicode_GET_LENGTH(sb) == 0) {
                /* Split into individual characters */
                Py_ssize_t slen = PyUnicode_GET_LENGTH(sa);
                parts = PyList_New(slen);
                if (parts == NULL) {
                    goto error;
                }
                for (Py_ssize_t i = 0; i < slen; i++) {
                    PyObject *ch = PyUnicode_FromOrdinal(PyUnicode_ReadChar(sa, i));
                    if (ch == NULL) {
                        Py_DECREF(parts);
                        goto error;
                    }
                    PyObject *ms = make_string_from_pyobj(ch);
                    Py_DECREF(ch);
                    if (ms == NULL) {
                        Py_DECREF(parts);
                        goto error;
                    }
                    PyList_SET_ITEM(parts, i, ms);
                }
            } else {
                parts = PyObject_CallMethod(sa, "split", "O", sb);
                if (parts == NULL) {
                    goto error;
                }
                /* Wrap each str in MenaiString */
                Py_ssize_t n = PyList_GET_SIZE(parts);
                for (Py_ssize_t i = 0; i < n; i++) {
                    PyObject *ms = make_string_from_pyobj(PyList_GET_ITEM(parts, i));
                    if (ms == NULL) {
                        Py_DECREF(parts);
                        goto error;
                    }
                    PyObject *old = PyList_GET_ITEM(parts, i);
                    PyList_SET_ITEM(parts, i, ms);
                    Py_DECREF(old);
                }
            }
            PyObject *tup = PyList_AsTuple(parts);
            Py_DECREF(parts);
            if (tup == NULL) goto error;
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, tup);
            Py_DECREF(tup);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_LIST_P:
            bool_store(regs, base + dest, IS_MENAI_LIST(regs[base + src0]));
            break;

        case OP_LIST_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_list(a, "list=?")) goto error;
            if (!require_list(b, "list=?")) goto error;
            int eq = PyObject_RichCompareBool(a, b, Py_EQ);
            if (eq < 0) goto error;
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_LIST_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_list(a, "list!=?")) goto error;
            if (!require_list(b, "list!=?")) goto error;
            int neq = PyObject_RichCompareBool(a, b, Py_NE);
            if (neq < 0) goto error;
            bool_store(regs, base + dest, neq);
            break;
        }

        case OP_LIST_NULL_P: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-null?")) goto error;
            PyObject *elems = menai_list_elements(a);
            int is_null = (PyTuple_GET_SIZE(elems) == 0);
            bool_store(regs, base + dest, is_null);
            break;
        }

        case OP_LIST_LENGTH: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-length")) goto error;
            PyObject *elems = menai_list_elements(a);
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            PyObject *iv = PyLong_FromSsize_t(n);
            if (iv == NULL) goto error;
            PyObject *_r = make_integer_value(iv);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_LIST_FIRST: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-first")) goto error;
            PyObject *elems = menai_list_elements(a);
            if (PyTuple_GET_SIZE(elems) == 0) {
                menai_raise_eval_error("Function 'list-first' requires a non-empty list");
                goto error;
            }
            PyObject *first = PyTuple_GET_ITEM(elems, 0);
            reg_set(regs, base + dest, first);
            break;
        }

        case OP_LIST_REST: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-rest")) goto error;
            PyObject *elems = menai_list_elements(a);
            if (PyTuple_GET_SIZE(elems) == 0) {
                menai_raise_eval_error("Function 'list-rest' requires a non-empty list");
                goto error;
            }
            PyObject *rest = PyTuple_GetSlice(elems, 1, PY_SSIZE_T_MAX);
            if (rest == NULL) goto error;
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, rest);
            Py_DECREF(rest);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_LIST_LAST: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-last")) goto error;
            PyObject *elems = menai_list_elements(a);
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            if (n == 0) {
                menai_raise_eval_error("Function 'list-last' requires a non-empty list");
                goto error;
            }
            PyObject *last = PyTuple_GET_ITEM(elems, n - 1);
            reg_set(regs, base + dest, last);
            break;
        }

        case OP_LIST_REF: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_list(a, "list-ref")) goto error;
            if (!IS_MENAI_INTEGER(b)) {
                menai_raise_eval_error("list-ref: index must be integer");
                goto error;
            }
            PyObject *elems = menai_list_elements(a);
            PyObject *bv = menai_integer_value(b);
            if (bv == NULL) goto error;
            Py_ssize_t idx = PyLong_AsSsize_t(bv);
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            if (idx < 0 || idx >= n) {
                menai_raise_eval_errorf("list-ref: index out of range: %zd", idx);
                goto error;
            }
            reg_set(regs, base + dest, PyTuple_GET_ITEM(elems, idx));
            break;
        }

        case OP_LIST_PREPEND: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_list(a, "list-prepend")) goto error;
            PyObject *elems = menai_list_elements(a);
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            PyObject *new_tup = PyTuple_New(n + 1);
            if (new_tup == NULL) goto error;
            Py_INCREF(item);
            PyTuple_SET_ITEM(new_tup, 0, item);
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *e = PyTuple_GET_ITEM(elems, i);
                Py_INCREF(e);
                PyTuple_SET_ITEM(new_tup, i + 1, e);
            }
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, new_tup);
            Py_DECREF(new_tup);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_LIST_APPEND: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_list(a, "list-append")) goto error;
            PyObject *elems = menai_list_elements(a);
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            PyObject *new_tup = PyTuple_New(n + 1);
            if (new_tup == NULL) goto error;
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *e = PyTuple_GET_ITEM(elems, i);
                Py_INCREF(e);
                PyTuple_SET_ITEM(new_tup, i, e);
            }
            Py_INCREF(item);
            PyTuple_SET_ITEM(new_tup, n, item);
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, new_tup);
            Py_DECREF(new_tup);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_LIST_REVERSE: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-reverse")) goto error;
            PyObject *elems = menai_list_elements(a);
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            PyObject *rev = PyTuple_New(n);
            if (rev == NULL) goto error;
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *e = PyTuple_GET_ITEM(elems, n - 1 - i);
                Py_INCREF(e);
                PyTuple_SET_ITEM(rev, i, e);
            }
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, rev);
            Py_DECREF(rev);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_LIST_CONCAT: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_list(a, "list-concat")) goto error;
            if (!require_list(b, "list-concat")) goto error;
            PyObject *ea = menai_list_elements(a);
            PyObject *eb = menai_list_elements(b);
            PyObject *cat = PySequence_Concat(ea, eb);
            if (cat == NULL) goto error;
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, cat);
            Py_DECREF(cat);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_LIST_MEMBER_P: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_list(a, "list-member?")) goto error;
            PyObject *elems = menai_list_elements(a);
            int found = PySequence_Contains(elems, item);
            if (found < 0) goto error;
            bool_store(regs, base + dest, found);
            break;
        }

        case OP_LIST_INDEX: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_list(a, "list-index")) goto error;
            PyObject *elems = menai_list_elements(a);
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            Py_ssize_t found = -1;
            for (Py_ssize_t i = 0; i < n; i++) {
                int eq = PyObject_RichCompareBool(PyTuple_GET_ITEM(elems, i), item, Py_EQ);
                if (eq < 0) goto error;
                if (eq) {
                    found = i;
                    break;
                }
            }
            if (found == -1) {
                reg_set(regs, base + dest, Menai_NONE);
            } else {
                PyObject *iv = PyLong_FromSsize_t(found);
                if (iv == NULL) goto error;
                PyObject *_r = make_integer_value(iv);
                if (_r == NULL) goto error;
                reg_set(regs, base + dest, _r);
                Py_DECREF(_r);
            }
            break;
        }

        case OP_LIST_SLICE: {
            PyObject *a = regs[base + src0], *b = regs[base + src1], *c = regs[base + src2];
            if (!require_list(a, "list-slice")) goto error;
            if (!IS_MENAI_INTEGER(b) || !IS_MENAI_INTEGER(c)) {
                menai_raise_eval_error("list-slice: indices must be integers");
                goto error;
            }
            PyObject *elems = menai_list_elements(a);
            PyObject *bv = menai_integer_value(b);
            if (bv == NULL) goto error;
            PyObject *cv = menai_integer_value(c);
            if (cv == NULL) goto error;
            Py_ssize_t start = PyLong_AsSsize_t(bv), end = PyLong_AsSsize_t(cv);
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            if (start < 0) {
                menai_raise_eval_errorf("list-slice start index cannot be negative: %zd", start);
                goto error;
            }
            if (end < 0) {
                menai_raise_eval_errorf("list-slice end index cannot be negative: %zd", end);
                goto error;
            }
            if (start > n) {
                menai_raise_eval_errorf("list-slice start index out of range: %zd (list length: %zd)", start, n);
                goto error;
            }
            if (end > n) {
                menai_raise_eval_errorf("list-slice end index out of range: %zd (list length: %zd)", end, n);
                goto error;
            }
            if (start > end) {
                menai_raise_eval_errorf("list-slice start index (%zd) cannot be greater than end index (%zd)", start, end);
                goto error;
            }
            PyObject *sliced = PyTuple_GetSlice(elems, start, end);
            if (sliced == NULL) goto error;
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, sliced);
            Py_DECREF(sliced);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_LIST_REMOVE: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_list(a, "list-remove")) goto error;
            PyObject *elems = menai_list_elements(a);
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            /* Count non-matching elements first */
            Py_ssize_t keep = 0;
            for (Py_ssize_t i = 0; i < n; i++) {
                int eq = PyObject_RichCompareBool(PyTuple_GET_ITEM(elems, i), item, Py_EQ);
                if (eq < 0) goto error;
                if (!eq) keep++;
            }
            PyObject *new_tup = PyTuple_New(keep);
            if (new_tup == NULL) goto error;
            Py_ssize_t j = 0;
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *e = PyTuple_GET_ITEM(elems, i);
                int eq = PyObject_RichCompareBool(e, item, Py_EQ);
                if (eq < 0) {
                    Py_DECREF(new_tup);
                    goto error;
                }
                if (!eq) {
                    Py_INCREF(e);
                    PyTuple_SET_ITEM(new_tup, j++, e);
                }
            }
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, new_tup);
            Py_DECREF(new_tup);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_LIST_TO_STRING: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_list(a, "list->string")) goto error;
            if (!require_string(b, "list->string")) goto error;
            PyObject *elems = menai_list_elements(a);
            PyObject *sep = menai_string_value(b);
            if (sep == NULL) goto error;
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            PyObject *parts = PyList_New(n);
            if (parts == NULL) {
                goto error;
            }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *elem = PyTuple_GET_ITEM(elems, i);
                if (!IS_MENAI_STRING(elem)) {
                    Py_DECREF(parts);
                    menai_raise_eval_error("list->string: all elements must be strings");
                    goto error;
                }
                PyObject *sv = menai_string_value(elem);
                if (sv == NULL) {
                    Py_DECREF(parts);
                    goto error;
                }
                Py_INCREF(sv);
                PyList_SET_ITEM(parts, i, sv); /* sv is borrowed; INCREF gives parts its owned ref */
            }
            PyObject *joined = PyUnicode_Join(sep, parts);
            Py_DECREF(parts);
            if (joined == NULL) goto error;
            PyObject *r = make_string_from_pyobj(joined);
            Py_DECREF(joined);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_LIST_TO_SET: {
            PyObject *a = regs[base + src0];
            if (!require_list_singular(a, "list->set")) goto error;
            PyObject *elems = menai_list_elements(a);
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_SetType, elems);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_DICT_P:
            bool_store(regs, base + dest, IS_MENAI_DICT(regs[base + src0]));
            break;

        case OP_DICT_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_dict(a, "dict=?")) goto error;
            if (!require_dict(b, "dict=?")) goto error;
            int eq = PyObject_RichCompareBool(a, b, Py_EQ);
            if (eq < 0) goto error;
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_DICT_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_dict(a, "dict!=?")) goto error;
            if (!require_dict(b, "dict!=?")) goto error;
            int neq = PyObject_RichCompareBool(a, b, Py_NE);
            if (neq < 0) goto error;
            bool_store(regs, base + dest, neq);
            break;
        }

        case OP_DICT_LENGTH: {
            PyObject *a = regs[base + src0];
            if (!require_dict(a, "dict-length")) goto error;
            PyObject *pairs = PyObject_GetAttrString(a, "pairs");
            if (pairs == NULL) goto error;
            Py_ssize_t n = PyTuple_GET_SIZE(pairs);
            Py_DECREF(pairs);
            PyObject *iv = PyLong_FromSsize_t(n);
            if (iv == NULL) goto error;
            PyObject *_r = make_integer_value(iv);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_DICT_KEYS: {
            PyObject *a = regs[base + src0];
            if (!require_dict(a, "dict-keys")) goto error;
            PyObject *r = PyObject_CallMethod(a, "dict_keys_as_list", NULL);
            /* dict-keys is a Python-level operation; delegate entirely */
            if (r == NULL) {
                PyErr_Clear();
                /* Build manually from pairs */
                PyObject *pairs = PyObject_GetAttrString(a, "pairs");
                if (pairs == NULL) goto error;
                Py_ssize_t n = PyTuple_GET_SIZE(pairs);
                PyObject *tup = PyTuple_New(n);
                if (tup == NULL) {
                    Py_DECREF(pairs);
                    goto error;
                }
                for (Py_ssize_t i = 0; i < n; i++) {
                    PyObject *pair = PyTuple_GET_ITEM(pairs, i);
                    PyObject *k = PyTuple_GET_ITEM(pair, 0);
                    Py_INCREF(k);
                    PyTuple_SET_ITEM(tup, i, k);
                }
                Py_DECREF(pairs);
                r = PyObject_CallOneArg((PyObject *)Menai_ListType, tup);
                Py_DECREF(tup);
            }
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_DICT_VALUES: {
            PyObject *a = regs[base + src0];
            if (!require_dict(a, "dict-values")) goto error;
            PyObject *pairs = PyObject_GetAttrString(a, "pairs");
            if (pairs == NULL) goto error;
            Py_ssize_t n = PyTuple_GET_SIZE(pairs);
            PyObject *tup = PyTuple_New(n);
            if (tup == NULL) {
                Py_DECREF(pairs);
                goto error;
            }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *pair = PyTuple_GET_ITEM(pairs, i);
                PyObject *v = PyTuple_GET_ITEM(pair, 1);
                Py_INCREF(v);
                PyTuple_SET_ITEM(tup, i, v);
            }
            Py_DECREF(pairs);
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, tup);
            Py_DECREF(tup);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_DICT_HAS_P: {
            PyObject *a = regs[base + src0], *key = regs[base + src1];
            if (!require_dict(a, "dict-has?")) goto error;
            PyObject *r = PyObject_CallMethod(a, "to_hashable_key", "O", key);
            if (r == NULL) goto error;
            PyObject *lookup = PyObject_GetAttrString(a, "lookup");
            if (lookup == NULL) {
                Py_DECREF(r);
                goto error;
            }
            int has = PyDict_Contains(lookup, r);
            Py_DECREF(r);
            Py_DECREF(lookup);
            if (has < 0) goto error;
            bool_store(regs, base + dest, has);
            break;
        }

        case OP_DICT_GET: {
            /* src0=dict, src1=key, src2=default */
            PyObject *a = regs[base + src0], *key = regs[base + src1], *def = regs[base + src2];
            if (!require_dict(a, "dict-get")) goto error;
            PyObject *hk = PyObject_CallMethod(a, "to_hashable_key", "O", key);
            if (hk == NULL) goto error;
            PyObject *lookup = PyObject_GetAttrString(a, "lookup");
            if (lookup == NULL) {
                Py_DECREF(hk);
                goto error;
            }
            PyObject *entry = PyDict_GetItem(lookup, hk);
            Py_DECREF(hk);
            Py_DECREF(lookup);
            if (entry != NULL) {
                /* entry is (key, value) tuple */
                PyObject *val = PyTuple_GET_ITEM(entry, 1);
                reg_set(regs, base + dest, val);
            } else {
                reg_set(regs, base + dest, def);
            }
            break;
        }

        case OP_DICT_SET: {
            /* src0=dict, src1=key, src2=value */
            PyObject *a = regs[base + src0], *key = regs[base + src1], *val = regs[base + src2];
            if (!require_dict(a, "dict-set")) goto error;
            /* Build new pairs tuple with key inserted or updated */
            PyObject *result = PyObject_CallMethod(
                a, "dict_set_impl", "OO", key, val);
            if (result == NULL) {
                PyErr_Clear();
                /* Build new pairs tuple manually */
                PyObject *pairs = PyObject_GetAttrString(a, "pairs");
                if (pairs == NULL) goto error;
                PyObject *hk = PyObject_CallMethod(a, "to_hashable_key", "O", key);
                if (hk == NULL) {
                    Py_DECREF(pairs);
                    goto error;
                }
                Py_ssize_t n = PyTuple_GET_SIZE(pairs);
                /* Find if key exists */
                int found = 0;
                PyObject *new_pairs = PyList_New(0);
                if (new_pairs == NULL) {
                    Py_DECREF(hk);
                    Py_DECREF(pairs);
                    goto error;
                }
                for (Py_ssize_t i = 0; i < n; i++) {
                    PyObject *pair = PyTuple_GET_ITEM(pairs, i);
                    PyObject *k = PyTuple_GET_ITEM(pair, 0);
                    PyObject *khk = PyObject_CallMethod(a, "to_hashable_key", "O", k);
                    if (khk == NULL) {
                        Py_DECREF(new_pairs);
                        Py_DECREF(hk);
                        Py_DECREF(pairs);
                        goto error;
                    }
                    int eq = PyObject_RichCompareBool(khk, hk, Py_EQ);
                    Py_DECREF(khk);
                    if (eq < 0) {
                        Py_DECREF(new_pairs);
                        Py_DECREF(hk);
                        Py_DECREF(pairs);
                        goto error;
                    }
                    PyObject *new_pair = eq ? PyTuple_Pack(2, key, val) : pair;
                    if (eq) found = 1;
                    if (new_pair == NULL) {
                        Py_DECREF(new_pairs);
                        Py_DECREF(hk);
                        Py_DECREF(pairs);
                        goto error;
                    }
                    if (!eq) Py_INCREF(new_pair);
                    if (PyList_Append(new_pairs, new_pair) < 0) {
                        Py_DECREF(new_pair);
                        Py_DECREF(new_pairs);
                        Py_DECREF(hk);
                        Py_DECREF(pairs);
                        goto error;
                    }
                    Py_DECREF(new_pair);
                }
                if (!found) {
                    PyObject *new_pair = PyTuple_Pack(2, key, val);
                    if (new_pair == NULL || PyList_Append(new_pairs, new_pair) < 0) {
                        Py_XDECREF(new_pair);
                        Py_DECREF(new_pairs);
                        Py_DECREF(hk);
                        Py_DECREF(pairs);
                        goto error;
                    }
                    Py_DECREF(new_pair);
                }
                Py_DECREF(hk);
                Py_DECREF(pairs);
                PyObject *new_tup = PyList_AsTuple(new_pairs);
                Py_DECREF(new_pairs);
                if (new_tup == NULL) goto error;
                result = PyObject_CallOneArg((PyObject *)Menai_DictType, new_tup);
                Py_DECREF(new_tup);
            }
            if (result == NULL) goto error;
            reg_set(regs, base + dest, result);
            Py_DECREF(result);
            break;
        }

        case OP_DICT_REMOVE: {
            PyObject *a = regs[base + src0], *key = regs[base + src1];
            if (!require_dict(a, "dict-remove")) goto error;
            PyObject *hk = PyObject_CallMethod(a, "to_hashable_key", "O", key);
            if (hk == NULL) goto error;
            PyObject *pairs = PyObject_GetAttrString(a, "pairs");
            if (pairs == NULL) {
                Py_DECREF(hk);
                goto error;
            }
            Py_ssize_t n = PyTuple_GET_SIZE(pairs);
            PyObject *new_list = PyList_New(0);
            if (new_list == NULL) {
                Py_DECREF(hk);
                Py_DECREF(pairs);
                goto error;
            }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *pair = PyTuple_GET_ITEM(pairs, i);
                PyObject *k = PyTuple_GET_ITEM(pair, 0);
                PyObject *khk = PyObject_CallMethod(a, "to_hashable_key", "O", k);
                if (khk == NULL) {
                    Py_DECREF(new_list);
                    Py_DECREF(hk);
                    Py_DECREF(pairs);
                    goto error;
                }
                int eq = PyObject_RichCompareBool(khk, hk, Py_EQ);
                Py_DECREF(khk);
                if (eq < 0) {
                    Py_DECREF(new_list);
                    Py_DECREF(hk);
                    Py_DECREF(pairs);
                    goto error;
                }
                if (!eq) {
                    Py_INCREF(pair);
                    if (PyList_Append(new_list, pair) < 0) {
                        Py_DECREF(pair);
                        Py_DECREF(new_list);
                        Py_DECREF(hk);
                        Py_DECREF(pairs);
                        goto error;
                    }
                    Py_DECREF(pair);
                }
            }
            Py_DECREF(hk);
            Py_DECREF(pairs);
            PyObject *new_tup = PyList_AsTuple(new_list);
            Py_DECREF(new_list);
            if (new_tup == NULL) goto error;
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_DictType, new_tup);
            Py_DECREF(new_tup);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_DICT_MERGE: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_dict(a, "dict-merge")) goto error;
            if (!require_dict(b, "dict-merge")) goto error;
            /* Delegate to the Python __add__ equivalent: call the Cython method */
            PyObject *r = PyObject_CallMethod(a, "dict_merge_impl", "O", b);
            if (r == NULL) {
                PyErr_Clear();
                /* Fallback: build merged dict manually */
                PyObject *pa = PyObject_GetAttrString(a, "pairs");
                if (pa == NULL) goto error;
                PyObject *pb = PyObject_GetAttrString(b, "pairs");
                if (pb == NULL) {
                    Py_DECREF(pa);
                    goto error;
                }
                /* a's pairs first, then b's new pairs */
                PyObject *merged = PyList_New(0);
                if (merged == NULL) {
                    Py_DECREF(pa);
                    Py_DECREF(pb);
                    goto error;
                }
                PyObject *seen = PyDict_New();
                if (seen == NULL) {
                    Py_DECREF(merged);
                    Py_DECREF(pa);
                    Py_DECREF(pb);
                    goto error;
                }
                /* Add a's pairs (with b's values if key in b) */
                PyObject *b_lookup = PyObject_GetAttrString(b, "lookup");
                if (b_lookup == NULL) {
                    Py_DECREF(seen);
                    Py_DECREF(merged);
                    Py_DECREF(pa);
                    Py_DECREF(pb);
                    goto error;
                }
                Py_ssize_t na = PyTuple_GET_SIZE(pa);
                for (Py_ssize_t i = 0; i < na; i++) {
                    PyObject *pair = PyTuple_GET_ITEM(pa, i);
                    PyObject *k = PyTuple_GET_ITEM(pair, 0);
                    PyObject *hk = PyObject_CallMethod(a, "to_hashable_key", "O", k);
                    if (hk == NULL) {
                        Py_DECREF(b_lookup);
                        Py_DECREF(seen);
                        Py_DECREF(merged);
                        Py_DECREF(pa);
                        Py_DECREF(pb);
                        goto error;
                    }
                    PyObject *b_entry = PyDict_GetItem(b_lookup, hk);
                    PyObject *use_pair = b_entry ? b_entry : pair;
                    Py_INCREF(use_pair);
                    if (PyList_Append(merged, use_pair) < 0 || PyDict_SetItem(seen, hk, Py_True) < 0) {
                        Py_DECREF(use_pair);
                        Py_DECREF(hk);
                        Py_DECREF(b_lookup);
                        Py_DECREF(seen);
                        Py_DECREF(merged);
                        Py_DECREF(pa);
                        Py_DECREF(pb);
                        goto error;
                    }
                    Py_DECREF(use_pair);
                    Py_DECREF(hk);
                }
                /* Add b's new pairs */
                Py_ssize_t nb = PyTuple_GET_SIZE(pb);
                for (Py_ssize_t i = 0; i < nb; i++) {
                    PyObject *pair = PyTuple_GET_ITEM(pb, i);
                    PyObject *k = PyTuple_GET_ITEM(pair, 0);
                    PyObject *hk = PyObject_CallMethod(b, "to_hashable_key", "O", k);
                    if (hk == NULL) {
                        Py_DECREF(b_lookup);
                        Py_DECREF(seen);
                        Py_DECREF(merged);
                        Py_DECREF(pa);
                        Py_DECREF(pb);
                        goto error;
                    }
                    if (!PyDict_Contains(seen, hk)) {
                        Py_INCREF(pair);
                        if (PyList_Append(merged, pair) < 0) {
                            Py_DECREF(pair);
                            Py_DECREF(hk);
                            Py_DECREF(b_lookup);
                            Py_DECREF(seen);
                            Py_DECREF(merged);
                            Py_DECREF(pa);
                            Py_DECREF(pb);
                            goto error;
                        }
                        Py_DECREF(pair);
                    }
                    Py_DECREF(hk);
                }
                Py_DECREF(b_lookup);
                Py_DECREF(seen);
                Py_DECREF(pa);
                Py_DECREF(pb);
                PyObject *new_tup = PyList_AsTuple(merged);
                Py_DECREF(merged);
                if (new_tup == NULL) goto error;
                r = PyObject_CallOneArg((PyObject *)Menai_DictType, new_tup);
                Py_DECREF(new_tup);
            }
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_SET_P:
            bool_store(regs, base + dest, IS_MENAI_SET(regs[base + src0]));
            break;

        case OP_SET_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set=?")) goto error;
            if (!require_set(b, "set=?")) goto error;
            int eq = PyObject_RichCompareBool(a, b, Py_EQ);
            if (eq < 0) goto error;
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_SET_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set!=?")) goto error;
            if (!require_set(b, "set!=?")) goto error;
            int neq = PyObject_RichCompareBool(a, b, Py_NE);
            if (neq < 0) goto error;
            bool_store(regs, base + dest, neq);
            break;
        }

        case OP_SET_LENGTH: {
            PyObject *a = regs[base + src0];
            if (!require_set_singular(a, "set-length")) goto error;
            PyObject *elems = PyObject_GetAttrString(a, "elements");
            if (elems == NULL) goto error;
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            Py_DECREF(elems);
            PyObject *iv = PyLong_FromSsize_t(n);
            if (iv == NULL) goto error;
            PyObject *_r = make_integer_value(iv);
            if (_r == NULL) goto error;
            reg_set(regs, base + dest, _r);
            Py_DECREF(_r);
            break;
        }

        case OP_SET_MEMBER_P: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_set_singular(a, "set-member?")) goto error;
            PyObject *hk = PyObject_CallMethod((PyObject *)Menai_DictType, "to_hashable_key", "O", item);
            if (hk == NULL) goto error;
            PyObject *members = PyObject_GetAttrString(a, "members");
            if (members == NULL) {
                Py_DECREF(hk);
                goto error;
            }
            int has = PySequence_Contains(members, hk);
            Py_DECREF(hk);
            Py_DECREF(members);
            if (has < 0) goto error;
            bool_store(regs, base + dest, has);
            break;
        }

        case OP_SET_ADD: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_set_singular(a, "set-add")) goto error;
            PyObject *hk = PyObject_CallMethod((PyObject *)Menai_DictType, "to_hashable_key", "O", item);
            if (hk == NULL) goto error;
            PyObject *members = PyObject_GetAttrString(a, "members");
            if (members == NULL) {
                Py_DECREF(hk);
                goto error;
            }
            int has = PySequence_Contains(members, hk);
            Py_DECREF(hk);
            Py_DECREF(members);
            if (has < 0) goto error;
            if (has) {
                reg_set(regs, base + dest, a);
            } else {
                PyObject *elems = PyObject_GetAttrString(a, "elements");
                if (elems == NULL) goto error;
                Py_ssize_t n = PyTuple_GET_SIZE(elems);
                PyObject *new_tup = PyTuple_New(n + 1);
                if (new_tup == NULL) {
                    Py_DECREF(elems);
                    goto error;
                }
                for (Py_ssize_t i = 0; i < n; i++) {
                    PyObject *e = PyTuple_GET_ITEM(elems, i);
                    Py_INCREF(e);
                    PyTuple_SET_ITEM(new_tup, i, e);
                }
                Py_DECREF(elems);
                Py_INCREF(item);
                PyTuple_SET_ITEM(new_tup, n, item);
                PyObject *r = PyObject_CallOneArg((PyObject *)Menai_SetType, new_tup);
                Py_DECREF(new_tup);
                if (r == NULL) goto error;
                reg_set(regs, base + dest, r);
                Py_DECREF(r);
            }
            break;
        }

        case OP_SET_REMOVE: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_set_singular(a, "set-remove")) goto error;
            PyObject *hk = PyObject_CallMethod((PyObject *)Menai_DictType, "to_hashable_key", "O", item);
            if (hk == NULL) goto error;
            PyObject *elems = PyObject_GetAttrString(a, "elements");
            if (elems == NULL) {
                Py_DECREF(hk);
                goto error;
            }
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            PyObject *new_list = PyList_New(0);
            if (new_list == NULL) {
                Py_DECREF(elems);
                Py_DECREF(hk);
                goto error;
            }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *e = PyTuple_GET_ITEM(elems, i);
                PyObject *ehk = PyObject_CallMethod(
                    (PyObject *)Menai_DictType, "to_hashable_key", "O", e);
                if (ehk == NULL) {
                    Py_DECREF(new_list);
                    Py_DECREF(elems);
                    Py_DECREF(hk);
                    goto error;
                }
                int eq = PyObject_RichCompareBool(ehk, hk, Py_EQ);
                Py_DECREF(ehk);
                if (eq < 0) {
                    Py_DECREF(new_list);
                    Py_DECREF(elems);
                    Py_DECREF(hk);
                    goto error;
                }
                if (!eq) {
                    Py_INCREF(e);
                    if (PyList_Append(new_list, e) < 0) {
                        Py_DECREF(e);
                        Py_DECREF(new_list);
                        Py_DECREF(elems);
                        Py_DECREF(hk);
                        goto error;
                    }
                    Py_DECREF(e);
                }
            }
            Py_DECREF(elems);
            Py_DECREF(hk);
            PyObject *new_tup = PyList_AsTuple(new_list);
            Py_DECREF(new_list);
            if (new_tup == NULL) goto error;
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_SetType, new_tup);
            Py_DECREF(new_tup);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_SET_UNION: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-union")) goto error;
            if (!require_set(b, "set-union")) goto error;
            PyObject *ea = PyObject_GetAttrString(a, "elements");
            if (ea == NULL) goto error;
            PyObject *mb = PyObject_GetAttrString(b, "members");
            if (mb == NULL) {
                Py_DECREF(ea);
                goto error;
            }
            PyObject *eb = PyObject_GetAttrString(b, "elements");
            if (eb == NULL) {
                Py_DECREF(mb);
                Py_DECREF(ea);
                goto error;
            }
            Py_ssize_t na = PyTuple_GET_SIZE(ea), nb = PyTuple_GET_SIZE(eb);
            PyObject *new_list = PyList_New(0);
            if (new_list == NULL) {
                Py_DECREF(eb);
                Py_DECREF(mb);
                Py_DECREF(ea);
                goto error;
            }
            PyObject *seen = PyDict_New();
            if (seen == NULL) {
                Py_DECREF(new_list);
                Py_DECREF(eb);
                Py_DECREF(mb);
                Py_DECREF(ea);
                goto error;
            }
            /* Add all of a's elements */
            for (Py_ssize_t i = 0; i < na; i++) {
                PyObject *e = PyTuple_GET_ITEM(ea, i);
                PyObject *hk = PyObject_CallMethod((PyObject *)Menai_DictType, "to_hashable_key", "O", e);
                if (hk == NULL) {
                    Py_DECREF(seen);
                    Py_DECREF(eb);
                    Py_DECREF(mb);
                    Py_DECREF(ea);
                    Py_DECREF(new_list);
                    goto error;
                }
                Py_INCREF(e);
                if (PyList_Append(new_list, e) < 0 || PyDict_SetItem(seen, hk, Py_True) < 0) {
                    Py_DECREF(e);
                    Py_DECREF(hk);
                    Py_DECREF(seen);
                    Py_DECREF(eb);
                    Py_DECREF(mb);
                    Py_DECREF(ea);
                    Py_DECREF(new_list);
                    goto error;
                }
                Py_DECREF(e);
                Py_DECREF(hk);
            }
            /* Add b's elements not in a */
            for (Py_ssize_t i = 0; i < nb; i++) {
                PyObject *e = PyTuple_GET_ITEM(eb, i);
                PyObject *hk = PyObject_CallMethod((PyObject *)Menai_DictType, "to_hashable_key", "O", e);
                if (hk == NULL) {
                    Py_DECREF(seen);
                    Py_DECREF(eb);
                    Py_DECREF(mb);
                    Py_DECREF(ea);
                    Py_DECREF(new_list);
                    goto error;
                }
                if (!PyDict_Contains(seen, hk)) {
                    Py_INCREF(e);
                    if (PyList_Append(new_list, e) < 0) {
                        Py_DECREF(e);
                        Py_DECREF(hk);
                        Py_DECREF(seen);
                        Py_DECREF(eb);
                        Py_DECREF(mb);
                        Py_DECREF(ea);
                        Py_DECREF(new_list);
                        goto error;
                    }
                    Py_DECREF(e);
                }
                Py_DECREF(hk);
            }
            Py_DECREF(seen);
            Py_DECREF(eb);
            Py_DECREF(mb);
            Py_DECREF(ea);
            {
                PyObject *new_tup = PyList_AsTuple(new_list);
                Py_DECREF(new_list);
                if (new_tup == NULL) goto error;
                PyObject *r = PyObject_CallOneArg((PyObject *)Menai_SetType, new_tup);
                Py_DECREF(new_tup);
                if (r == NULL) goto error;
                reg_set(regs, base + dest, r);
                Py_DECREF(r);
            }
            break;
        }

        case OP_SET_INTERSECTION: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-intersection")) goto error;
            if (!require_set(b, "set-intersection")) goto error;
            PyObject *ea = PyObject_GetAttrString(a, "elements");
            if (ea == NULL) goto error;
            PyObject *mb = PyObject_GetAttrString(b, "members");
            if (mb == NULL) {
                Py_DECREF(ea);
                goto error;
            }
            Py_ssize_t na = PyTuple_GET_SIZE(ea);
            PyObject *new_list = PyList_New(0);
            if (new_list == NULL) {
                Py_DECREF(mb);
                Py_DECREF(ea);
                goto error;
            }
            for (Py_ssize_t i = 0; i < na; i++) {
                PyObject *e = PyTuple_GET_ITEM(ea, i);
                PyObject *hk = PyObject_CallMethod((PyObject *)Menai_DictType, "to_hashable_key", "O", e);
                if (hk == NULL) {
                    Py_DECREF(new_list);
                    Py_DECREF(mb);
                    Py_DECREF(ea);
                    goto error;
                }
                int in_b = PySequence_Contains(mb, hk);
                Py_DECREF(hk);
                if (in_b < 0) {
                    Py_DECREF(new_list);
                    Py_DECREF(mb);
                    Py_DECREF(ea);
                    goto error;
                }
                if (in_b) {
                    Py_INCREF(e);
                    if (PyList_Append(new_list, e) < 0) {
                        Py_DECREF(e);
                        Py_DECREF(new_list);
                        Py_DECREF(mb);
                        Py_DECREF(ea);
                        goto error;
                    }
                    Py_DECREF(e);
                }
            }
            Py_DECREF(mb);
            Py_DECREF(ea);
            PyObject *new_tup = PyList_AsTuple(new_list);
            Py_DECREF(new_list);
            if (new_tup == NULL) goto error;
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_SetType, new_tup);
            Py_DECREF(new_tup);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_SET_DIFFERENCE: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-difference")) goto error;
            if (!require_set(b, "set-difference")) goto error;
            PyObject *ea = PyObject_GetAttrString(a, "elements");
            if (ea == NULL) goto error;
            PyObject *mb = PyObject_GetAttrString(b, "members");
            if (mb == NULL) {
                Py_DECREF(ea);
                goto error;
            }
            Py_ssize_t na = PyTuple_GET_SIZE(ea);
            PyObject *new_list = PyList_New(0);
            if (new_list == NULL) {
                Py_DECREF(mb);
                Py_DECREF(ea);
                goto error;
            }
            for (Py_ssize_t i = 0; i < na; i++) {
                PyObject *e = PyTuple_GET_ITEM(ea, i);
                PyObject *hk = PyObject_CallMethod((PyObject *)Menai_DictType, "to_hashable_key", "O", e);
                if (hk == NULL) {
                    Py_DECREF(new_list);
                    Py_DECREF(mb);
                    Py_DECREF(ea);
                    goto error;
                }
                int in_b = PySequence_Contains(mb, hk);
                Py_DECREF(hk);
                if (in_b < 0) {
                    Py_DECREF(new_list);
                    Py_DECREF(mb);
                    Py_DECREF(ea);
                    goto error;
                }
                if (!in_b) {
                    Py_INCREF(e);
                    if (PyList_Append(new_list, e) < 0) {
                        Py_DECREF(e);
                        Py_DECREF(new_list);
                        Py_DECREF(mb);
                        Py_DECREF(ea);
                        goto error;
                    }
                    Py_DECREF(e);
                }
            }
            Py_DECREF(mb);
            Py_DECREF(ea);
            PyObject *new_tup = PyList_AsTuple(new_list);
            Py_DECREF(new_list);
            if (new_tup == NULL) goto error;
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_SetType, new_tup);
            Py_DECREF(new_tup);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_SET_SUBSET_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-subset?")) goto error;
            if (!require_set(b, "set-subset?")) goto error;
            PyObject *ma = PyObject_GetAttrString(a, "members");
            if (ma == NULL) goto error;
            PyObject *mb = PyObject_GetAttrString(b, "members");
            if (mb == NULL) {
                Py_DECREF(ma);
                goto error;
            }
            /* frozenset.issubset: use PyObject_CallMethod */
            int r = PyObject_RichCompareBool(ma, mb, Py_LE);
            Py_DECREF(ma);
            Py_DECREF(mb);
            if (r < 0) goto error;
            bool_store(regs, base + dest, r);
            break;
        }

        case OP_SET_TO_LIST: {
            PyObject *a = regs[base + src0];
            if (!require_set_singular(a, "set->list")) goto error;
            PyObject *elems = PyObject_GetAttrString(a, "elements");
            if (elems == NULL) goto error;
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, elems);
            Py_DECREF(elems);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_RANGE: {
            /* src0=start, src1=end, src2=step — all integers */
            PyObject *ra = regs[base + src0], *rb = regs[base + src1], *rc = regs[base + src2];
            if (!IS_MENAI_INTEGER(ra) || !IS_MENAI_INTEGER(rb) || !IS_MENAI_INTEGER(rc)) {
                menai_raise_eval_error("range requires integer arguments");
                goto error;
            }
            PyObject *av = menai_integer_value(ra);
            if (av == NULL) goto error;
            PyObject *bv = menai_integer_value(rb);
            if (bv == NULL) goto error;
            PyObject *cv = menai_integer_value(rc);
            if (cv == NULL) goto error;
            long start = PyLong_AsLong(av), end = PyLong_AsLong(bv), step = PyLong_AsLong(cv);
            if ((start == -1 || end == -1 || step == -1) && PyErr_Occurred()) goto error;
            if (step == 0) {
                menai_raise_eval_error("range: step cannot be zero");
                goto error;
            }
            /* Compute length */
            Py_ssize_t n = 0;
            if (step > 0 && end > start) n = (end - start + step - 1) / step;
            else if (step < 0 && end < start) n = (start - end - step - 1) / (-step);
            PyObject *tup = PyTuple_New(n);
            if (tup == NULL) goto error;
            long val = start;
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *iv = PyLong_FromLong(val);
                if (iv == NULL) {
                    Py_DECREF(tup);
                    goto error;
                }
                PyObject *mi = make_integer(iv);
                Py_DECREF(iv);
                if (mi == NULL) {
                    Py_DECREF(tup);
                    goto error;
                }
                PyTuple_SET_ITEM(tup, i, mi);
                val += step;
            }
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, tup);
            Py_DECREF(tup);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_MAKE_STRUCT: {
            /*
             * MAKE_STRUCT src0, src1:
             * src0 = absolute slot of MenaiStructType descriptor in outgoing zone.
             * src1 = field count. Fields are in slots src0+1..src0+n_fields.
             */
            PyObject *struct_type = regs[base + src0];
            if (!IS_MENAI_STRUCTTYPE(struct_type)) {
                menai_raise_eval_error("struct constructor: first argument must be a struct type");
                goto error;
            }
            int n_fields = src1;
            PyObject *fields = PyTuple_New(n_fields);
            if (fields == NULL) goto error;
            for (int i = 0; i < n_fields; i++) {
                PyObject *fv = regs[base + src0 + 1 + i];
                Py_INCREF(fv);
                PyTuple_SET_ITEM(fields, i, fv);
            }
            PyObject *kwargs = Py_BuildValue("{sOsO}", "struct_type", struct_type, "fields", fields);
            Py_DECREF(fields);
            if (kwargs == NULL) goto error;
            PyObject *instance = PyObject_Call((PyObject *)Menai_StructType, empty_tuple, kwargs);
            Py_DECREF(kwargs);
            if (instance == NULL) goto error;
            reg_set(regs, base + dest, instance);
            Py_DECREF(instance);
            break;
        }

        case OP_STRUCT_P:
            bool_store(regs, base + dest, IS_MENAI_STRUCT(regs[base + src0]));
            break;

        case OP_STRUCT_TYPE_P: {
            PyObject *stype = regs[base + src0], *val = regs[base + src1];
            if (!require_structtype(stype, "struct-type?")) goto error;
            if (!IS_MENAI_STRUCT(val)) {
                bool_store(regs, base + dest, 0);
                break;
            }
            PyObject *val_stype = PyObject_GetAttrString(val, "struct_type");
            if (val_stype == NULL) goto error;
            PyObject *tag_a = PyObject_GetAttrString(stype, "tag");
            if (tag_a == NULL) {
                Py_DECREF(val_stype);
                goto error;
            }
            PyObject *tag_b = PyObject_GetAttrString(val_stype, "tag");
            Py_DECREF(val_stype);
            if (tag_b == NULL) {
                Py_DECREF(tag_a);
                goto error;
            }
            int eq = PyObject_RichCompareBool(tag_a, tag_b, Py_EQ);
            Py_DECREF(tag_a);
            Py_DECREF(tag_b);
            if (eq < 0) goto error;
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_STRUCT_GET: {
            /* src1 holds a MenaiSymbol field name */
            PyObject *val = regs[base + src0], *field_sym = regs[base + src1];
            if (!require_struct(val, "struct-get")) goto error;
            if (!require_symbol(field_sym, "struct-get")) goto error;
            PyObject *stype = PyObject_GetAttrString(val, "struct_type");
            if (stype == NULL) goto error;
            PyObject *name = menai_symbol_name(field_sym);
            if (name == NULL) goto error;
            PyObject *idx = PyObject_CallMethod(stype, "field_index", "O", name);
            if (idx == NULL) {
                if (PyErr_ExceptionMatches(PyExc_KeyError)) {
                    PyErr_Clear();
                    PyObject *stype_name = PyObject_GetAttrString(stype, "name");
                    if (stype_name != NULL) {
                        menai_raise_eval_errorf(
                            "'struct-get': struct '%s' has no field '%s'",
                            PyUnicode_AsUTF8(stype_name), PyUnicode_AsUTF8(name));
                        Py_DECREF(stype_name);
                    }
                }
                Py_DECREF(stype);
                goto error;
            }
            Py_DECREF(stype);
            Py_ssize_t fi = PyLong_AsSsize_t(idx);
            Py_DECREF(idx);
            if (fi == -1 && PyErr_Occurred()) goto error;
            PyObject *fields = PyObject_GetAttrString(val, "fields");
            if (fields == NULL) goto error;
            PyObject *fv = PyTuple_GET_ITEM(fields, fi);
            reg_set(regs, base + dest, fv);
            Py_DECREF(fields);
            break;
        }

        case OP_STRUCT_GET_IMM: {
            /* src1 holds a MenaiInteger field index */
            PyObject *val = regs[base + src0], *fidx = regs[base + src1];
            if (!require_struct(val, "struct-get-imm")) goto error;
            if (!require_integer(fidx, "struct-get-imm")) goto error;
            PyObject *iv = menai_integer_value(fidx);
            if (iv == NULL) goto error;
            Py_ssize_t fi = PyLong_AsSsize_t(iv);
            if (fi == -1 && PyErr_Occurred()) goto error;
            PyObject *fields = PyObject_GetAttrString(val, "fields");
            if (fields == NULL) goto error;
            PyObject *fv = PyTuple_GET_ITEM(fields, fi);
            reg_set(regs, base + dest, fv);
            Py_DECREF(fields);
            break;
        }

        case OP_STRUCT_SET: {
            PyObject *val = regs[base + src0], *field_sym = regs[base + src1], *new_val = regs[base + src2];
            if (!require_struct(val, "struct-set")) goto error;
            if (!require_symbol(field_sym, "struct-set")) goto error;
            PyObject *stype = PyObject_GetAttrString(val, "struct_type");
            if (stype == NULL) goto error;
            PyObject *name = menai_symbol_name(field_sym);
            if (name == NULL) goto error;
            PyObject *idx = PyObject_CallMethod(stype, "field_index", "O", name);
            if (idx == NULL) {
                if (PyErr_ExceptionMatches(PyExc_KeyError)) {
                    PyErr_Clear();
                    PyObject *stype_name = PyObject_GetAttrString(stype, "name");
                    if (stype_name != NULL) {
                        menai_raise_eval_errorf(
                            "'struct-set': struct '%s' has no field '%s'",
                            PyUnicode_AsUTF8(stype_name), PyUnicode_AsUTF8(name));
                        Py_DECREF(stype_name);
                    }
                }
                Py_DECREF(stype);
                goto error;
            }
            Py_ssize_t fi = PyLong_AsSsize_t(idx);
            Py_DECREF(idx);
            if (fi == -1 && PyErr_Occurred()) {
                Py_DECREF(stype);
                goto error;
            }
            PyObject *fields = PyObject_GetAttrString(val, "fields");
            if (fields == NULL) {
                Py_DECREF(stype);
                goto error;
            }
            Py_ssize_t nf = PyTuple_GET_SIZE(fields);
            PyObject *new_fields = PyTuple_New(nf);
            if (new_fields == NULL) {
                Py_DECREF(fields);
                Py_DECREF(stype);
                goto error;
            }
            for (Py_ssize_t i = 0; i < nf; i++) {
                PyObject *fv = (i == fi) ? new_val : PyTuple_GET_ITEM(fields, i);
                Py_INCREF(fv);
                PyTuple_SET_ITEM(new_fields, i, fv);
            }
            Py_DECREF(fields);
            PyObject *kwargs = Py_BuildValue("{sOsO}", "struct_type", stype, "fields", new_fields);
            Py_DECREF(stype);
            Py_DECREF(new_fields);
            if (kwargs == NULL) goto error;
            PyObject *r = PyObject_Call((PyObject *)Menai_StructType, empty_tuple, kwargs);
            Py_DECREF(kwargs);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRUCT_SET_IMM: {
            PyObject *val = regs[base + src0], *fidx = regs[base + src1], *new_val = regs[base + src2];
            if (!require_struct(val, "struct-set-imm")) goto error;
            if (!require_integer(fidx, "struct-set-imm")) goto error;
            PyObject *iv = menai_integer_value(fidx);
            if (iv == NULL) goto error;
            Py_ssize_t fi = PyLong_AsSsize_t(iv);
            if (fi == -1 && PyErr_Occurred()) goto error;
            PyObject *stype = PyObject_GetAttrString(val, "struct_type");
            if (stype == NULL) goto error;
            PyObject *fields = PyObject_GetAttrString(val, "fields");
            if (fields == NULL) {
                Py_DECREF(stype);
                goto error;
            }
            Py_ssize_t nf = PyTuple_GET_SIZE(fields);
            PyObject *new_fields = PyTuple_New(nf);
            if (new_fields == NULL) {
                Py_DECREF(fields);
                Py_DECREF(stype);
                goto error;
            }
            for (Py_ssize_t i = 0; i < nf; i++) {
                PyObject *fv = (i == fi) ? new_val : PyTuple_GET_ITEM(fields, i);
                Py_INCREF(fv);
                PyTuple_SET_ITEM(new_fields, i, fv);
            }
            Py_DECREF(fields);
            PyObject *kwargs = Py_BuildValue("{sOsO}", "struct_type", stype, "fields", new_fields);
            Py_DECREF(stype);
            Py_DECREF(new_fields);
            if (kwargs == NULL) goto error;
            PyObject *r = PyObject_Call((PyObject *)Menai_StructType, empty_tuple, kwargs);
            Py_DECREF(kwargs);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRUCT_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_struct(a, "struct=?")) goto error;
            if (!require_struct(b, "struct=?")) goto error;
            int eq = PyObject_RichCompareBool(a, b, Py_EQ);
            if (eq < 0) goto error;
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_STRUCT_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_struct(a, "struct!=?")) goto error;
            if (!require_struct(b, "struct!=?")) goto error;
            int neq = PyObject_RichCompareBool(a, b, Py_NE);
            if (neq < 0) goto error;
            bool_store(regs, base + dest, neq);
            break;
        }

        case OP_STRUCT_TYPE: {
            PyObject *val = regs[base + src0];
            if (!require_struct(val, "struct-type")) goto error;
            PyObject *stype = PyObject_GetAttrString(val, "struct_type");
            if (stype == NULL) goto error;
            reg_set(regs, base + dest, stype);
            Py_DECREF(stype);
            break;
        }

        case OP_STRUCT_TYPE_NAME: {
            PyObject *val = regs[base + src0];
            if (!require_structtype(val, "struct-type-name")) goto error;
            PyObject *name = PyObject_GetAttrString(val, "name");
            if (name == NULL) goto error;
            PyObject *r = make_string_from_pyobj(name);
            Py_DECREF(name);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        case OP_STRUCT_FIELDS: {
            PyObject *val = regs[base + src0];
            if (!require_structtype(val, "struct-fields")) goto error;
            PyObject *field_names = PyObject_GetAttrString(val, "field_names");
            if (field_names == NULL) goto error;
            Py_ssize_t n = PyTuple_GET_SIZE(field_names);
            PyObject *tup = PyTuple_New(n);
            if (tup == NULL) {
                Py_DECREF(field_names);
                goto error;
            }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *fname = PyTuple_GET_ITEM(field_names, i);
                /* Wrap in MenaiSymbol */
                PyObject *sym = PyObject_CallOneArg((PyObject *)Menai_SymbolType, fname);
                if (sym == NULL) {
                    Py_DECREF(tup);
                    Py_DECREF(field_names);
                    goto error;
                }
                PyTuple_SET_ITEM(tup, i, sym);
            }
            Py_DECREF(field_names);
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_ListType, tup);
            Py_DECREF(tup);
            if (r == NULL) goto error;
            reg_set(regs, base + dest, r);
            Py_DECREF(r);
            break;
        }

        default:
            menai_raise_eval_errorf("Unimplemented opcode: %d", opcode);
            goto error;
        }

        continue;

error:
        /* Release all live frames above the sentinel. */
        for (int d = frame_depth; d >= 1; d--)
            frame_release(&frames[d]);
        return NULL;
    }
}

/* ---------------------------------------------------------------------------
 * menai_vm_c_execute — the Python-callable entry point
 * ------------------------------------------------------------------------- */

static PyObject *
menai_vm_c_execute(PyObject *self, PyObject *args)
{
    PyObject *code;
    PyObject *constants_dict;
    PyObject *prelude_dict;

    if (!PyArg_ParseTuple(args, "OOO", &code, &constants_dict, &prelude_dict))
        return NULL;

    /* Convert compiler-world constants in the code object tree to fast C types. */
    PyObject *_tmp = PyObject_CallOneArg(fn_convert_code_object, code);
    if (_tmp == NULL)
        return NULL;
    Py_DECREF(_tmp);

    /* Convert constants dict (pi, e, etc.) from slow to fast types. */
    PyObject *fast_constants = PyDict_New();
    if (fast_constants == NULL) return NULL;
    {
        PyObject *ckey, *cval;
        Py_ssize_t cpos = 0;
        while (PyDict_Next(constants_dict, &cpos, &ckey, &cval)) {
            PyObject *converted = PyObject_CallOneArg(fn_convert_value, cval);
            if (converted == NULL) {
                Py_DECREF(fast_constants);
                return NULL;
            }
            int ok = PyDict_SetItem(fast_constants, ckey, converted);
            Py_DECREF(converted);
            if (ok < 0) {
                Py_DECREF(fast_constants);
                return NULL;
            }
        }
    }

    /* Build the globals dict (constants + prelude), converting prelude values
     * from slow compiler-world types to fast C types. */
    PyObject *globals;
    if (prelude_dict != Py_None && PyDict_Size(prelude_dict) > 0) {
        PyObject *fast_prelude = PyDict_New();
        if (fast_prelude == NULL) {
            Py_DECREF(fast_constants);
            return NULL;
        }
        PyObject *pkey, *pval;
        Py_ssize_t ppos = 0;
        while (PyDict_Next(prelude_dict, &ppos, &pkey, &pval)) {
            PyObject *converted = PyObject_CallOneArg(fn_convert_value, pval);
            if (converted == NULL) {
                Py_DECREF(fast_prelude);
                Py_DECREF(fast_constants);
                return NULL;
            }
            int ok = PyDict_SetItem(fast_prelude, pkey, converted);
            Py_DECREF(converted);
            if (ok < 0) {
                Py_DECREF(fast_prelude);
                Py_DECREF(fast_constants);
                return NULL;
            }
        }
        globals = build_globals(fast_constants, fast_prelude);
        Py_DECREF(fast_prelude);
    } else {
        globals = build_globals(fast_constants, prelude_dict);
    }
    Py_DECREF(fast_constants);
    if (globals == NULL)
        return NULL;

    /* Compute the register window size. */
    int max_locals = max_local_count(code);
    if (max_locals < 0) {
        Py_DECREF(globals);
        return NULL;
    }

    /* Also scan prelude functions for their max_local_count. */
    if (prelude_dict != Py_None && PyDict_Size(prelude_dict) > 0) {
        PyObject *key, *val;
        Py_ssize_t pos = 0;
        while (PyDict_Next(globals, &pos, &key, &val)) {
            if (IS_MENAI_FUNCTION(val)) {
                PyObject *bc = PyObject_GetAttrString(val, "bytecode");
                if (bc == NULL) {
                    Py_DECREF(globals);
                    return NULL;
                }
                int n = max_local_count(bc);
                Py_DECREF(bc);
                if (n < 0) {
                    Py_DECREF(globals);
                    return NULL;
                }
                if (n > max_locals)
                    max_locals = n;
            }
        }
    }

    /* Allocate the register array. */
    PyObject **regs = regs_alloc(MAX_FRAME_DEPTH, max_locals);
    if (regs == NULL) {
        Py_DECREF(globals);
        return NULL;
    }

    /* Run the VM. */
    PyObject *result = execute_loop(code, globals, regs, max_locals);

    /* Clean up. */
    regs_free(regs, MAX_FRAME_DEPTH, max_locals);
    Py_DECREF(globals);

    if (result == NULL)
        return NULL;

    /* Convert fast C types back to compiler-world types. */
    PyObject *slow_result = PyObject_CallOneArg(fn_to_slow, result);
    Py_DECREF(result);
    return slow_result;
}

/* ---------------------------------------------------------------------------
 * Module definition
 * ------------------------------------------------------------------------- */

static PyMethodDef menai_vm_c_methods[] = {
    {
        "execute",
        menai_vm_c_execute,
        METH_VARARGS,
        "Execute a Menai CodeObject and return the result."
    },
    { NULL, NULL, 0, NULL }
};

static struct PyModuleDef menai_vm_c_module = {
    PyModuleDef_HEAD_INIT,
    "menai.menai_vm_c",
    NULL,
    -1,
    menai_vm_c_methods
};

PyMODINIT_FUNC
PyInit_menai_vm_c(void)
{
    PyObject *module = PyModule_Create(&menai_vm_c_module);
    if (module == NULL)
        return NULL;

    if (menai_vm_shim_init() < 0) {
        Py_DECREF(module);
        return NULL;
    }

    return module;
}
