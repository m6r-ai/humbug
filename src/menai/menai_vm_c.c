/*
 * menai_vm_c.c — C implementation of the Menai VM execute loop.
 *
 * Exposes a single Python-callable function:
 *
 * menai_vm_c.execute(code, globals_dict, prelude_dict) -> MenaiValue
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

/* menai_value_c init — lives in the same .so */
extern PyObject *_menai_value_c_init(void);

/*
 * Limits
 */
#define MAX_FRAME_DEPTH 1024

/*
 * Cancellation check interval.
 *
 * PyErr_CheckSignals is not free — it was measured at ~6.7% of total CPU at
 * an interval of 1000.  Menai is a pure functional language with no I/O side
 * effects, so a few extra milliseconds of Ctrl-C latency is acceptable.
 * 1 << 17 = 131072 instructions between checks.
 */
#define CANCEL_CHECK_INTERVAL (1 << 17)

/*
 * Instruction encoding constants — must match menai_bytecode.py
 */
#define OPCODE_SHIFT 48
#define DEST_SHIFT   36
#define SRC0_SHIFT   24
#define SRC1_SHIFT   12
#define FIELD_MASK   0xFFFu
#define OPCODE_MASK  0xFFFFu

/*
 * Opcode values — must match menai_bytecode.py Opcode enum
 */
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

/*
 * Shim state — definitions of the externs declared in menai_vm_shim.h
 */
PyTypeObject *Menai_NoneType = NULL;
PyTypeObject *Menai_BooleanType = NULL;
PyTypeObject *Menai_IntegerType = NULL;
PyTypeObject *Menai_FloatType = NULL;
PyTypeObject *Menai_ComplexType = NULL;
PyTypeObject *Menai_StringType = NULL;
PyTypeObject *Menai_SymbolType = NULL;
PyTypeObject *Menai_ListType = NULL;
PyTypeObject *Menai_DictType = NULL;
PyTypeObject *Menai_SetType = NULL;
PyTypeObject *Menai_FunctionType = NULL;
PyTypeObject *Menai_StructTypeType = NULL;
PyTypeObject *Menai_StructType = NULL;

PyObject *Menai_NONE = NULL;
PyObject *Menai_TRUE = NULL;
PyObject *Menai_FALSE = NULL;
PyObject *Menai_EMPTY_LIST = NULL;
PyObject *Menai_EMPTY_DICT = NULL;
PyObject *Menai_EMPTY_SET = NULL;

/*
 * Module-level state fetched at init
 */
static PyObject *MenaiEvalError_type = NULL;
static PyObject *MenaiCancelledException_type = NULL;
static PyObject *fn_convert_code_object = NULL;
static PyObject *fn_convert_value = NULL;
static PyObject *fn_to_slow = NULL;

/*
 * Interned method name strings — cached at init time so string method calls
 * in the hot loop use PyObject_CallMethodOneArg/NoArgs instead of the
 * format-string-parsing PyObject_CallMethod.
 */
static PyObject *_str_upper   = NULL;
static PyObject *_str_lower   = NULL;
static PyObject *_str_strip   = NULL;
static PyObject *_str_lstrip  = NULL;
static PyObject *_str_rstrip  = NULL;
static PyObject *_str_split   = NULL;
static PyObject *_str_replace = NULL;

/*
 * Fast type-check macros
 */
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

/*
 * Wrap an already-deduplicated elements tuple as a MenaiSet, stealing the
 * tuple reference.  Builds the frozenset of hashable keys using the same
 * to_hashable_key method exposed on MenaiDictType.
 * Returns a new reference, or NULL on error (tuple ref still stolen).
 */
static PyObject *
menai_set_from_elements(PyObject *elements)
{
    Py_ssize_t n = PyTuple_GET_SIZE(elements);
    PyObject *members_set = PySet_New(NULL);
    if (!members_set) {
        Py_DECREF(elements);
       	return NULL;
    }
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *hk = menai_hashable_key(PyTuple_GET_ITEM(elements, i));
            if (!hk) {
            Py_DECREF(members_set);
            Py_DECREF(elements);
            return NULL;
        }

        if (PySet_Add(members_set, hk) < 0) {
            Py_DECREF(hk);
            Py_DECREF(members_set);
    	    Py_DECREF(elements);
            return NULL;
        }

        Py_DECREF(hk);
    }

    PyObject *members = PyFrozenSet_New(members_set);
    Py_DECREF(members_set);
    if (!members) {
        Py_DECREF(elements);
       	return NULL;
    }

    MenaiSet_Object *obj = (MenaiSet_Object *)Menai_SetType->tp_alloc(Menai_SetType, 0);
    if (!obj) {
        Py_DECREF(members);
       	Py_DECREF(elements);
       	return NULL;
    }

    obj->elements = elements;  /* steal */
    obj->members  = members;   /* steal */
    return (PyObject *)obj;
}

static inline int menai_boolean_value(PyObject *o) {
    return ((MenaiBoolean_Object *)o)->value;
}

static inline double menai_float_value(PyObject *o) {
    return ((MenaiFloat_Object *)o)->value;
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

static inline void reg_set_own(PyObject **regs, int slot, PyObject *val) {
    PyObject *old = regs[slot];
    regs[slot] = val;
    Py_DECREF(old);
}

static inline void reg_set_borrow(PyObject **regs, int slot, PyObject *val) {
    PyObject *old = regs[slot];
    Py_INCREF(val);
    regs[slot] = val;
    Py_DECREF(old);
}

/*
 * pylong_compare — fast integer comparison bypassing PyObject_RichCompareBool.
 *
 * For the common case where both values fit in a C long, this reduces to two
 * PyLong_AsLong calls and a C comparison — no Python dispatch overhead.
 * Falls back to PyObject_RichCompareBool for bignums (where PyLong_AsLong
 * returns -1 with OverflowError set).
 *
 * op must be one of Py_EQ, Py_NE, Py_LT, Py_GT, Py_LE, Py_GE.
 */
static inline int
pylong_compare(PyObject *a, PyObject *b, int op)
{
    long la = PyLong_AsLong(a);
    if (la != -1 || !PyErr_Occurred()) {
        long lb = PyLong_AsLong(b);
        if (lb != -1 || !PyErr_Occurred()) {
            switch (op) {
                case Py_EQ: return la == lb;
                case Py_NE: return la != lb;
                case Py_LT: return la <  lb;
                case Py_GT: return la >  lb;
                case Py_LE: return la <= lb;
                case Py_GE: return la >= lb;
            }
        }
        PyErr_Clear();
    } else {
        PyErr_Clear();
    }
    return PyObject_RichCompareBool(a, b, op);
}

static inline PyObject *make_integer_value(PyObject *py_int) {
    if (!py_int) return NULL;

    MenaiInteger_Object *r = (MenaiInteger_Object *)Menai_IntegerType->tp_alloc(Menai_IntegerType, 0);
    if (r) {
        r->value = py_int;
    } else {
        Py_DECREF(py_int);
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
    reg_set_borrow(regs, slot, cond ? Menai_TRUE : Menai_FALSE);
}

static inline PyObject *
menai_complex_value(PyObject *obj)
{
    return ((MenaiComplex_Object *)obj)->value;
}

PyObject *menai_raise_eval_error(const char *message);
PyObject *menai_raise_eval_errorf(const char *fmt, ...);

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

static int
fetch_type(PyObject *module, const char *name, PyTypeObject **dst)
{
    PyObject *obj = PyObject_GetAttrString(module, name);
    if (obj == NULL) return -1;
 
    if (!PyType_Check(obj)) {
        PyErr_Format(PyExc_TypeError, "menai_vm_shim_init: %s is not a type", name);
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
        PyErr_Format(PyExc_TypeError, "menai_vm_shim_init: %s is not callable", name);
        Py_DECREF(obj);
        return -1;
    }

    *dst = obj;
    return 0;
}

int
menai_vm_shim_init(void)
{
    PyObject *vc = _menai_value_c_init();
    if (vc == NULL) return -1;

    if (fetch_type(vc, "MenaiNone", &Menai_NoneType) < 0) goto fail;
    if (fetch_type(vc, "MenaiBoolean", &Menai_BooleanType) < 0) goto fail;
    if (fetch_type(vc, "MenaiInteger", &Menai_IntegerType) < 0) goto fail;
    if (fetch_type(vc, "MenaiFloat", &Menai_FloatType) < 0) goto fail;
    if (fetch_type(vc, "MenaiComplex", &Menai_ComplexType) < 0) goto fail;
    if (fetch_type(vc, "MenaiString", &Menai_StringType) < 0) goto fail;
    if (fetch_type(vc, "MenaiSymbol", &Menai_SymbolType) < 0) goto fail;
    if (fetch_type(vc, "MenaiList", &Menai_ListType) < 0) goto fail;
    if (fetch_type(vc, "MenaiDict", &Menai_DictType) < 0) goto fail;
    if (fetch_type(vc, "MenaiSet", &Menai_SetType) < 0) goto fail;
    if (fetch_type(vc, "MenaiFunction", &Menai_FunctionType) < 0) goto fail;
    if (fetch_type(vc, "MenaiStructType", &Menai_StructTypeType) < 0) goto fail;
    if (fetch_type(vc, "MenaiStruct", &Menai_StructType) < 0) goto fail;

    if (fetch_singleton(vc, "Menai_NONE", &Menai_NONE) < 0) goto fail;
    if (fetch_singleton(vc, "Menai_BOOLEAN_TRUE", &Menai_TRUE) < 0) goto fail;
    if (fetch_singleton(vc, "Menai_BOOLEAN_FALSE", &Menai_FALSE) < 0) goto fail;
    if (fetch_singleton(vc, "Menai_LIST_EMPTY", &Menai_EMPTY_LIST) < 0) goto fail;
    if (fetch_singleton(vc, "Menai_DICT_EMPTY", &Menai_EMPTY_DICT) < 0) goto fail;
    if (fetch_singleton(vc, "Menai_SET_EMPTY", &Menai_EMPTY_SET) < 0) goto fail;

    if (fetch_callable(vc, "convert_code_object", &fn_convert_code_object) < 0) goto fail;
    if (fetch_callable(vc, "convert_value", &fn_convert_value) < 0) goto fail;
    if (fetch_callable(vc, "to_slow", &fn_to_slow) < 0) goto fail;

    _str_upper   = PyUnicode_InternFromString("upper");
    _str_lower   = PyUnicode_InternFromString("lower");
    _str_strip   = PyUnicode_InternFromString("strip");
    _str_lstrip  = PyUnicode_InternFromString("lstrip");
    _str_rstrip  = PyUnicode_InternFromString("rstrip");
    _str_split   = PyUnicode_InternFromString("split");
    _str_replace = PyUnicode_InternFromString("replace");
    if (!_str_upper || !_str_lower || !_str_strip ||
        !_str_lstrip || !_str_rstrip || !_str_split || !_str_replace)
        goto fail;

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
 * plain C — no Python objects except those listed below, all of which are
 * kept alive by the frame stack.  constants, names, and local_count are
 * cached here at frame_setup time so the hot loop never calls
 * PyObject_GetAttrString on the code object.
 * ------------------------------------------------------------------------- */

typedef struct {
    PyObject       *code_obj;         /* CodeObject — kept alive, not dereferenced in loop */
    PyObject       *constants;        /* borrowed ref — list of fast constant values */
    PyObject       *names;            /* borrowed ref — list of global name strings */
    PyObject       *closure_caches;   /* borrowed ref — list of child _closure_cache tuples */
    uint64_t       *instrs;           /* raw C pointer into the array.array buffer */
    int             code_len;
    int             local_count;
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

/* frame_setup — slow path used only for the top-level CodeObject at execute
 * start.  All subsequent calls go through frame_setup_func which reads
 * pre-cached fields directly from MenaiFunction_Object.
 */
static int
frame_setup(Frame *f, PyObject *code_obj, int base, int return_dest)
{
    PyObject *instrs_obj = PyObject_GetAttrString(code_obj, "instructions");
    if (instrs_obj == NULL) return -1;

    Py_buffer view;
    if (PyObject_GetBuffer(instrs_obj, &view, PyBUF_SIMPLE) < 0) {
        Py_DECREF(instrs_obj);
        return -1;
    }

    PyObject *constants = PyObject_GetAttrString(code_obj, "constants");
    if (constants == NULL) {
        PyBuffer_Release(&view);
        Py_DECREF(instrs_obj);
        return -1;
    }
    PyObject *names = PyObject_GetAttrString(code_obj, "names");
    if (names == NULL) {
        Py_DECREF(constants);
        PyBuffer_Release(&view);
        Py_DECREF(instrs_obj);
        return -1;
    }
    PyObject *lc_obj = PyObject_GetAttrString(code_obj, "local_count");
    if (lc_obj == NULL) {
        Py_DECREF(names);
        Py_DECREF(constants);
        PyBuffer_Release(&view);
        Py_DECREF(instrs_obj);
        return -1;
    }
    int local_count = (int)PyLong_AsLong(lc_obj);
    Py_DECREF(lc_obj);
    if (local_count == -1 && PyErr_Occurred()) {
        Py_DECREF(names);
        Py_DECREF(constants);
        PyBuffer_Release(&view);
        Py_DECREF(instrs_obj);
        return -1;
    }

    Py_INCREF(code_obj);
    Py_XDECREF(f->code_obj);
    f->code_obj    = code_obj;
    f->constants   = constants;   /* borrowed — f->code_obj keeps code_obj alive */
    Py_DECREF(constants);         /* drop owned ref from GetAttrString */
    f->names       = names;       /* borrowed — f->code_obj keeps code_obj alive */
    Py_DECREF(names);             /* drop owned ref from GetAttrString */
    PyObject *_cc = PyObject_GetAttrString(code_obj, "_code_caches");
    f->closure_caches = (_cc && PyList_Check(_cc)) ? _cc : NULL;
    Py_XDECREF(_cc);  /* drop owned ref — f->code_obj keeps code_obj alive */
    PyErr_Clear();
    f->instrs      = (uint64_t *)view.buf;
    f->code_len    = (int)(view.len / sizeof(uint64_t));
    f->local_count = local_count;
    f->ip          = 0;
    f->base        = base;
    f->return_dest = return_dest;
    f->is_sentinel = 0;
    PyBuffer_Release(&view);
    Py_DECREF(instrs_obj);  /* top-level: instrs backed by code_obj.instructions which code_obj owns */
    return 0;
}

/*
 * frame_setup_func — fast path for all function calls.
 * Reads pre-cached fields from func with zero Python API calls.
 */
static inline void
frame_setup_func(Frame *f, MenaiFunction_Object *func,
                 PyObject *code_obj, int base, int return_dest)
{
    Py_INCREF(code_obj);
    Py_XDECREF(f->code_obj);
    f->code_obj    = code_obj;
    f->instrs      = func->instrs;
    f->code_len    = func->code_len;
    f->constants   = func->constants;
    f->names       = func->names;
    f->local_count = func->local_count;
    f->closure_caches = func->closure_caches;  /* borrowed — func owns bytecode which owns it */
    f->ip          = 0;
    f->base        = base;
    f->return_dest = return_dest;
    f->is_sentinel = 0;
}

static void
frame_release(Frame *f)
{
    Py_XDECREF(f->code_obj);
    f->code_obj  = NULL;
    f->instrs    = NULL;
    f->constants = NULL;
    f->names     = NULL;
    f->closure_caches = NULL;
}

/* ---------------------------------------------------------------------------
 * Register array helpers
 *
 * The register array is a flat PyObject* array:
 *   regs[depth * max_locals + slot]
 * All slots are initialised to Menai_NONE (borrowed — the singleton is
 * kept alive by the module).  reg_set_own/reg_set_borrow manage reference counts correctly.
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
 * Slots that hold something other than Menai_NONE were set via reg_set_own/reg_set_borrow
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
    if (code_get_int(code, "local_count", &local_count) < 0) return -1;
    if (code_get_int(code, "outgoing_arg_slots", &outgoing) < 0) return -1;
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

/*
 * build_globals
 *      Merge constants and prelude into a flat PyObject* dict
 *
 * The C VM looks up globals by name on LOAD_NAME.  We build a Python dict
 * once at the start of execute() and keep it for the duration.
 */
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
    MenaiFunction_Object *func = (MenaiFunction_Object *)func_obj;
    PyObject *bytecode = func->bytecode;  /* borrowed — kept alive by func_obj */

    int param_count = func->param_count;
    int is_variadic = func->is_variadic;

    if (is_variadic) {
        int min_arity = param_count - 1;
        if (arity < min_arity) {
            PyObject *name = func->name;
            const char *fname = (name != NULL && name != Py_None)
                                ? PyUnicode_AsUTF8(name) : "<lambda>";
            menai_raise_eval_errorf(
                "Function '%s' expects at least %d argument%s, got %d",
                fname, min_arity, min_arity == 1 ? "" : "s", arity);
            return -1;
        }
        /* Pack excess args into a MenaiList for the rest parameter. */
        int rest_count = arity - min_arity;
        PyObject **rest_arr = rest_count > 0
            ? (PyObject **)PyMem_Malloc(rest_count * sizeof(PyObject *)) : NULL;
        if (rest_count > 0 && !rest_arr) { PyErr_NoMemory(); return -1; }
        for (int k = 0; k < rest_count; k++) {
            rest_arr[k] = regs[callee_base + min_arity + k];
            Py_INCREF(rest_arr[k]);
        }
        PyObject *rest_list = menai_list_from_array_steal(rest_arr, rest_count);
        if (rest_list == NULL) return -1;

        reg_set_own(regs, callee_base + min_arity, rest_list);

    } else if (arity != param_count) {
        PyObject *name = func->name;
        const char *fname = (name != NULL && name != Py_None) ? PyUnicode_AsUTF8(name) : "<lambda>";
        menai_raise_eval_errorf(
            "Function '%s' expects %d argument%s, got %d",
            fname, param_count, param_count == 1 ? "" : "s", arity);
        return -1;
    }

    /* Populate capture slots: regs[callee_base + param_count + i] */
    PyObject *captured = func->captured_values;
    Py_ssize_t ncap = PyList_GET_SIZE(captured);
    for (Py_ssize_t i = 0; i < ncap; i++) {
        PyObject *cv = PyList_GET_ITEM(captured, i);
        /* Most captures are already fast C types (set by OP_PATCH_CLOSURE
            * from registers).  Prelude closures may hold slow-world values
            * that were not converted at load time.  Check with IS_MENAI_*
            * first to avoid a Python call in the common fast case. */
        PyTypeObject *cvt = Py_TYPE(cv);
        if (cvt == Menai_NoneType     || cvt == Menai_BooleanType  ||
            cvt == Menai_IntegerType  || cvt == Menai_FloatType    ||
            cvt == Menai_ComplexType  || cvt == Menai_StringType   ||
            cvt == Menai_SymbolType   || cvt == Menai_ListType     ||
            cvt == Menai_DictType     || cvt == Menai_SetType      ||
            cvt == Menai_FunctionType || cvt == Menai_StructTypeType ||
            cvt == Menai_StructType) {
            reg_set_borrow(regs, callee_base + param_count + (int)i, cv);
        } else {
            PyObject *fast_cv = PyObject_CallOneArg(fn_convert_value, cv);
            if (fast_cv == NULL) return -1;
            reg_set_own(regs, callee_base + param_count + (int)i, fast_cv);
        }
    }

    frame_setup_func(new_frame, func, bytecode, callee_base, return_dest);
    return 0;
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
    frames[0] = (Frame){
        .is_sentinel = 1,
        .code_obj = NULL,
        .constants = NULL,
        .names = NULL,
        .instrs = NULL,
        .closure_caches = NULL,
    };
    frames[1] = (Frame){
        .is_sentinel = 0,
        .code_obj = NULL,
        .constants = NULL,
        .names = NULL,
        .instrs = NULL,
        .closure_caches = NULL,
    };

    /* Set up frame at depth 1 for the top-level code object. */
    if (frame_setup(&frames[1], code, 0, 0) < 0)
        return NULL;

    int frame_depth = 1;
    Frame *frame = &frames[1];
    int instr_count = 0;

    while (1) {
        /* Cancellation check */
        if ((++instr_count & (CANCEL_CHECK_INTERVAL - 1)) == 0) {
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
        int dest = (int)((word >> DEST_SHIFT) & FIELD_MASK);
        int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
        int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
        int src2 = (int)(word & FIELD_MASK);
        int base = frame->base;

        switch (opcode) {

        /* ----------------------------------------------------------------- */
        case OP_LOAD_NONE:
            reg_set_borrow(regs, base + dest, Menai_NONE);
            break;

        case OP_LOAD_TRUE:
            reg_set_borrow(regs, base + dest, Menai_TRUE);
            break;

        case OP_LOAD_FALSE:
            reg_set_borrow(regs, base + dest, Menai_FALSE);
            break;

        case OP_LOAD_EMPTY_LIST:
            reg_set_borrow(regs, base + dest, Menai_EMPTY_LIST);
            break;

        case OP_LOAD_EMPTY_DICT:
            reg_set_borrow(regs, base + dest, Menai_EMPTY_DICT);
            break;

        case OP_LOAD_EMPTY_SET:
            reg_set_borrow(regs, base + dest, Menai_EMPTY_SET);
            break;

        case OP_LOAD_CONST: {
            PyObject *val = PyList_GET_ITEM(frame->constants, src0);
            reg_set_borrow(regs, base + dest, val);
            break;
        }

        case OP_LOAD_NAME: {
            PyObject *name = PyList_GET_ITEM(frame->names, src0);
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
                goto error;
            }
            reg_set_borrow(regs, base + dest, val);
            break;
        }

        case OP_MOVE:
            reg_set_borrow(regs, base + dest, regs[base + src0]);
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

            PyObject *s = PyObject_GetAttrString(msg, "value");
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
            reg_set_own(regs, caller->base + saved_return_dest, retval);

            frame = caller;
            break;
        }

        case OP_CALL: {
            PyObject *raw = regs[base + src0];
            int arity     = src1;

            int callee_base = base + frame->local_count;

            if (IS_MENAI_FUNCTION(raw)) {
                if (frame_depth >= MAX_FRAME_DEPTH) {
                    menai_raise_eval_error("Maximum call depth exceeded");
                    goto error;
                }
                frame_depth++;
                Frame *new_frame = &frames[frame_depth];
                *new_frame = (Frame){ .code_obj = NULL, .closure_caches = NULL,
                                      .constants = NULL, .names = NULL, .instrs = NULL };

                if (call_setup(new_frame, raw, regs, callee_base,
                               arity, dest) < 0) {
                    frame_depth--;
                    goto error;
                }
                frame = new_frame;

            } else if (IS_MENAI_STRUCTTYPE(raw)) {
                /* Struct constructor call */
                Py_ssize_t n_fields = PyTuple_GET_SIZE(((MenaiStructType_Object *)raw)->field_names);
                if (arity != (int)n_fields) {
                    PyObject *sname = ((MenaiStructType_Object *)raw)->name;
                    menai_raise_eval_errorf(
                        "Struct constructor '%s' called with wrong number of arguments",
                        sname ? PyUnicode_AsUTF8(sname) : "?");
                    goto error;
                }
                PyObject *fields = PyTuple_New(n_fields);
                if (fields == NULL) goto error;
                for (int i = 0; i < (int)n_fields; i++) {
                    PyObject *fv = regs[callee_base + i];
                    Py_INCREF(fv);
                    PyTuple_SET_ITEM(fields, i, fv);
                }
                PyObject *instance = menai_struct_alloc(raw, fields);
                if (instance == NULL) goto error;
                reg_set_own(regs, base + dest, instance);

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

            int local_count = frame->local_count;

            if (IS_MENAI_FUNCTION(raw)) {
                /* Move outgoing args down to base+0..n_args-1 in place. */
                for (int i = 0; i < n_args; i++) {
                    PyObject *v = regs[base + local_count + i];
                    reg_set_borrow(regs, base + i, v);
                }

                /* Reuse current frame — release old instructions first. */
                frame->instrs = NULL;

                int saved_return_dest = frame->return_dest;
                if (call_setup(frame, raw, regs, base, n_args, saved_return_dest) < 0) {
                    Py_DECREF(raw);
                    goto error;
                }
                Py_DECREF(raw);
            } else if (IS_MENAI_STRUCTTYPE(raw)) {
                Py_ssize_t n_fields = PyTuple_GET_SIZE(((MenaiStructType_Object *)raw)->field_names);
                if (n_args != (int)n_fields) {
                    PyObject *sname = ((MenaiStructType_Object *)raw)->name;
                    menai_raise_eval_errorf(
                        "Struct constructor '%s' called with wrong number of arguments",
                        sname ? PyUnicode_AsUTF8(sname) : "?");
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
                PyObject *instance = menai_struct_alloc(raw, fields);
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
                reg_set_own(regs, caller->base + saved_return_dest, retval);
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
            PyObject *nb = menai_symbol_name(b);
            bool_store(regs, base + dest, na == nb || PyUnicode_Compare(na, nb) == 0);
            break;
        }

        case OP_SYMBOL_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_symbol_pair(a, b, "symbol!=?")) goto error;
            PyObject *na = menai_symbol_name(a);
            PyObject *nb = menai_symbol_name(b);
            bool_store(regs, base + dest, na != nb && PyUnicode_Compare(na, nb) != 0);
            break;
        }

        case OP_SYMBOL_TO_STRING: {
            PyObject *a = regs[base + src0];
            if (!require_symbol(a, "symbol->string")) goto error;
            PyObject *name = menai_symbol_name(a);
            PyObject *r = make_string_from_pyobj(name);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
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
            MenaiFunction_Object *fn = (MenaiFunction_Object *)f;
            int min_a = fn->is_variadic ? fn->param_count - 1 : fn->param_count;
            PyObject *r = PyLong_FromLong(min_a);
            if (r == NULL) goto error;
            PyObject *_r = make_integer_value(r);
            Py_DECREF(r);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FUNCTION_VARIADIC_P: {
            PyObject *f = regs[base + src0];
            if (!require_function_singular(f, "function-variadic?")) goto error;
            bool_store(regs, base + dest, ((MenaiFunction_Object *)f)->is_variadic);
            break;
        }

        case OP_FUNCTION_ACCEPTS_P: {
            PyObject *f = regs[base + src0];
            PyObject *n_obj = regs[base + src1];
            if (!require_function_singular(f, "function-accepts?")) goto error;
            if (!require_integer(n_obj, "function-accepts?")) goto error;
            MenaiFunction_Object *fn = (MenaiFunction_Object *)f;
            int pc = fn->param_count;
            int is_var = fn->is_variadic;
            PyObject *n_py = menai_integer_value(n_obj);
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
            PyObject *bv = menai_integer_value(b);
            bool_store(regs, base + dest, pylong_compare(av, bv, Py_EQ));
            break;
        }

        case OP_INTEGER_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer!=?")) goto error;
            if (!require_integer(b, "integer!=?")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            bool_store(regs, base + dest, pylong_compare(av, bv, Py_NE));
            break;
        }

        case OP_INTEGER_LT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer<?")) goto error;
            if (!require_integer(b, "integer<?")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            bool_store(regs, base + dest, pylong_compare(av, bv, Py_LT));
            break;
        }

        case OP_INTEGER_GT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer>?")) goto error;
            if (!require_integer(b, "integer>?")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            bool_store(regs, base + dest, pylong_compare(av, bv, Py_GT));
            break;
        }

        case OP_INTEGER_LTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer<=?")) goto error;
            if (!require_integer(b, "integer<=?")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            bool_store(regs, base + dest, pylong_compare(av, bv, Py_LE));
            break;
        }

        case OP_INTEGER_GTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer>=?")) goto error;
            if (!require_integer(b, "integer>=?")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            bool_store(regs, base + dest, pylong_compare(av, bv, Py_GE));
            break;
        }

        case OP_INTEGER_ABS: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-abs")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *_r = make_integer_value(PyNumber_Absolute(av));
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_NEG: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-neg")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *_r = make_integer_value(PyNumber_Negative(av));
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_NOT: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-bit-not")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *_r = make_integer_value(PyNumber_Invert(av));
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_ADD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer+")) goto error;
            if (!require_integer(b, "integer+")) goto error;

            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            PyObject *_r = make_integer_value(PyNumber_Add(av, bv));
            if (!_r) goto error;

            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_SUB: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-")) goto error;
            if (!require_integer(b, "integer-")) goto error;

            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            PyObject *_r = make_integer_value(PyNumber_Subtract(av, bv));
            if (!_r) goto error;

            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_MUL: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer*")) goto error;
            if (!require_integer(b, "integer*")) goto error;

            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            PyObject *_r = make_integer_value(PyNumber_Multiply(av, bv));
            if (!_r) goto error;

            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_DIV: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer/")) goto error;
            if (!require_integer(b, "integer/")) goto error;
            PyObject *bv = menai_integer_value(b);
            long _bvl = PyLong_AsLong(bv);
            /* A bignum divisor (OverflowError from AsLong) is never zero. */
            if (!PyErr_Occurred() && _bvl == 0) {
                menai_raise_eval_error("Division by zero in 'integer/'");
                goto error;
            }
            PyErr_Clear();
            PyObject *av = menai_integer_value(a);
            PyObject *_res = PyNumber_FloorDivide(av, bv);
            PyObject *_r = make_integer_value(_res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_MOD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer%")) goto error;
            if (!require_integer(b, "integer%")) goto error;
            PyObject *bv = menai_integer_value(b);
            long _bvl = PyLong_AsLong(bv);
            if (!PyErr_Occurred() && _bvl == 0) {
                menai_raise_eval_error("Modulo by zero in 'integer%'");
                goto error;
            }
            PyErr_Clear();
            PyObject *av = menai_integer_value(a);
            PyObject *_res = PyNumber_Remainder(av, bv);
            PyObject *_r = make_integer_value(_res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_EXPN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-expn")) goto error;
            if (!require_integer(b, "integer-expn")) goto error;
            PyObject *bv = menai_integer_value(b);
            long _bvl = PyLong_AsLong(bv);
            /* A bignum exponent (OverflowError) is always positive — only reject negative longs. */
            if (!PyErr_Occurred() && _bvl < 0) {
                menai_raise_eval_error("Function 'integer-expn' requires a non-negative exponent");
                goto error;
            }
            PyErr_Clear();
            PyObject *av = menai_integer_value(a);
            PyObject *_res = PyNumber_Power(av, bv, Py_None);
            PyObject *_r = make_integer_value(_res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_OR: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-or")) goto error;
            if (!require_integer(b, "integer-bit-or")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            PyObject *_r = make_integer_value(PyNumber_Or(av, bv));
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_AND: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-and")) goto error;
            if (!require_integer(b, "integer-bit-and")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            PyObject *_r = make_integer_value(PyNumber_And(av, bv));
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_XOR: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-xor")) goto error;
            if (!require_integer(b, "integer-bit-xor")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            PyObject *_r = make_integer_value(PyNumber_Xor(av, bv));
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_SHIFT_LEFT: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-shift-left")) goto error;
            if (!require_integer(b, "integer-bit-shift-left")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            PyObject *_r = make_integer_value(PyNumber_Lshift(av, bv));
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_SHIFT_RIGHT: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-shift-right")) goto error;
            if (!require_integer(b, "integer-bit-shift-right")) goto error;
            PyObject *av = menai_integer_value(a);
            PyObject *bv = menai_integer_value(b);
            PyObject *_r = make_integer_value(PyNumber_Rshift(av, bv));
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_MIN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-min")) goto error;
            if (!require_integer(b, "integer-min")) goto error;
            PyObject *_av = menai_integer_value(a);
            PyObject *_bv = menai_integer_value(b);
            reg_set_borrow(regs, base + dest, pylong_compare(_av, _bv, Py_LE) ? a : b);
            break;
        }

        case OP_INTEGER_MAX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-max")) goto error;
            if (!require_integer(b, "integer-max")) goto error;
            PyObject *_av = menai_integer_value(a);
            PyObject *_bv = menai_integer_value(b);
            reg_set_borrow(regs, base + dest, pylong_compare(_av, _bv, Py_GE) ? a : b);
            break;
        }

        case OP_INTEGER_TO_FLOAT: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer->float")) goto error;
            PyObject *_av = menai_integer_value(a);
            double d = PyLong_AsDouble(_av);
            if (d == -1.0 && PyErr_Occurred()) goto error;
            PyObject *_r = make_float(d);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_TO_COMPLEX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer->complex")) goto error;
            if (!require_integer(b, "integer->complex")) goto error;
            PyObject *_av = menai_integer_value(a);
            double re = PyLong_AsDouble(_av);
            if (re == -1.0 && PyErr_Occurred()) goto error;
            PyObject *_bv = menai_integer_value(b);
            double im = PyLong_AsDouble(_bv);
            if (im == -1.0 && PyErr_Occurred()) goto error;
            PyObject *r = make_complex_from_doubles(re, im);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_INTEGER_TO_STRING: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer->string")) goto error;
            if (!require_integer(b, "integer->string")) goto error;
            PyObject *_bv = menai_integer_value(b);
            long radix = PyLong_AsLong(_bv);
            if (radix == -1 && PyErr_Occurred()) goto error;
            if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
                menai_raise_eval_errorf("integer->string: radix must be 2, 8, 10, or 16, got %ld", radix);
                goto error;
            }
            PyObject *av = menai_integer_value(a);
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
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_INTEGER_CODEPOINT_TO_STRING: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-codepoint->string")) goto error;
            PyObject *_av = menai_integer_value(a);
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
            reg_set_own(regs, base + dest, r);
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
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_ABS: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-abs")) goto error;
            double v = menai_float_value(a);
            {
                PyObject *_r = make_float(fabs(v));
                if (_r == NULL) goto error;
                reg_set_own(regs, base + dest, _r);
            }
            break;
        }

        case OP_FLOAT_ADD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float+")) goto error;
            if (!require_float(b, "float+")) goto error;
            PyObject *_r = make_float(menai_float_value(a) + menai_float_value(b));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_SUB: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float-")) goto error;
            if (!require_float(b, "float-")) goto error;
            PyObject *_r = make_float(menai_float_value(a) - menai_float_value(b));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_MUL: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float*")) goto error;
            if (!require_float(b, "float*")) goto error;
            PyObject *_r = make_float(menai_float_value(a) * menai_float_value(b));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
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
            reg_set_own(regs, base + dest, _r);
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
            reg_set_own(regs, base + dest, _r);
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
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_EXP: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-exp")) goto error;
            PyObject *_r = make_float(exp(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_EXPN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float-expn")) goto error;
            if (!require_float(b, "float-expn")) goto error;
            PyObject *_r = make_float(pow(menai_float_value(a), menai_float_value(b)));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
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
            reg_set_own(regs, base + dest, _r);
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
            reg_set_own(regs, base + dest, _r);
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
            reg_set_own(regs, base + dest, _r);
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
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_SIN: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-sin")) goto error;
            PyObject *_r = make_float(sin(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_COS: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-cos")) goto error;
            PyObject *_r = make_float(cos(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_TAN: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-tan")) goto error;
            PyObject *_r = make_float(tan(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
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
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_FLOOR: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-floor")) goto error;
            PyObject *_r = make_float(floor(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_CEIL: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-ceil")) goto error;
            PyObject *_r = make_float(ceil(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_ROUND: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float-round")) goto error;
            PyObject *_r = make_float(round(menai_float_value(a)));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_MIN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float-min")) goto error;
            if (!require_float(b, "float-min")) goto error;
            double av = menai_float_value(a), bv = menai_float_value(b);
            PyObject *_r = make_float(av <= bv ? av : bv);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_MAX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float-max")) goto error;
            if (!require_float(b, "float-max")) goto error;
            double av = menai_float_value(a), bv = menai_float_value(b);
            PyObject *_r = make_float(av >= bv ? av : bv);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
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
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_TO_COMPLEX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float->complex")) goto error;
            if (!require_float(b, "float->complex")) goto error;
            PyObject *r = make_complex_from_doubles(menai_float_value(a), menai_float_value(b));
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
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
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_MAKE_CLOSURE: {
            /*
             * MAKE_CLOSURE dest, src0:
             * src0 is the index into code_objects of the child CodeObject.
             * Creates a MenaiFunction with captured_values pre-allocated to
             * None, ready for PATCH_CLOSURE to fill in.
             *
             * All metadata is read from the _closure_cache tuple built once
             * by menai_convert_code_object — zero PyObject_GetAttrString
             * calls in the hot loop; closure_caches is pre-loaded at frame
             * setup time.
             */
            if (frame->closure_caches == NULL) {
                menai_raise_eval_error("MAKE_CLOSURE: _code_caches not set on code object");
                goto error;
            }

            PyObject *closure_cache = PyList_GET_ITEM(frame->closure_caches, src0);

            Py_ssize_t ncap = (Py_ssize_t)PyLong_AsLong(PyTuple_GET_ITEM(closure_cache, 3));

            PyObject *cap_list = PyList_New(ncap);
            if (cap_list == NULL) {
                goto error;
            }

            for (Py_ssize_t i = 0; i < ncap; i++) {
                Py_INCREF(Py_None);
                PyList_SET_ITEM(cap_list, i, Py_None);
            }

            /* child_code is the bytecode object stored in the cache at index 9 */
            PyObject *child_code = PyTuple_GET_ITEM(closure_cache, 9);
            PyObject *func = menai_function_alloc(closure_cache, child_code, cap_list);
            Py_DECREF(cap_list);
            if (func == NULL) goto error;
            reg_set_own(regs, base + dest, func);
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
            PyObject *cap_list = ((MenaiFunction_Object *)closure)->captured_values;
            PyObject *val = regs[base + src2];
            Py_INCREF(val);
            int set_ok = PyList_SetItem(cap_list, src1, val); /* steals val ref */
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

            PyObject **elements = ((MenaiList_Object *)raw_args)->elements;
            int arity = (int)((MenaiList_Object *)raw_args)->length;

            if (IS_MENAI_FUNCTION(raw_func)) {
                if (frame_depth >= MAX_FRAME_DEPTH) {
                    menai_raise_eval_error("Maximum call depth exceeded");
                    goto error;
                }

                int callee_base = base + frame->local_count;

                /* Scatter list elements into the callee window */
                for (int i = 0; i < arity; i++)
                    reg_set_borrow(regs, callee_base + i, elements[i]);

                frame_depth++;
                Frame *new_frame = &frames[frame_depth];
                *new_frame = (Frame){ .code_obj = NULL, .closure_caches = NULL,
                                      .constants = NULL, .names = NULL, .instrs = NULL };
                if (call_setup(new_frame, raw_func, regs, callee_base, arity, dest) < 0) {
                    frame_depth--;
                    goto error;
                }
                frame = new_frame;

            } else if (IS_MENAI_STRUCTTYPE(raw_func)) {
                Py_ssize_t n_fields = PyTuple_GET_SIZE(((MenaiStructType_Object *)raw_func)->field_names);
                if (arity != (int)n_fields) {
                    menai_raise_eval_error("Struct constructor called with wrong number of arguments");
                    goto error;
                }
                PyObject *fields = PyTuple_New(n_fields);
                if (fields == NULL) goto error;
                for (int i = 0; i < (int)n_fields; i++) {
                    PyObject *fv = elements[i];
                    Py_INCREF(fv);
                    PyTuple_SET_ITEM(fields, i, fv);
                }
                PyObject *instance = menai_struct_alloc(raw_func, fields);
                if (instance == NULL) goto error;
                reg_set_own(regs, base + dest, instance);
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

            PyObject **elements = ((MenaiList_Object *)raw_args)->elements;
            int arity = (int)((MenaiList_Object *)raw_args)->length;

            if (IS_MENAI_FUNCTION(raw_func)) {
                /* Scatter args into base+0..arity-1 (reusing current frame's base) */
                for (int i = 0; i < arity; i++) reg_set_borrow(regs, base + i, elements[i]);
                Py_DECREF(raw_args);

                /* Release old frame instructions, reuse frame */
                frame->instrs = NULL;

                int saved_return_dest = frame->return_dest;
                if (call_setup(frame, raw_func, regs, base, arity, saved_return_dest) < 0) {
                    Py_DECREF(raw_func);
                    goto error;
                }
                Py_DECREF(raw_func);

            } else if (IS_MENAI_STRUCTTYPE(raw_func)) {
                Py_ssize_t n_fields = PyTuple_GET_SIZE(((MenaiStructType_Object *)raw_func)->field_names);
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
                    PyObject *fv = elements[i];
                    Py_INCREF(fv);
                    PyTuple_SET_ITEM(fields, i, fv);
                }

                PyObject *retval = menai_struct_alloc(raw_func, fields);
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

                reg_set_own(regs, caller->base + saved_return_dest, retval);
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
            bool_store(regs, base + dest, PyObject_RichCompareBool(a, b, Py_EQ));
            break;
        }

        case OP_COMPLEX_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex!=?")) goto error;
            if (!require_complex(b, "complex!=?")) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(a, b, Py_NE));
            break;
        }

        case OP_COMPLEX_REAL: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-real")) goto error;
            PyObject *cv = menai_complex_value(a);
            double r = PyComplex_RealAsDouble(cv);
            PyObject *_fr = make_float(r);
            if (_fr == NULL) goto error;
            reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_IMAG: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-imag")) goto error;
            PyObject *cv = menai_complex_value(a);
            double i = PyComplex_ImagAsDouble(cv);
            PyObject *_fr = make_float(i);
            if (_fr == NULL) goto error;
            reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_ABS: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-abs")) goto error;
            PyObject *cv = menai_complex_value(a);
            double re = PyComplex_RealAsDouble(cv), im = PyComplex_ImagAsDouble(cv);
            PyObject *_fr = make_float(sqrt(re*re + im*im));
            if (_fr == NULL) goto error;
            reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_NEG: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-neg")) goto error;
            PyObject *cv = menai_complex_value(a);
            PyObject *neg = PyNumber_Negative(cv);
            PyObject *_r = make_complex_value(neg);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_ADD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex+")) goto error;
            if (!require_complex(b, "complex+")) goto error;
            PyObject *av = menai_complex_value(a);
            PyObject *bv = menai_complex_value(b);
            PyObject *res = PyNumber_Add(av, bv);
            PyObject *_r = make_complex_value(res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SUB: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex-")) goto error;
            if (!require_complex(b, "complex-")) goto error;
            PyObject *av = menai_complex_value(a);
            PyObject *bv = menai_complex_value(b);
            PyObject *res = PyNumber_Subtract(av, bv);
            PyObject *_r = make_complex_value(res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_MUL: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex*")) goto error;
            if (!require_complex(b, "complex*")) goto error;
            PyObject *av = menai_complex_value(a);
            PyObject *bv = menai_complex_value(b);
            PyObject *res = PyNumber_Multiply(av, bv);
            PyObject *_r = make_complex_value(res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_DIV: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex/")) goto error;
            if (!require_complex(b, "complex/")) goto error;
            PyObject *av = menai_complex_value(a);
            PyObject *bv = menai_complex_value(b);
            /* Check for zero divisor */
            double br = PyComplex_RealAsDouble(bv), bi = PyComplex_ImagAsDouble(bv);
            if (br == 0.0 && bi == 0.0) {
                menai_raise_eval_error("Division by zero in 'complex/'");
                goto error;
            }
            PyObject *res = PyNumber_TrueDivide(av, bv);
            PyObject *_r = make_complex_value(res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_EXPN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex-expn")) goto error;
            if (!require_complex(b, "complex-expn")) goto error;
            PyObject *av = menai_complex_value(a);
            PyObject *bv = menai_complex_value(b);
            PyObject *res = PyNumber_Power(av, bv, Py_None);
            PyObject *_r = make_complex_value(res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_EXP: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-exp")) goto error;
            PyObject *cv = menai_complex_value(a);
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = cexp(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOG: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-log")) goto error;
            PyObject *cv = menai_complex_value(a);
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = clog(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOG10: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-log10")) goto error;
            PyObject *cv = menai_complex_value(a);
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = clog(z) / log(10.0);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SIN: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-sin")) goto error;
            PyObject *cv = menai_complex_value(a);
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = csin(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_COS: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-cos")) goto error;
            PyObject *cv = menai_complex_value(a);
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = ccos(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_TAN: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-tan")) goto error;
            PyObject *cv = menai_complex_value(a);
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = ctan(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SQRT: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-sqrt")) goto error;
            PyObject *cv = menai_complex_value(a);
            double complex z = PyComplex_RealAsDouble(cv) + PyComplex_ImagAsDouble(cv) * I;
            double complex cr = csqrt(z);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOGN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex-logn")) goto error;
            if (!require_complex(b, "complex-logn")) goto error;
            PyObject *av = menai_complex_value(a);
            PyObject *bv = menai_complex_value(b);
            double complex za = PyComplex_RealAsDouble(av) + PyComplex_ImagAsDouble(av) * I;
            double complex zb = PyComplex_RealAsDouble(bv) + PyComplex_ImagAsDouble(bv) * I;
            if (zb == 0.0) {
                menai_raise_eval_error("Function 'complex-logn' requires a non-zero base");
                goto error;
            }
            double complex cr = clog(za) / clog(zb);
            PyObject *_r = make_complex_from_doubles(creal(cr), cimag(cr));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
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
            reg_set_own(regs, base + dest, r);
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
            Py_ssize_t len = PyUnicode_GET_LENGTH(sv);
            PyObject *r = PyLong_FromSsize_t(len);
            if (r == NULL) goto error;
            PyObject *_r = make_integer_value(r);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_STRING_UPCASE: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-upcase")) goto error;
            PyObject *sv = menai_string_value(a);
            PyObject *up = PyObject_CallMethodNoArgs(sv, _str_upper);
            if (up == NULL) goto error;
            PyObject *r = make_string_from_pyobj(up);
            Py_DECREF(up);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_DOWNCASE: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-downcase")) goto error;
            PyObject *sv = menai_string_value(a);
            PyObject *lo = PyObject_CallMethodNoArgs(sv, _str_lower);
            if (lo == NULL) goto error;
            PyObject *r = make_string_from_pyobj(lo);
            Py_DECREF(lo);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_TRIM: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-trim")) goto error;
            PyObject *sv = menai_string_value(a);
            PyObject *t = PyObject_CallMethodNoArgs(sv, _str_strip);
            if (t == NULL) goto error;
            PyObject *r = make_string_from_pyobj(t);
            Py_DECREF(t);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_TRIM_LEFT: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-trim-left")) goto error;
            PyObject *sv = menai_string_value(a);
            PyObject *t = PyObject_CallMethodNoArgs(sv, _str_lstrip);
            if (t == NULL) goto error;
            PyObject *r = make_string_from_pyobj(t);
            Py_DECREF(t);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_TRIM_RIGHT: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-trim-right")) goto error;
            PyObject *sv = menai_string_value(a);
            PyObject *t = PyObject_CallMethodNoArgs(sv, _str_rstrip);
            if (t == NULL) goto error;
            PyObject *r = make_string_from_pyobj(t);
            Py_DECREF(t);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_CONCAT: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-concat")) goto error;
            if (!require_string(b, "string-concat")) goto error;
            PyObject *sa = menai_string_value(a);
            PyObject *sb = menai_string_value(b);
            PyObject *cat = PyUnicode_Concat(sa, sb);
            if (cat == NULL) goto error;
            PyObject *r = make_string_from_pyobj(cat);
            Py_DECREF(cat);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_PREFIX_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-prefix?")) goto error;
            if (!require_string(b, "string-prefix?")) goto error;
            PyObject *sa = menai_string_value(a);
            PyObject *sb = menai_string_value(b);
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

            PyObject *sb = menai_string_value(b);

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
            PyObject *iv = menai_integer_value(b);
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
            reg_set_own(regs, base + dest, r);
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

            PyObject *bv = menai_integer_value(b);

            PyObject *cv = menai_integer_value(c);

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
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_REPLACE: {
            PyObject *a = regs[base + src0], *b = regs[base + src1], *c = regs[base + src2];
            if (!require_string(a, "string-replace")) goto error;
            if (!require_string(b, "string-replace")) goto error;
            if (!require_string(c, "string-replace")) goto error;
            PyObject *sa = menai_string_value(a);
            PyObject *sb = menai_string_value(b);
            PyObject *sc = menai_string_value(c);
            PyObject *replaced = PyObject_CallMethodObjArgs(sa, _str_replace, sb, sc, NULL);
            if (replaced == NULL) goto error;
            PyObject *r = make_string_from_pyobj(replaced);
            Py_DECREF(replaced);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_INDEX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-index")) goto error;
            if (!require_string(b, "string-index")) goto error;
            PyObject *sa = menai_string_value(a);
            PyObject *sb = menai_string_value(b);
            Py_ssize_t idx = PyUnicode_Find(sa, sb, 0, PY_SSIZE_T_MAX, 1);
            if (idx == -2) goto error; /* error */
            if (idx == -1) {
                reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                PyObject *iv = PyLong_FromSsize_t(idx);
                if (iv == NULL) goto error;
                PyObject *_r = make_integer_value(iv);
                if (_r == NULL) goto error;
                reg_set_own(regs, base + dest, _r);
            }
            break;
        }

        case OP_STRING_TO_INTEGER_CODEPOINT: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string->integer-codepoint")) goto error;
            PyObject *sa = menai_string_value(a);
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
            reg_set_own(regs, base + dest, _r);
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
            long radix = PyLong_AsLong(bv);
            if (radix == -1 && PyErr_Occurred()) goto error;
            if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
                menai_raise_eval_errorf("string->integer radix must be 2, 8, 10, or 16, got %ld", radix);
                goto error;
            }
            PyObject *sa = menai_string_value(a);
            PyObject *stripped = PyObject_CallMethodNoArgs(sa, _str_strip);
            if (stripped == NULL) goto error;
            PyObject *ri = PyLong_FromUnicodeObject(stripped, (int)radix);
            Py_DECREF(stripped);
            if (ri == NULL) {
                PyErr_Clear();
                reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                PyObject *_r = make_integer_value(ri);
                if (_r == NULL) goto error;
                reg_set_own(regs, base + dest, _r);
            }
            break;
        }

        case OP_STRING_TO_NUMBER: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string->number")) goto error;
            /* Delegate to the Menai object's method via Python call */
            PyObject *sa = menai_string_value(a);
            /* Try int, then float, then complex — matching Python VM logic */
            PyObject *result = NULL;
            /* Check for 'j'/'J' → complex */
            PyObject *lower = PyObject_CallMethodNoArgs(sa, _str_lower);
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
                    PyObject *r = make_integer_value(result);
                    if (r == NULL) goto error;
                    reg_set_own(regs, base + dest, r);
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
                    reg_set_own(regs, base + dest, r);
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
                reg_set_own(regs, base + dest, _r);
            } else {
                PyErr_Clear();
                reg_set_borrow(regs, base + dest, Menai_NONE);
            }
            break;
        }

        case OP_STRING_TO_LIST: {
            /* src0=string, src1=delimiter string */
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string->list")) goto error;
            if (!require_string(b, "string->list")) goto error;
            PyObject *sa = menai_string_value(a);
            PyObject *sb = menai_string_value(b);
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
                parts = PyObject_CallMethodOneArg(sa, _str_split, sb);
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
            Py_ssize_t stl_n = PyList_GET_SIZE(parts);
            PyObject **stl2_arr = stl_n > 0
                ? (PyObject **)PyMem_Malloc(stl_n * sizeof(PyObject *)) : NULL;
            if (stl_n > 0 && !stl2_arr) { Py_DECREF(parts); PyErr_NoMemory(); goto error; }
            for (Py_ssize_t i = 0; i < stl_n; i++) {
                stl2_arr[i] = PyList_GET_ITEM(parts, i);
                Py_INCREF(stl2_arr[i]);
            }
            Py_DECREF(parts);
            PyObject *r = menai_list_from_array_steal(stl2_arr, stl_n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_P:
            bool_store(regs, base + dest, IS_MENAI_LIST(regs[base + src0]));
            break;

        case OP_LIST_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_list(a, "list=?")) goto error;
            if (!require_list(b, "list=?")) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(a, b, Py_EQ));
            break;
        }

        case OP_LIST_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_list(a, "list!=?")) goto error;
            if (!require_list(b, "list!=?")) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(a, b, Py_NE));
            break;
        }

        case OP_LIST_NULL_P: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-null?")) goto error;
            int is_null = (((MenaiList_Object *)a)->length == 0);
            bool_store(regs, base + dest, is_null);
            break;
        }

        case OP_LIST_LENGTH: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-length")) goto error;
            Py_ssize_t n = ((MenaiList_Object *)a)->length;
            PyObject *iv = PyLong_FromSsize_t(n);
            if (iv == NULL) goto error;
            PyObject *_r = make_integer_value(iv);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_LIST_FIRST: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-first")) goto error;
            MenaiList_Object *lst_f = (MenaiList_Object *)a;
            if (lst_f->length == 0) {
                menai_raise_eval_error("Function 'list-first' requires a non-empty list");
                goto error;
            }
            reg_set_borrow(regs, base + dest, lst_f->elements[0]);
            break;
        }

        case OP_LIST_REST: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-rest")) goto error;
            MenaiList_Object *lst_r = (MenaiList_Object *)a;
            if (lst_r->length == 0) {
                menai_raise_eval_error("Function 'list-rest' requires a non-empty list");
                goto error;
            }
            Py_ssize_t rest_n = lst_r->length - 1;
            PyObject **rest_arr = rest_n > 0
                ? (PyObject **)PyMem_Malloc(rest_n * sizeof(PyObject *)) : NULL;
            if (rest_n > 0 && !rest_arr) { PyErr_NoMemory(); goto error; }
            for (Py_ssize_t i = 0; i < rest_n; i++) {
                rest_arr[i] = lst_r->elements[i + 1];
                Py_INCREF(rest_arr[i]);
            }
            PyObject *r = menai_list_from_array_steal(rest_arr, rest_n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_LAST: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-last")) goto error;
            MenaiList_Object *lst_l = (MenaiList_Object *)a;
            Py_ssize_t n = lst_l->length;
            if (n == 0) {
                menai_raise_eval_error("Function 'list-last' requires a non-empty list");
                goto error;
            }
            reg_set_borrow(regs, base + dest, lst_l->elements[n - 1]);
            break;
        }

        case OP_LIST_REF: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_list(a, "list-ref")) goto error;
            if (!IS_MENAI_INTEGER(b)) {
                menai_raise_eval_error("list-ref: index must be integer");
                goto error;
            }
            MenaiList_Object *lst_ref = (MenaiList_Object *)a;
            PyObject *bv = menai_integer_value(b);
            Py_ssize_t idx = PyLong_AsSsize_t(bv);
            Py_ssize_t n = lst_ref->length;
            if (idx < 0 || idx >= n) {
                menai_raise_eval_errorf("list-ref: index out of range: %zd", idx);
                goto error;
            }
            reg_set_borrow(regs, base + dest, lst_ref->elements[idx]);
            break;
        }

        case OP_LIST_PREPEND: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_list(a, "list-prepend")) goto error;
            MenaiList_Object *lst_pre = (MenaiList_Object *)a;
            Py_ssize_t n = lst_pre->length;
            PyObject **pre_arr = (PyObject **)PyMem_Malloc((n + 1) * sizeof(PyObject *));
            if (!pre_arr) { PyErr_NoMemory(); goto error; }
            pre_arr[0] = item;
            Py_INCREF(item);
            for (Py_ssize_t i = 0; i < n; i++) {
                pre_arr[i + 1] = lst_pre->elements[i];
                Py_INCREF(pre_arr[i + 1]);
            }
            PyObject *r = menai_list_from_array_steal(pre_arr, n + 1);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_APPEND: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_list(a, "list-append")) goto error;
            MenaiList_Object *lst_app = (MenaiList_Object *)a;
            Py_ssize_t n = lst_app->length;
            PyObject **app_arr = (PyObject **)PyMem_Malloc((n + 1) * sizeof(PyObject *));
            if (!app_arr) { PyErr_NoMemory(); goto error; }
            for (Py_ssize_t i = 0; i < n; i++) {
                app_arr[i] = lst_app->elements[i];
                Py_INCREF(app_arr[i]);
            }
            app_arr[n] = item;
            Py_INCREF(item);
            PyObject *r = menai_list_from_array_steal(app_arr, n + 1);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_REVERSE: {
            PyObject *a = regs[base + src0];
            if (!require_list(a, "list-reverse")) goto error;
            MenaiList_Object *lst_rev = (MenaiList_Object *)a;
            Py_ssize_t n = lst_rev->length;
            PyObject **rev_arr = n > 0
                ? (PyObject **)PyMem_Malloc(n * sizeof(PyObject *)) : NULL;
            if (n > 0 && !rev_arr) { PyErr_NoMemory(); goto error; }
            for (Py_ssize_t i = 0; i < n; i++) {
                rev_arr[i] = lst_rev->elements[n - 1 - i];
                Py_INCREF(rev_arr[i]);
            }
            PyObject *r = menai_list_from_array_steal(rev_arr, n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_CONCAT: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_list(a, "list-concat")) goto error;
            if (!require_list(b, "list-concat")) goto error;
            MenaiList_Object *lst_ca = (MenaiList_Object *)a;
            MenaiList_Object *lst_cb = (MenaiList_Object *)b;
            Py_ssize_t na = lst_ca->length, nb = lst_cb->length;
            Py_ssize_t nc = na + nb;
            PyObject **cat_arr = nc > 0
                ? (PyObject **)PyMem_Malloc(nc * sizeof(PyObject *)) : NULL;
            if (nc > 0 && !cat_arr) { PyErr_NoMemory(); goto error; }
            for (Py_ssize_t i = 0; i < na; i++) {
                cat_arr[i] = lst_ca->elements[i];
                Py_INCREF(cat_arr[i]);
            }
            for (Py_ssize_t i = 0; i < nb; i++) {
                cat_arr[na + i] = lst_cb->elements[i];
                Py_INCREF(cat_arr[na + i]);
            }
            PyObject *r = menai_list_from_array_steal(cat_arr, nc);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_MEMBER_P: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_list(a, "list-member?")) goto error;
            MenaiList_Object *lst_mem = (MenaiList_Object *)a;
            int mem_found = 0;
            for (Py_ssize_t i = 0; i < lst_mem->length; i++) {
                int eq = PyObject_RichCompareBool(lst_mem->elements[i], item, Py_EQ);
                if (eq < 0) goto error;
                if (eq) { mem_found = 1; break; }
            }
            bool_store(regs, base + dest, mem_found);
            break;
        }

        case OP_LIST_INDEX: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_list(a, "list-index")) goto error;
            MenaiList_Object *lst_idx = (MenaiList_Object *)a;
            Py_ssize_t n = lst_idx->length;
            Py_ssize_t found = -1;
            for (Py_ssize_t i = 0; i < n; i++) {
                int eq = PyObject_RichCompareBool(lst_idx->elements[i], item, Py_EQ);
                if (eq < 0) goto error;
                if (eq) {
                    found = i;
                    break;
                }
            }
            if (found == -1) {
                reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                PyObject *iv = PyLong_FromSsize_t(found);
                if (iv == NULL) goto error;
                PyObject *_r = make_integer_value(iv);
                if (_r == NULL) goto error;
                reg_set_own(regs, base + dest, _r);
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
            MenaiList_Object *lst_sl = (MenaiList_Object *)a;
            PyObject *bv = menai_integer_value(b);
            PyObject *cv = menai_integer_value(c);
            Py_ssize_t start = PyLong_AsSsize_t(bv), end = PyLong_AsSsize_t(cv);
            Py_ssize_t n = lst_sl->length;
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
            Py_ssize_t sl_n = end - start;
            PyObject **sl_arr = sl_n > 0
                ? (PyObject **)PyMem_Malloc(sl_n * sizeof(PyObject *)) : NULL;
            if (sl_n > 0 && !sl_arr) { PyErr_NoMemory(); goto error; }
            for (Py_ssize_t i = 0; i < sl_n; i++) {
                sl_arr[i] = lst_sl->elements[start + i];
                Py_INCREF(sl_arr[i]);
            }
            PyObject *r = menai_list_from_array_steal(sl_arr, sl_n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_REMOVE: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_list(a, "list-remove")) goto error;
            MenaiList_Object *lst_rm = (MenaiList_Object *)a;
            Py_ssize_t n = lst_rm->length;
            /* Count non-matching elements first */
            Py_ssize_t keep = 0;
            for (Py_ssize_t i = 0; i < n; i++) {
                int eq = PyObject_RichCompareBool(lst_rm->elements[i], item, Py_EQ);
                if (eq < 0) goto error;
                if (!eq) keep++;
            }
            PyObject **rm_arr = keep > 0
                ? (PyObject **)PyMem_Malloc(keep * sizeof(PyObject *)) : NULL;
            if (keep > 0 && !rm_arr) { PyErr_NoMemory(); goto error; }
            Py_ssize_t j = 0;
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *e = lst_rm->elements[i];
                int eq = PyObject_RichCompareBool(e, item, Py_EQ);
                if (eq < 0) {
                    for (Py_ssize_t k = 0; k < j; k++) Py_DECREF(rm_arr[k]);
                    PyMem_Free(rm_arr);
                    goto error;
                }
                if (!eq) {
                    Py_INCREF(e);
                    rm_arr[j++] = e;
                }
            }
            PyObject *r = menai_list_from_array_steal(rm_arr, keep);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_TO_STRING: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_list(a, "list->string")) goto error;
            if (!require_string(b, "list->string")) goto error;
            MenaiList_Object *lst_ts = (MenaiList_Object *)a;
            PyObject *sep = menai_string_value(b);
            Py_ssize_t n = lst_ts->length;
            PyObject *parts = PyList_New(n);
            if (parts == NULL) {
                goto error;
            }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *elem = lst_ts->elements[i];
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
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_TO_SET: {
            PyObject *a = regs[base + src0];
            if (!require_list_singular(a, "list->set")) goto error;
            MenaiList_Object *lst_lts = (MenaiList_Object *)a;
            PyObject *tmp_tup = PyTuple_New(lst_lts->length);
            if (!tmp_tup) goto error;
            for (Py_ssize_t i = 0; i < lst_lts->length; i++) {
                Py_INCREF(lst_lts->elements[i]);
                PyTuple_SET_ITEM(tmp_tup, i, lst_lts->elements[i]);
            }
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_SetType, tmp_tup);
            Py_DECREF(tmp_tup);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_P:
            bool_store(regs, base + dest, IS_MENAI_DICT(regs[base + src0]));
            break;

        case OP_DICT_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_dict(a, "dict=?")) goto error;
            if (!require_dict(b, "dict=?")) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(a, b, Py_EQ));
            break;
        }

        case OP_DICT_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_dict(a, "dict!=?")) goto error;
            if (!require_dict(b, "dict!=?")) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(a, b, Py_NE));
            break;
        }

        case OP_DICT_LENGTH: {
            PyObject *a = regs[base + src0];
            if (!require_dict(a, "dict-length")) goto error;
            PyObject *pairs = ((MenaiDict_Object *)a)->pairs;
            Py_ssize_t n = PyTuple_GET_SIZE(pairs);
            PyObject *iv = PyLong_FromSsize_t(n);
            if (iv == NULL) goto error;
            PyObject *_r = make_integer_value(iv);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
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
                PyObject *pairs = ((MenaiDict_Object *)a)->pairs;
                Py_ssize_t n = PyTuple_GET_SIZE(pairs);
                PyObject **dk_arr = n > 0
                    ? (PyObject **)PyMem_Malloc(n * sizeof(PyObject *)) : NULL;
                if (n > 0 && !dk_arr) { PyErr_NoMemory(); goto error; }
                for (Py_ssize_t i = 0; i < n; i++) {
                    PyObject *pair = PyTuple_GET_ITEM(pairs, i);
                    PyObject *k = PyTuple_GET_ITEM(pair, 0);
                    Py_INCREF(k);
                    dk_arr[i] = k;
                }
                r = menai_list_from_array_steal(dk_arr, n);
            }
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_VALUES: {
            PyObject *a = regs[base + src0];
            if (!require_dict(a, "dict-values")) goto error;
            PyObject *pairs = ((MenaiDict_Object *)a)->pairs;
            Py_ssize_t n = PyTuple_GET_SIZE(pairs);
            PyObject **dv_arr = n > 0
                ? (PyObject **)PyMem_Malloc(n * sizeof(PyObject *)) : NULL;
            if (n > 0 && !dv_arr) { PyErr_NoMemory(); goto error; }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *pair = PyTuple_GET_ITEM(pairs, i);
                PyObject *v = PyTuple_GET_ITEM(pair, 1);
                Py_INCREF(v);
                dv_arr[i] = v;
            }
            PyObject *r = menai_list_from_array_steal(dv_arr, n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_HAS_P: {
            PyObject *a = regs[base + src0], *key = regs[base + src1];
            if (!require_dict(a, "dict-has?")) goto error;
            PyObject *r = menai_hashable_key(key);
            if (r == NULL) goto error;
            PyObject *lookup = ((MenaiDict_Object *)a)->lookup;
            int has = PyDict_Contains(lookup, r);
            Py_DECREF(r);
            if (has < 0) goto error;
            bool_store(regs, base + dest, has);
            break;
        }

        case OP_DICT_GET: {
            /* src0=dict, src1=key, src2=default */
            PyObject *a = regs[base + src0], *key = regs[base + src1], *def = regs[base + src2];
            if (!require_dict(a, "dict-get")) goto error;
            PyObject *hk = menai_hashable_key(key);
            if (hk == NULL) goto error;
            PyObject *lookup = ((MenaiDict_Object *)a)->lookup;
            PyObject *entry = PyDict_GetItem(lookup, hk);
            Py_DECREF(hk);
            if (entry != NULL) {
                /* entry is (key, value) tuple */
                PyObject *val = PyTuple_GET_ITEM(entry, 1);
                reg_set_borrow(regs, base + dest, val);
            } else {
                reg_set_borrow(regs, base + dest, def);
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
                PyObject *pairs = ((MenaiDict_Object *)a)->pairs;
                PyObject *hk = menai_hashable_key(key);
                if (hk == NULL) {
                    goto error;
                }
                Py_ssize_t n = PyTuple_GET_SIZE(pairs);
                /* Find if key exists */
                int found = 0;
                PyObject *new_pairs = PyList_New(0);
                if (new_pairs == NULL) {
                    Py_DECREF(hk);
                    goto error;
                }
                for (Py_ssize_t i = 0; i < n; i++) {
                    PyObject *pair = PyTuple_GET_ITEM(pairs, i);
                    PyObject *k = PyTuple_GET_ITEM(pair, 0);
                    PyObject *khk = menai_hashable_key(k);
                    if (khk == NULL) {
                        Py_DECREF(new_pairs);
                        Py_DECREF(hk);
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
                        goto error;
                    }
                    if (!eq) Py_INCREF(new_pair);
                    if (PyList_Append(new_pairs, new_pair) < 0) {
                        Py_DECREF(new_pair);
                        Py_DECREF(new_pairs);
                        Py_DECREF(hk);
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
                        goto error;
                    }
                    Py_DECREF(new_pair);
                }
                Py_DECREF(hk);
                PyObject *new_tup = PyList_AsTuple(new_pairs);
                Py_DECREF(new_pairs);
                if (new_tup == NULL) goto error;
                result = PyObject_CallOneArg((PyObject *)Menai_DictType, new_tup);
                Py_DECREF(new_tup);
            }
            if (result == NULL) goto error;
            reg_set_own(regs, base + dest, result);
            break;
        }

        case OP_DICT_REMOVE: {
            PyObject *a = regs[base + src0], *key = regs[base + src1];
            if (!require_dict(a, "dict-remove")) goto error;
            PyObject *hk = menai_hashable_key(key);
            if (hk == NULL) goto error;
            PyObject *pairs = ((MenaiDict_Object *)a)->pairs;
            Py_ssize_t n = PyTuple_GET_SIZE(pairs);
            PyObject *new_list = PyList_New(0);
            if (new_list == NULL) {
                Py_DECREF(hk);
                goto error;
            }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *pair = PyTuple_GET_ITEM(pairs, i);
                PyObject *k = PyTuple_GET_ITEM(pair, 0);
                PyObject *khk = menai_hashable_key(k);
                if (khk == NULL) {
                    Py_DECREF(new_list);
                    Py_DECREF(hk);
                    goto error;
                }
                int eq = PyObject_RichCompareBool(khk, hk, Py_EQ);
                Py_DECREF(khk);
                if (eq < 0) {
                    Py_DECREF(new_list);
                    Py_DECREF(hk);
                    goto error;
                }
                if (!eq) {
                    Py_INCREF(pair);
                    if (PyList_Append(new_list, pair) < 0) {
                        Py_DECREF(pair);
                        Py_DECREF(new_list);
                        Py_DECREF(hk);
                        goto error;
                    }
                    Py_DECREF(pair);
                }
            }
            Py_DECREF(hk);
            PyObject *new_tup = PyList_AsTuple(new_list);
            Py_DECREF(new_list);
            if (new_tup == NULL) goto error;
            PyObject *r = PyObject_CallOneArg((PyObject *)Menai_DictType, new_tup);
            Py_DECREF(new_tup);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
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
                PyObject *pa = ((MenaiDict_Object *)a)->pairs;
                PyObject *pb = ((MenaiDict_Object *)b)->pairs;
                /* a's pairs first, then b's new pairs */
                PyObject *merged = PyList_New(0);
                if (merged == NULL) {
                    goto error;
                }
                PyObject *seen = PyDict_New();
                if (seen == NULL) {
                    Py_DECREF(merged);
                    goto error;
                }
                /* Add a's pairs (with b's values if key in b) */
                PyObject *b_lookup = ((MenaiDict_Object *)b)->lookup;
                Py_ssize_t na = PyTuple_GET_SIZE(pa);
                for (Py_ssize_t i = 0; i < na; i++) {
                    PyObject *pair = PyTuple_GET_ITEM(pa, i);
                    PyObject *k = PyTuple_GET_ITEM(pair, 0);
                    PyObject *hk = menai_hashable_key(k);
                    if (hk == NULL) {
                        Py_DECREF(b_lookup);
                        Py_DECREF(seen);
                        Py_DECREF(merged);
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
                    PyObject *hk = menai_hashable_key(k);
                    if (hk == NULL) {
                        Py_DECREF(b_lookup);
                        Py_DECREF(seen);
                        Py_DECREF(merged);
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
                            goto error;
                        }
                        Py_DECREF(pair);
                    }
                    Py_DECREF(hk);
                }
                Py_DECREF(b_lookup);
                Py_DECREF(seen);
                PyObject *new_tup = PyList_AsTuple(merged);
                Py_DECREF(merged);
                if (new_tup == NULL) goto error;
                r = PyObject_CallOneArg((PyObject *)Menai_DictType, new_tup);
                Py_DECREF(new_tup);
            }
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_P:
            bool_store(regs, base + dest, IS_MENAI_SET(regs[base + src0]));
            break;

        case OP_SET_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set=?")) goto error;
            if (!require_set(b, "set=?")) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(a, b, Py_EQ));
            break;
        }

        case OP_SET_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set!=?")) goto error;
            if (!require_set(b, "set!=?")) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(a, b, Py_NE));
            break;
        }

        case OP_SET_LENGTH: {
            PyObject *a = regs[base + src0];
            if (!require_set_singular(a, "set-length")) goto error;
            PyObject *elems = ((MenaiSet_Object *)a)->elements;
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            PyObject *iv = PyLong_FromSsize_t(n);
            if (iv == NULL) goto error;
            PyObject *_r = make_integer_value(iv);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_SET_MEMBER_P: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_set_singular(a, "set-member?")) goto error;
            PyObject *hk = menai_hashable_key(item);
            if (hk == NULL) goto error;
            PyObject *members = ((MenaiSet_Object *)a)->members;
            int has = PySequence_Contains(members, hk);
            Py_DECREF(hk);
            if (has < 0) goto error;
            bool_store(regs, base + dest, has);
            break;
        }

        case OP_SET_ADD: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_set_singular(a, "set-add")) goto error;
            PyObject *hk = menai_hashable_key(item);
            if (hk == NULL) goto error;
            PyObject *members = ((MenaiSet_Object *)a)->members;
            int has = PySequence_Contains(members, hk);
            Py_DECREF(hk);
            if (has < 0) goto error;
            if (has) {
                reg_set_borrow(regs, base + dest, a);
            } else {
                PyObject *elems = ((MenaiSet_Object *)a)->elements;
                Py_ssize_t n = PyTuple_GET_SIZE(elems);
                PyObject *new_tup = PyTuple_New(n + 1);
                if (new_tup == NULL) {
                    goto error;
                }
                for (Py_ssize_t i = 0; i < n; i++) {
                    PyObject *e = PyTuple_GET_ITEM(elems, i);
                    Py_INCREF(e);
                    PyTuple_SET_ITEM(new_tup, i, e);
                }
                Py_INCREF(item);
                PyTuple_SET_ITEM(new_tup, n, item);
                PyObject *r = menai_set_from_elements(new_tup);
                if (r == NULL) goto error;
                reg_set_own(regs, base + dest, r);
            }
            break;
        }

        case OP_SET_REMOVE: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_set_singular(a, "set-remove")) goto error;
            PyObject *hk = menai_hashable_key(item);
            if (hk == NULL) goto error;
            PyObject *elems = ((MenaiSet_Object *)a)->elements;
            Py_ssize_t n = PyTuple_GET_SIZE(elems);
            PyObject *new_list = PyList_New(0);
            if (new_list == NULL) {
                Py_DECREF(hk);
                goto error;
            }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *e = PyTuple_GET_ITEM(elems, i);
                PyObject *ehk = menai_hashable_key(e);
                if (ehk == NULL) {
                    Py_DECREF(new_list);
                    Py_DECREF(hk);
                    goto error;
                }
                int eq = PyObject_RichCompareBool(ehk, hk, Py_EQ);
                Py_DECREF(ehk);
                if (eq < 0) {
                    Py_DECREF(new_list);
                    Py_DECREF(hk);
                    goto error;
                }
                if (!eq) {
                    Py_INCREF(e);
                    if (PyList_Append(new_list, e) < 0) {
                        Py_DECREF(e);
                        Py_DECREF(new_list);
                        Py_DECREF(hk);
                        goto error;
                    }
                    Py_DECREF(e);
                }
            }
            Py_DECREF(hk);
            PyObject *new_tup = PyList_AsTuple(new_list);
            Py_DECREF(new_list);
            if (new_tup == NULL) goto error;
            PyObject *r = menai_set_from_elements(new_tup);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_UNION: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-union")) goto error;
            if (!require_set(b, "set-union")) goto error;
            PyObject *ea = ((MenaiSet_Object *)a)->elements;
            PyObject *eb = ((MenaiSet_Object *)b)->elements;
            Py_ssize_t na = PyTuple_GET_SIZE(ea), nb = PyTuple_GET_SIZE(eb);
            PyObject *new_list = PyList_New(0);
            if (new_list == NULL) {
                goto error;
            }
            PyObject *seen = PyDict_New();
            if (seen == NULL) {
                Py_DECREF(new_list);
                goto error;
            }
            /* Add all of a's elements */
            for (Py_ssize_t i = 0; i < na; i++) {
                PyObject *e = PyTuple_GET_ITEM(ea, i);
                PyObject *hk = menai_hashable_key(e);
                if (hk == NULL) {
                    Py_DECREF(seen);
                    Py_DECREF(new_list);
                    goto error;
                }
                Py_INCREF(e);
                if (PyList_Append(new_list, e) < 0 || PyDict_SetItem(seen, hk, Py_True) < 0) {
                    Py_DECREF(e);
                    Py_DECREF(hk);
                    Py_DECREF(seen);
                    Py_DECREF(new_list);
                    goto error;
                }
                Py_DECREF(e);
                Py_DECREF(hk);
            }
            /* Add b's elements not in a */
            for (Py_ssize_t i = 0; i < nb; i++) {
                PyObject *e = PyTuple_GET_ITEM(eb, i);
                PyObject *hk = menai_hashable_key(e);
                if (hk == NULL) {
                    Py_DECREF(seen);
                    Py_DECREF(new_list);
                    goto error;
                }
                if (!PyDict_Contains(seen, hk)) {
                    Py_INCREF(e);
                    if (PyList_Append(new_list, e) < 0) {
                        Py_DECREF(e);
                        Py_DECREF(hk);
                        Py_DECREF(seen);
                        Py_DECREF(new_list);
                        goto error;
                    }
                    Py_DECREF(e);
                }
                Py_DECREF(hk);
            }
            Py_DECREF(seen);
            {
                PyObject *new_tup = PyList_AsTuple(new_list);
                Py_DECREF(new_list);
                if (new_tup == NULL) goto error;
                PyObject *r = menai_set_from_elements(new_tup);
                if (r == NULL) goto error;
                reg_set_own(regs, base + dest, r);
            }
            break;
        }

        case OP_SET_INTERSECTION: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-intersection")) goto error;
            if (!require_set(b, "set-intersection")) goto error;
            PyObject *ea = ((MenaiSet_Object *)a)->elements;
            PyObject *mb = ((MenaiSet_Object *)b)->members;
            Py_ssize_t na = PyTuple_GET_SIZE(ea);
            PyObject *new_list = PyList_New(0);
            if (new_list == NULL) {
                goto error;
            }
            for (Py_ssize_t i = 0; i < na; i++) {
                PyObject *e = PyTuple_GET_ITEM(ea, i);
                PyObject *hk = menai_hashable_key(e);
                if (hk == NULL) {
                    Py_DECREF(new_list);
                    goto error;
                }
                int in_b = PySequence_Contains(mb, hk);
                Py_DECREF(hk);
                if (in_b < 0) {
                    Py_DECREF(new_list);
                    goto error;
                }
                if (in_b) {
                    Py_INCREF(e);
                    if (PyList_Append(new_list, e) < 0) {
                        Py_DECREF(e);
                        Py_DECREF(new_list);
                        goto error;
                    }
                    Py_DECREF(e);
                }
            }
            PyObject *new_tup = PyList_AsTuple(new_list);
            Py_DECREF(new_list);
            if (new_tup == NULL) goto error;
            PyObject *r = menai_set_from_elements(new_tup);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_DIFFERENCE: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-difference")) goto error;
            if (!require_set(b, "set-difference")) goto error;
            PyObject *ea = ((MenaiSet_Object *)a)->elements;
            PyObject *mb = ((MenaiSet_Object *)b)->members;
            Py_ssize_t na = PyTuple_GET_SIZE(ea);
            PyObject *new_list = PyList_New(0);
            if (new_list == NULL) {
                goto error;
            }
            for (Py_ssize_t i = 0; i < na; i++) {
                PyObject *e = PyTuple_GET_ITEM(ea, i);
                PyObject *hk = menai_hashable_key(e);
                if (hk == NULL) {
                    Py_DECREF(new_list);
                    goto error;
                }
                int in_b = PySequence_Contains(mb, hk);
                Py_DECREF(hk);
                if (in_b < 0) {
                    Py_DECREF(new_list);
                    goto error;
                }
                if (!in_b) {
                    Py_INCREF(e);
                    if (PyList_Append(new_list, e) < 0) {
                        Py_DECREF(e);
                        Py_DECREF(new_list);
                        goto error;
                    }
                    Py_DECREF(e);
                }
            }
            PyObject *new_tup = PyList_AsTuple(new_list);
            Py_DECREF(new_list);
            if (new_tup == NULL) goto error;
            PyObject *r = menai_set_from_elements(new_tup);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_SUBSET_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-subset?")) goto error;
            if (!require_set(b, "set-subset?")) goto error;
            PyObject *ma = ((MenaiSet_Object *)a)->members;
            PyObject *mb = ((MenaiSet_Object *)b)->members;
            /* frozenset.issubset: use PyObject_CallMethod */
            bool_store(regs, base + dest, PyObject_RichCompareBool(ma, mb, Py_LE));
            break;
        }

        case OP_SET_TO_LIST: {
            PyObject *a = regs[base + src0];
            if (!require_set_singular(a, "set->list")) goto error;
            PyObject *set_elems = ((MenaiSet_Object *)a)->elements;
            Py_ssize_t set_n = PyTuple_GET_SIZE(set_elems);
            PyObject **stl_arr = set_n > 0
                ? (PyObject **)PyMem_Malloc(set_n * sizeof(PyObject *)) : NULL;
            if (set_n > 0 && !stl_arr) { PyErr_NoMemory(); goto error; }
            for (Py_ssize_t i = 0; i < set_n; i++) {
                stl_arr[i] = PyTuple_GET_ITEM(set_elems, i);
                Py_INCREF(stl_arr[i]);
            }
            PyObject *r = menai_list_from_array_steal(stl_arr, set_n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
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
            PyObject *bv = menai_integer_value(rb);
            PyObject *cv = menai_integer_value(rc);
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
            PyObject **rng_arr = n > 0
                ? (PyObject **)PyMem_Malloc(n * sizeof(PyObject *)) : NULL;
            if (n > 0 && !rng_arr) { PyErr_NoMemory(); goto error; }
            long val = start;
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *iv = PyLong_FromLong(val);
                if (iv == NULL) {
                    for (Py_ssize_t k = 0; k < i; k++) Py_DECREF(rng_arr[k]);
                    PyMem_Free(rng_arr);
                    goto error;
                }
                PyObject *mi = make_integer_value(iv);
                if (mi == NULL) {
                    for (Py_ssize_t k = 0; k < i; k++) Py_DECREF(rng_arr[k]);
                    PyMem_Free(rng_arr);
                    goto error;
                }
                rng_arr[i] = mi;
                val += step;
            }
            PyObject *r = menai_list_from_array_steal(rng_arr, n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
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
            PyObject *instance = menai_struct_alloc(struct_type, fields);
            if (instance == NULL) goto error;
            reg_set_own(regs, base + dest, instance);
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
            int tag_a = ((MenaiStructType_Object *)stype)->tag;
            int tag_b = ((MenaiStructType_Object *)((MenaiStruct_Object *)val)->struct_type)->tag;
            bool_store(regs, base + dest, tag_a == tag_b);
            break;
        }

        case OP_STRUCT_GET: {
            /* src1 holds a MenaiSymbol field name */
            PyObject *val = regs[base + src0], *field_sym = regs[base + src1];
            if (!require_struct(val, "struct-get")) goto error;
            if (!require_symbol(field_sym, "struct-get")) goto error;
            PyObject *stype = ((MenaiStruct_Object *)val)->struct_type;
            PyObject *name = menai_symbol_name(field_sym);
            PyObject *idx = PyDict_GetItem(((MenaiStructType_Object *)stype)->_field_index, name);
            if (idx == NULL) {
                menai_raise_eval_errorf(
                    "'struct-get': struct '%s' has no field '%s'",
                    PyUnicode_AsUTF8(((MenaiStructType_Object *)stype)->name),
                    PyUnicode_AsUTF8(name));
                goto error;
            }
            Py_ssize_t fi = PyLong_AsSsize_t(idx);  /* idx borrowed from dict */
            if (fi == -1 && PyErr_Occurred()) goto error;
            PyObject *fields = ((MenaiStruct_Object *)val)->fields;
            PyObject *fv = PyTuple_GET_ITEM(fields, fi);
            reg_set_borrow(regs, base + dest, fv);
            break;
        }

        case OP_STRUCT_GET_IMM: {
            /* src1 holds a MenaiInteger field index */
            PyObject *val = regs[base + src0], *fidx = regs[base + src1];
            if (!require_struct(val, "struct-get-imm")) goto error;
            if (!require_integer(fidx, "struct-get-imm")) goto error;
            PyObject *iv = menai_integer_value(fidx);
            Py_ssize_t fi = PyLong_AsSsize_t(iv);
            if (fi == -1 && PyErr_Occurred()) goto error;
            PyObject *fields = ((MenaiStruct_Object *)val)->fields;
            PyObject *fv = PyTuple_GET_ITEM(fields, fi);
            reg_set_borrow(regs, base + dest, fv);
            break;
        }

        case OP_STRUCT_SET: {
            PyObject *val = regs[base + src0], *field_sym = regs[base + src1], *new_val = regs[base + src2];
            if (!require_struct(val, "struct-set")) goto error;
            if (!require_symbol(field_sym, "struct-set")) goto error;
            PyObject *stype = ((MenaiStruct_Object *)val)->struct_type;
            PyObject *name = menai_symbol_name(field_sym);
            PyObject *idx = PyDict_GetItem(((MenaiStructType_Object *)stype)->_field_index, name);
            if (idx == NULL) {
                menai_raise_eval_errorf(
                    "'struct-set': struct '%s' has no field '%s'",
                    PyUnicode_AsUTF8(((MenaiStructType_Object *)stype)->name),
                    PyUnicode_AsUTF8(name));
                goto error;
            }
            Py_ssize_t fi = PyLong_AsSsize_t(idx);  /* idx borrowed from dict */
            if (fi == -1 && PyErr_Occurred()) goto error;
            PyObject *fields = ((MenaiStruct_Object *)val)->fields;
            Py_ssize_t nf = PyTuple_GET_SIZE(fields);
            PyObject *new_fields = PyTuple_New(nf);
            if (new_fields == NULL) {
                goto error;
            }
            for (Py_ssize_t i = 0; i < nf; i++) {
                PyObject *fv = (i == fi) ? new_val : PyTuple_GET_ITEM(fields, i);
                Py_INCREF(fv);
                PyTuple_SET_ITEM(new_fields, i, fv);
            }
            PyObject *r = menai_struct_alloc(stype, new_fields);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRUCT_SET_IMM: {
            PyObject *val = regs[base + src0], *fidx = regs[base + src1], *new_val = regs[base + src2];
            if (!require_struct(val, "struct-set-imm")) goto error;
            if (!require_integer(fidx, "struct-set-imm")) goto error;
            PyObject *iv = menai_integer_value(fidx);
            Py_ssize_t fi = PyLong_AsSsize_t(iv);
            if (fi == -1 && PyErr_Occurred()) goto error;
            PyObject *stype = ((MenaiStruct_Object *)val)->struct_type;
            PyObject *fields = ((MenaiStruct_Object *)val)->fields;
            Py_ssize_t nf = PyTuple_GET_SIZE(fields);
            PyObject *new_fields = PyTuple_New(nf);
            if (new_fields == NULL) {
                goto error;
            }
            for (Py_ssize_t i = 0; i < nf; i++) {
                PyObject *fv = (i == fi) ? new_val : PyTuple_GET_ITEM(fields, i);
                Py_INCREF(fv);
                PyTuple_SET_ITEM(new_fields, i, fv);
            }
            PyObject *r = menai_struct_alloc(stype, new_fields);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRUCT_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_struct(a, "struct=?")) goto error;
            if (!require_struct(b, "struct=?")) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(a, b, Py_EQ));
            break;
        }

        case OP_STRUCT_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_struct(a, "struct!=?")) goto error;
            if (!require_struct(b, "struct!=?")) goto error;
            bool_store(regs, base + dest, PyObject_RichCompareBool(a, b, Py_NE));
            break;
        }

        case OP_STRUCT_TYPE: {
            PyObject *val = regs[base + src0];
            if (!require_struct(val, "struct-type")) goto error;
            PyObject *stype = ((MenaiStruct_Object *)val)->struct_type;
            reg_set_borrow(regs, base + dest, stype);
            break;
        }

        case OP_STRUCT_TYPE_NAME: {
            PyObject *val = regs[base + src0];
            if (!require_structtype(val, "struct-type-name")) goto error;
            PyObject *name = ((MenaiStructType_Object *)val)->name;
            PyObject *r = make_string_from_pyobj(name);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRUCT_FIELDS: {
            PyObject *val = regs[base + src0];
            if (!require_structtype(val, "struct-fields")) goto error;
            PyObject *field_names = ((MenaiStructType_Object *)val)->field_names;
            Py_ssize_t n = PyTuple_GET_SIZE(field_names);
            PyObject **sf_arr = n > 0
                ? (PyObject **)PyMem_Malloc(n * sizeof(PyObject *)) : NULL;
            if (n > 0 && !sf_arr) { PyErr_NoMemory(); goto error; }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *fname = PyTuple_GET_ITEM(field_names, i);
                /* Wrap in MenaiSymbol */
                PyObject *sym = PyObject_CallOneArg((PyObject *)Menai_SymbolType, fname);
                if (sym == NULL) {
                    for (Py_ssize_t k = 0; k < i; k++) Py_DECREF(sf_arr[k]);
                    PyMem_Free(sf_arr);
                    goto error;
                }
                sf_arr[i] = sym;
            }
            PyObject *r = menai_list_from_array_steal(sf_arr, n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
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
