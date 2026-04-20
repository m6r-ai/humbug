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
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/*
 * Portable complex arithmetic — avoids <complex.h>, which is unsupported on MSVC.
 *
 * All complex math is expressed in terms of <math.h> functions (exp, log, sin,
 * cos, tan, sqrt, atan2, hypot), which are available on every target platform.
 */
typedef struct {
    double re;
    double im;
} mc_t;

static inline mc_t mc(double re, double im) {
    mc_t z = {re, im};
    return z;
}

static inline int mc_zero(mc_t z) {
    return z.re == 0.0 && z.im == 0.0;
}

static inline mc_t mc_mul(mc_t a, mc_t b) {
    return mc(a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re);
}

static inline mc_t mc_div(mc_t a, mc_t b) {
    double d = b.re * b.re + b.im * b.im;
    return mc((a.re * b.re + a.im * b.im) / d, (a.im * b.re - a.re * b.im) / d);
}

static inline mc_t mc_exp(mc_t z) {
    double e = exp(z.re);
    return mc(e * cos(z.im), e * sin(z.im));
}

static inline mc_t mc_log(mc_t z) {
    return mc(log(hypot(z.re, z.im)), atan2(z.im, z.re));
}

static inline mc_t mc_pow(mc_t a, mc_t b) {
    return mc_zero(a) ? mc(0.0, 0.0) : mc_exp(mc_mul(b, mc_log(a)));
}

static inline mc_t mc_sqrt(mc_t z) {
    double r = hypot(z.re, z.im);
    double s = sqrt((r + z.re) / 2.0);
    double t = (z.im >= 0.0 ? 1.0 : -1.0) * sqrt((r - z.re) / 2.0);
    return mc(s, t);
}

static inline mc_t mc_sin(mc_t z) {
    return mc(sin(z.re) * cosh(z.im), cos(z.re) * sinh(z.im));
}

static inline mc_t mc_cos(mc_t z) {
    return mc(cos(z.re) * cosh(z.im), -sin(z.re) * sinh(z.im));
}

static inline mc_t mc_tan(mc_t z) {
    return mc_div(mc_sin(z), mc_cos(z));
}

static inline mc_t mc_log10(mc_t z) {
    mc_t l = mc_log(z);
    double s = 1.0 / log(10.0);
    return mc(l.re * s, l.im * s);
}

static inline mc_t mc_logn(mc_t a, mc_t b) {
    return mc_div(mc_log(a), mc_log(b)); 
}

#include "menai_vm_value.h"
#include "menai_vm_string.h"
#include "menai_vm_hashtable.h"
#include "menai_vm_integer.h"

/*
 * Portable overflow-detecting arithmetic for the small-integer fast paths.
 *
 * _menai_add_overflow(a, b, &result) returns 1 if a+b overflows long, 0 otherwise.
 * _menai_sub_overflow and _menai_mul_overflow follow the same convention.
 */
#if defined(__GNUC__) || defined(__clang__)
#define _menai_add_overflow(a, b, rp) __builtin_add_overflow((a), (b), (rp))
#define _menai_sub_overflow(a, b, rp) __builtin_sub_overflow((a), (b), (rp))
#define _menai_mul_overflow(a, b, rp) __builtin_mul_overflow((a), (b), (rp))
#else
static inline int _menai_add_overflow(long a, long b, long *r) {
    unsigned long ua = (unsigned long)a, ub = (unsigned long)b;
    unsigned long ur = ua + ub;
    *r = (long)ur;
    return (a > 0 && b > 0 && *r < 0) || (a < 0 && b < 0 && *r > 0);
}
static inline int _menai_sub_overflow(long a, long b, long *r) {
    unsigned long ua = (unsigned long)a, ub = (unsigned long)b;
    unsigned long ur = ua - ub;
    *r = (long)ur;
    return (b < 0 && a > 0 && *r < 0) || (b > 0 && a < 0 && *r > 0);
}
static inline int _menai_mul_overflow(long a, long b, long *r) {
    /* Conservative: use double to detect overflow. */
    double d = (double)a * (double)b;
    *r = (long)((unsigned long)a * (unsigned long)b);
    return d > (double)LONG_MAX || d < (double)LONG_MIN;
}
#endif

/* menai_vm_value init — lives in the same .so */
extern PyObject *_menai_vm_value_init(void);

extern PyObject *menai_convert_value(PyObject *src);
extern PyObject *menai_convert_code_object(PyObject *code);
extern PyObject *menai_to_slow(PyObject *src);

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
#define DEST_SHIFT 36
#define SRC0_SHIFT 24
#define SRC1_SHIFT 12
#define FIELD_MASK 0xFFFu
#define OPCODE_MASK 0xFFFFu

/*
 * Opcode values — must match menai_bytecode.py Opcode enum
 */
#define OP_LOAD_NONE 0
#define OP_LOAD_TRUE 1
#define OP_LOAD_FALSE 2
#define OP_LOAD_EMPTY_LIST 3
#define OP_LOAD_EMPTY_DICT 4
#define OP_LOAD_EMPTY_SET 5
#define OP_LOAD_CONST 6
#define OP_LOAD_NAME 7
#define OP_MOVE 8
#define OP_JUMP 20
#define OP_JUMP_IF_FALSE 21
#define OP_JUMP_IF_TRUE 22
#define OP_RAISE_ERROR 23
#define OP_MAKE_CLOSURE 30
#define OP_PATCH_CLOSURE 31
#define OP_CALL 32
#define OP_TAIL_CALL 33
#define OP_APPLY 34
#define OP_TAIL_APPLY 35
#define OP_RETURN 37
#define OP_EMIT_TRACE 40
#define OP_NONE_P 50
#define OP_FUNCTION_P 60
#define OP_FUNCTION_EQ_P 61
#define OP_FUNCTION_NEQ_P 62
#define OP_FUNCTION_MIN_ARITY 63
#define OP_FUNCTION_VARIADIC_P 64
#define OP_FUNCTION_ACCEPTS_P 65
#define OP_SYMBOL_P 80
#define OP_SYMBOL_EQ_P 81
#define OP_SYMBOL_NEQ_P 82
#define OP_SYMBOL_TO_STRING 83
#define OP_BOOLEAN_P 100
#define OP_BOOLEAN_EQ_P 101
#define OP_BOOLEAN_NEQ_P 102
#define OP_BOOLEAN_NOT 103
#define OP_INTEGER_P 120
#define OP_INTEGER_EQ_P 121
#define OP_INTEGER_NEQ_P 122
#define OP_INTEGER_LT_P 123
#define OP_INTEGER_GT_P 124
#define OP_INTEGER_LTE_P 125
#define OP_INTEGER_GTE_P 126
#define OP_INTEGER_ABS 127
#define OP_INTEGER_ADD 128
#define OP_INTEGER_SUB 129
#define OP_INTEGER_MUL 130
#define OP_INTEGER_DIV 131
#define OP_INTEGER_MOD 132
#define OP_INTEGER_NEG 133
#define OP_INTEGER_EXPN 134
#define OP_INTEGER_BIT_NOT 135
#define OP_INTEGER_BIT_SHIFT_LEFT 136
#define OP_INTEGER_BIT_SHIFT_RIGHT 137
#define OP_INTEGER_BIT_OR 138
#define OP_INTEGER_BIT_AND 139
#define OP_INTEGER_BIT_XOR 140
#define OP_INTEGER_MIN 141
#define OP_INTEGER_MAX 142
#define OP_INTEGER_TO_FLOAT 143
#define OP_INTEGER_TO_COMPLEX 144
#define OP_INTEGER_TO_STRING 145
#define OP_INTEGER_CODEPOINT_TO_STRING 146
#define OP_FLOAT_P 160
#define OP_FLOAT_EQ_P 161
#define OP_FLOAT_NEQ_P 162
#define OP_FLOAT_LT_P 163
#define OP_FLOAT_GT_P 164
#define OP_FLOAT_LTE_P 165
#define OP_FLOAT_GTE_P 166
#define OP_FLOAT_NEG 167
#define OP_FLOAT_ADD 168
#define OP_FLOAT_SUB 169
#define OP_FLOAT_MUL 170
#define OP_FLOAT_DIV 171
#define OP_FLOAT_FLOOR_DIV 172
#define OP_FLOAT_MOD 173
#define OP_FLOAT_EXP 174
#define OP_FLOAT_EXPN 175
#define OP_FLOAT_LOG 176
#define OP_FLOAT_LOG10 177
#define OP_FLOAT_LOG2 178
#define OP_FLOAT_LOGN 179
#define OP_FLOAT_SIN 180
#define OP_FLOAT_COS 181
#define OP_FLOAT_TAN 182
#define OP_FLOAT_SQRT 183
#define OP_FLOAT_ABS 184
#define OP_FLOAT_TO_INTEGER 185
#define OP_FLOAT_TO_COMPLEX 186
#define OP_FLOAT_TO_STRING 187
#define OP_FLOAT_FLOOR 188
#define OP_FLOAT_CEIL 189
#define OP_FLOAT_ROUND 190
#define OP_FLOAT_MIN 191
#define OP_FLOAT_MAX 192
#define OP_COMPLEX_P 200
#define OP_COMPLEX_EQ_P 201
#define OP_COMPLEX_NEQ_P 202
#define OP_COMPLEX_REAL 203
#define OP_COMPLEX_IMAG 204
#define OP_COMPLEX_ABS 205
#define OP_COMPLEX_ADD 206
#define OP_COMPLEX_SUB 207
#define OP_COMPLEX_MUL 208
#define OP_COMPLEX_DIV 209
#define OP_COMPLEX_NEG 210
#define OP_COMPLEX_EXP 211
#define OP_COMPLEX_EXPN 212
#define OP_COMPLEX_LOG 213
#define OP_COMPLEX_LOG10 214
#define OP_COMPLEX_LOGN 215
#define OP_COMPLEX_SIN 216
#define OP_COMPLEX_COS 217
#define OP_COMPLEX_TAN 218
#define OP_COMPLEX_SQRT 219
#define OP_COMPLEX_TO_STRING 220
#define OP_STRING_P 240
#define OP_STRING_EQ_P 241
#define OP_STRING_NEQ_P 242
#define OP_STRING_LT_P 243
#define OP_STRING_GT_P 244
#define OP_STRING_LTE_P 245
#define OP_STRING_GTE_P 246
#define OP_STRING_LENGTH 247
#define OP_STRING_UPCASE 248
#define OP_STRING_DOWNCASE 249
#define OP_STRING_TRIM 250
#define OP_STRING_TRIM_LEFT 251
#define OP_STRING_TRIM_RIGHT 252
#define OP_STRING_TO_INTEGER 253
#define OP_STRING_TO_NUMBER 254
#define OP_STRING_TO_LIST 255
#define OP_STRING_REF 256
#define OP_STRING_PREFIX_P 257
#define OP_STRING_SUFFIX_P 258
#define OP_STRING_CONCAT 259
#define OP_STRING_SLICE 260
#define OP_STRING_REPLACE 261
#define OP_STRING_INDEX 262
#define OP_STRING_TO_INTEGER_CODEPOINT 263
#define OP_DICT_P 280
#define OP_DICT_EQ_P 281
#define OP_DICT_NEQ_P 282
#define OP_DICT_KEYS 283
#define OP_DICT_VALUES 284
#define OP_DICT_LENGTH 285
#define OP_DICT_HAS_P 286
#define OP_DICT_REMOVE 287
#define OP_DICT_MERGE 288
#define OP_DICT_SET 289
#define OP_DICT_GET 290
#define OP_LIST_P 300
#define OP_LIST_EQ_P 301
#define OP_LIST_NEQ_P 302
#define OP_LIST_PREPEND 303
#define OP_LIST_APPEND 304
#define OP_LIST_REVERSE 305
#define OP_LIST_FIRST 306
#define OP_LIST_REST 307
#define OP_LIST_LAST 308
#define OP_LIST_LENGTH 309
#define OP_LIST_REF 310
#define OP_LIST_NULL_P 311
#define OP_LIST_MEMBER_P 312
#define OP_LIST_INDEX 313
#define OP_LIST_SLICE 314
#define OP_LIST_REMOVE 315
#define OP_LIST_CONCAT 316
#define OP_LIST_TO_STRING 317
#define OP_LIST_TO_SET 318
#define OP_SET_P 340
#define OP_SET_EQ_P 341
#define OP_SET_NEQ_P 342
#define OP_SET_MEMBER_P 343
#define OP_SET_ADD 344
#define OP_SET_REMOVE 345
#define OP_SET_LENGTH 346
#define OP_SET_UNION 347
#define OP_SET_INTERSECTION 348
#define OP_SET_DIFFERENCE 349
#define OP_SET_SUBSET_P 350
#define OP_SET_TO_LIST 351
#define OP_MAKE_STRUCT 360
#define OP_STRUCT_P 361
#define OP_STRUCT_TYPE_P 362
#define OP_STRUCT_GET 363
#define OP_STRUCT_GET_IMM 364
#define OP_STRUCT_SET 365
#define OP_STRUCT_SET_IMM 366
#define OP_STRUCT_EQ_P 367
#define OP_STRUCT_NEQ_P 368
#define OP_STRUCT_TYPE 369
#define OP_STRUCT_TYPE_NAME 370
#define OP_STRUCT_FIELDS 371
#define OP_RANGE 380

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

/*
 * Fast type-check macros
 */
#define IS_MENAI_NONE(o) (Py_TYPE(o) == Menai_NoneType)
#define IS_MENAI_BOOLEAN(o) (Py_TYPE(o) == Menai_BooleanType)
#define IS_MENAI_INTEGER(o) (Py_TYPE(o) == Menai_IntegerType)
#define IS_MENAI_FLOAT(o) (Py_TYPE(o) == Menai_FloatType)
#define IS_MENAI_COMPLEX(o) (Py_TYPE(o) == Menai_ComplexType)
#define IS_MENAI_STRING(o) (Py_TYPE(o) == Menai_StringType)
#define IS_MENAI_SYMBOL(o) (Py_TYPE(o) == Menai_SymbolType)
#define IS_MENAI_LIST(o) (Py_TYPE(o) == Menai_ListType)
#define IS_MENAI_DICT(o) (Py_TYPE(o) == Menai_DictType)
#define IS_MENAI_SET(o) (Py_TYPE(o) == Menai_SetType)
#define IS_MENAI_FUNCTION(o) (Py_TYPE(o) == Menai_FunctionType)
#define IS_MENAI_STRUCTTYPE(o) (Py_TYPE(o) == Menai_StructTypeType)
#define IS_MENAI_STRUCT(o) (Py_TYPE(o) == Menai_StructType)


static inline int menai_boolean_value(PyObject *o) {
    return ((MenaiBoolean_Object *)o)->value;
}

static inline double menai_float_value(PyObject *o) {
    return ((MenaiFloat_Object *)o)->value;
}

static inline PyObject *menai_symbol_name(PyObject *o) {
    return ((MenaiSymbol_Object *)o)->name;
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
 * menai_integer_compare — compare two MenaiInteger objects using MenaiInt.
 *
 * Fast path for the common case where both are small (is_big == 0): plain C
 * comparison of the long values.  Falls back to menai_int_* for big integers.
 *
 * op must be one of Py_EQ, Py_NE, Py_LT, Py_GT, Py_LE, Py_GE.
 * Never fails.
 */
static inline int
menai_integer_compare(PyObject *a, PyObject *b, int op)
{
    MenaiInteger_Object *ia = (MenaiInteger_Object *)a;
    MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
    if (!ia->is_big && !ib->is_big) {
        long la = ia->small, lb = ib->small;
        switch (op) {
            case Py_EQ: return la == lb;
            case Py_NE: return la != lb;
            case Py_LT: return la < lb;
            case Py_GT: return la > lb;
            case Py_LE: return la <= lb;
            case Py_GE: return la >= lb;
        }
    }
    const MenaiInt *ma = ia->is_big ? &ia->big : NULL;
    const MenaiInt *mb = ib->is_big ? &ib->big : NULL;
    MenaiInt tmp_a, tmp_b;
    menai_int_init(&tmp_a);
    menai_int_init(&tmp_b);
    if (!ia->is_big) menai_int_from_long(ia->small, &tmp_a);
    if (!ib->is_big) menai_int_from_long(ib->small, &tmp_b);
    const MenaiInt *pa = ia->is_big ? ma : &tmp_a;
    const MenaiInt *pb = ib->is_big ? mb : &tmp_b;
    int result;
    switch (op) {
        case Py_EQ: result = menai_int_eq(pa, pb); break;
        case Py_NE: result = menai_int_ne(pa, pb); break;
        case Py_LT: result = menai_int_lt(pa, pb); break;
        case Py_GT: result = menai_int_gt(pa, pb); break;
        case Py_LE: result = menai_int_le(pa, pb); break;
        case Py_GE: result = menai_int_ge(pa, pb); break;
        default: result = 0; break;
    }
    menai_int_free(&tmp_a);
    menai_int_free(&tmp_b);
    return result;
}

/*
 * make_integer_from_ssize_t — create a MenaiInteger from a Py_ssize_t.
 *
 * Py_ssize_t fits in a long on all supported platforms, so this is a direct
 * delegation to menai_integer_from_long.
 */
static inline PyObject *make_integer_from_ssize_t(Py_ssize_t n) {
    return menai_integer_from_long((long)n);
}

static inline PyObject *make_integer_from_long(long n) {
    return menai_integer_from_long(n);
}

static inline PyObject *make_float(double v) {
    MenaiFloat_Object *r = (MenaiFloat_Object *)Menai_FloatType->tp_alloc(Menai_FloatType, 0);
    if (r) r->value = v;
    return (PyObject *)r;
}

static inline PyObject *make_complex(double real, double imag) {
    MenaiComplex_Object *r = (MenaiComplex_Object *)Menai_ComplexType->tp_alloc(Menai_ComplexType, 0);
    if (r) {
        r->real = real;
        r->imag = imag;
    }
    return (PyObject *)r;
}

static inline void bool_store(PyObject **regs, int slot, int cond) {
    reg_set_borrow(regs, slot, cond ? Menai_TRUE : Menai_FALSE);
}

PyObject *menai_raise_eval_error(const char *message);
PyObject *menai_raise_eval_errorf(const char *fmt, ...);

static const char *
menai_type_name(PyObject *val)
{
    PyTypeObject *t = Py_TYPE(val);
    if (t == Menai_NoneType)       return "none";
    if (t == Menai_BooleanType)    return "boolean";
    if (t == Menai_IntegerType)    return "integer";
    if (t == Menai_FloatType)      return "float";
    if (t == Menai_ComplexType)    return "complex";
    if (t == Menai_StringType)     return "string";
    if (t == Menai_SymbolType)     return "symbol";
    if (t == Menai_ListType)       return "list";
    if (t == Menai_DictType)       return "dict";
    if (t == Menai_SetType)        return "set";
    if (t == Menai_FunctionType)   return "function";
    if (t == Menai_StructTypeType) return "struct-type";
    if (t == Menai_StructType)     return "struct";
    return "unknown";
}

static inline int
require_type_impl(int ok, PyObject *val, const char *op_name, const char *noun)
{
    if (ok) return 1;
    menai_raise_eval_errorf("Function '%s' requires %s, got %s",
                            op_name, noun, menai_type_name(val));
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

int
menai_vm_shim_init(void)
{
    PyObject *vc = _menai_vm_value_init();
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

/*
 * Frame struct
 *
 * The C VM maintains a fixed-size stack of Frame structs.  All fields are
 * plain C — no Python objects except those listed below, all of which are
 * kept alive by the frame stack.  constants, names, and local_count are
 * cached here at frame_setup time so the hot loop never calls
 * PyObject_GetAttrString on the code object.
 */
typedef struct {
    PyObject *code_obj;         /* CodeObject — kept alive, not dereferenced in loop */
    PyObject *constants;        /* borrowed ref — list of fast constant values */
    PyObject **constants_items; /* raw pointer into constants ob_item array */
    PyObject *names;            /* borrowed ref — list of global name strings */
    PyObject **names_items;     /* raw pointer into names ob_item array */
    PyObject *closure_caches;   /* borrowed ref — list of child _closure_cache tuples */
    PyObject **closure_caches_items; /* raw pointer into closure_caches ob_item, or NULL */
    uint64_t *instrs;           /* raw C pointer into the array.array buffer */
    int code_len;
    int local_count;
    int ip;
    int base;
    int return_dest;
    int is_sentinel;
} Frame;

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

/*
 * frame_setup
 *
 * Slow path used only for the top-level CodeObject at execute start
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
    f->code_obj = code_obj;
    f->constants = constants;     /* borrowed — f->code_obj keeps code_obj alive */
    Py_DECREF(constants);         /* drop owned ref from GetAttrString */
    f->constants_items = ((PyListObject *)constants)->ob_item;
    f->names = names;             /* borrowed — f->code_obj keeps code_obj alive */
    Py_DECREF(names);             /* drop owned ref from GetAttrString */
    f->names_items = ((PyListObject *)names)->ob_item;
    PyObject *_cc = PyObject_GetAttrString(code_obj, "_code_caches");
    f->closure_caches = (_cc && PyList_Check(_cc)) ? _cc : NULL;
    f->closure_caches_items = f->closure_caches ? ((PyListObject *)f->closure_caches)->ob_item : NULL;
    Py_XDECREF(_cc);  /* drop owned ref — f->code_obj keeps code_obj alive */
    PyErr_Clear();
    f->instrs = (uint64_t *)view.buf;
    f->code_len = (int)(view.len / sizeof(uint64_t));
    f->local_count = local_count;
    f->ip = 0;
    f->base = base;
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
    f->code_obj = code_obj;
    f->instrs = func->instrs;
    f->code_len = func->code_len;
    f->constants = func->constants;
    f->constants_items = func->constants_items;
    f->names = func->names;
    f->names_items = func->names_items;
    f->local_count = func->local_count;
    f->closure_caches = func->closure_caches;  /* borrowed — func owns bytecode which owns it */
    f->closure_caches_items = func->closure_caches_items;
    f->ip = 0;
    f->base = base;
    f->return_dest = return_dest;
    f->is_sentinel = 0;
}

static void
frame_release(Frame *f)
{
    Py_XDECREF(f->code_obj);
    f->code_obj = NULL;
    f->instrs = NULL;
    f->constants = NULL;
    f->constants_items = NULL;
    f->names = NULL;
    f->names_items = NULL;
    f->closure_caches = NULL;
    f->closure_caches_items = NULL;
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
    for (Py_ssize_t i = 0; i < n; i++) Py_DECREF(regs[i]);  /* every slot is an owned reference */
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

    /* Iterative DFS using a plain C pointer stack of borrowed refs. */
    Py_ssize_t stack_cap = 16;
    Py_ssize_t stack_top = 0;
    PyObject **stack = (PyObject **)PyMem_Malloc(stack_cap * sizeof(PyObject *));
    if (stack == NULL) {
        Py_DECREF(children);
        PyErr_NoMemory();
        return -1;
    }

    Py_ssize_t n = PyList_GET_SIZE(children);
    for (Py_ssize_t i = 0; i < n; i++) {
        if (stack_top == stack_cap) {
            stack_cap *= 2;
            PyObject **tmp = (PyObject **)PyMem_Realloc(stack, stack_cap * sizeof(PyObject *));
            if (tmp == NULL) {
                Py_DECREF(children);
                PyMem_Free(stack);
                PyErr_NoMemory();
                return -1;
            }
            stack = tmp;
        }
        stack[stack_top++] = PyList_GET_ITEM(children, i);
    }
    Py_DECREF(children);

    while (stack_top > 0) {
        PyObject *co = stack[--stack_top];

        int lc = 0, oa = 0;
        if (code_get_int(co, "local_count", &lc) < 0 ||
            code_get_int(co, "outgoing_arg_slots", &oa) < 0) {
            PyMem_Free(stack);
            return -1;
        }
        if (lc + oa > best) best = lc + oa;

        PyObject *sub = PyObject_GetAttrString(co, "code_objects");
        if (sub == NULL) {
            PyMem_Free(stack);
            return -1;
        }
        Py_ssize_t m = PyList_GET_SIZE(sub);
        for (Py_ssize_t i = 0; i < m; i++) {
            if (stack_top == stack_cap) {
                stack_cap *= 2;
                PyObject **tmp = (PyObject **)PyMem_Realloc(stack, stack_cap * sizeof(PyObject *));
                if (tmp == NULL) {
                    Py_DECREF(sub);
                    PyMem_Free(stack);
                    PyErr_NoMemory();
                    return -1;
                }
                stack = tmp;
            }
            stack[stack_top++] = PyList_GET_ITEM(sub, i);
        }
        Py_DECREF(sub);
    }
    PyMem_Free(stack);
    return best;
}

/*
 * GlobalsTable — open-addressing hash table for O(1) name lookup.
 *
 * Built once before execution starts from the constants and prelude dicts.
 * Never mutated during execution.  Values are owned references.
 *
 * Lookup takes the UTF-8 string from frame->names_items[src0] (extracted once
 * at build time via PyUnicode_AsUTF8, cached in each slot).  The hash is a
 * FNV-1a string hash so the hot path cost is one hash + one strcmp per probe.
 * The slot count is the smallest power of 2 satisfying slot_count * 2 / 3 >= count.
 */
typedef struct {
    const char *name;  /* UTF-8 — points into PyUnicode internal buffer; NULL = empty */
    Py_hash_t hash;    /* FNV-1a hash of name */
    PyObject *value;   /* owned MenaiValue reference — valid only when name != NULL */
} GlobalsSlot;

typedef struct {
    const char *name;  /* UTF-8 — points into PyUnicode internal buffer */
    PyObject *value;   /* owned MenaiValue reference */
} GlobalsEntry;

typedef struct {
    GlobalsSlot *slots;     /* hash table — slot_count entries */
    GlobalsEntry *entries;  /* flat array — count entries, for iteration */
    Py_ssize_t slot_count;  /* power of 2 */
    Py_ssize_t count;       /* number of live entries */
} GlobalsTable;

static void
globals_free(GlobalsTable *gt)
{
    for (Py_ssize_t i = 0; i < gt->count; i++) Py_XDECREF(gt->entries[i].value);
    PyMem_Free(gt->slots);
    PyMem_Free(gt->entries);
    gt->slots = NULL;
    gt->entries = NULL;
    gt->slot_count = 0;
    gt->count = 0;
}

/*
 * _globals_str_hash — FNV-1a hash of a UTF-8 C string.
 *
 * Returns a value in [0, PY_SSIZE_T_MAX]; never -1.
 */
static inline Py_hash_t
_globals_str_hash(const char *s)
{
    Py_uhash_t h = 14695981039346656037ULL;  /* FNV offset basis */
    const unsigned char *p = (const unsigned char *)s;
    while (*p) {
        h ^= (Py_uhash_t)*p++;
        h *= 1099511628211ULL;  /* FNV prime */
    }
    Py_hash_t r = (Py_hash_t)(h & (Py_uhash_t)PY_SSIZE_T_MAX);
    return r == -1 ? -2 : r;
}

/*
 * globals_build — build a GlobalsTable from one or two Python dicts.
 *
 * prelude_dict may be NULL or Py_None.  Values from both dicts are converted
 * to fast C types via fn_convert_value before being stored.  Returns 0 on
 * success, -1 on error with a Python exception set.
 */
static int
globals_build(GlobalsTable *gt, PyObject *constants_dict, PyObject *prelude_dict)
{
    Py_ssize_t nc = PyDict_Size(constants_dict);
    Py_ssize_t np = (prelude_dict && prelude_dict != Py_None) ? PyDict_Size(prelude_dict) : 0;
    Py_ssize_t total = nc + np;

    gt->slots = NULL;
    gt->entries = NULL;
    gt->slot_count = 0;
    gt->count = 0;

    if (total > 0) {
        gt->entries = (GlobalsEntry *)PyMem_Malloc(total * sizeof(GlobalsEntry));
        if (gt->entries == NULL) {
            PyErr_NoMemory();
            return -1;
        }
        /* Slot count: smallest power of 2 with slot_count * 2 / 3 >= total */
        Py_ssize_t min_slots = (total * 3 + 1) / 2;
        Py_ssize_t sc = 4;
        while (sc < min_slots) sc <<= 1;
        gt->slots = (GlobalsSlot *)PyMem_Malloc(sc * sizeof(GlobalsSlot));
        if (gt->slots == NULL) {
            PyMem_Free(gt->entries);
            gt->entries = NULL;
            PyErr_NoMemory();
            return -1;
        }
        memset(gt->slots, 0, sc * sizeof(GlobalsSlot));
        gt->slot_count = sc;
    }

    PyObject *key, *val;
    Py_ssize_t pos = 0;
    while (PyDict_Next(constants_dict, &pos, &key, &val)) {
        PyObject *converted = menai_convert_value(val);
        if (converted == NULL) {
            globals_free(gt);
            return -1;
        }
        const char *name_utf8 = PyUnicode_AsUTF8(key);
        if (name_utf8 == NULL) {
            Py_DECREF(converted);
            globals_free(gt);
            return -1;
        }
        gt->entries[gt->count].name = name_utf8;
        gt->entries[gt->count].value = converted;
        gt->count++;
    }

    if (np > 0) {
        pos = 0;
        while (PyDict_Next(prelude_dict, &pos, &key, &val)) {
            PyObject *converted = menai_convert_value(val);
            if (converted == NULL) {
                globals_free(gt);
                return -1;
            }
            const char *name_utf8 = PyUnicode_AsUTF8(key);
            if (name_utf8 == NULL) {
                Py_DECREF(converted);
                globals_free(gt);
                return -1;
            }
            gt->entries[gt->count].name = name_utf8;
            gt->entries[gt->count].value = converted;
            gt->count++;
        }
    }

    /* Populate the hash table from the entries array. */
    Py_ssize_t mask = gt->slot_count - 1;
    for (Py_ssize_t i = 0; i < gt->count; i++) {
        const char *name = gt->entries[i].name;
        Py_hash_t h = _globals_str_hash(name);
        Py_uhash_t perturb = (Py_uhash_t)h;
        Py_ssize_t slot = (Py_ssize_t)(perturb & (Py_uhash_t)mask);
        for (;;) {
            if (gt->slots[slot].name == NULL) {
                gt->slots[slot].name = name;
                gt->slots[slot].hash = h;
                gt->slots[slot].value = gt->entries[i].value;
                break;
            }
            perturb >>= 5;
            slot = (Py_ssize_t)((5 * (Py_uhash_t)slot + 1 + perturb) & (Py_uhash_t)mask);
        }
    }
    return 0;
}

static PyObject *
globals_lookup(const GlobalsTable *gt, const char *name)
{
    if (gt->slot_count == 0) return NULL;
    Py_hash_t h = _globals_str_hash(name);
    Py_ssize_t mask = gt->slot_count - 1;
    Py_uhash_t perturb = (Py_uhash_t)h;
    Py_ssize_t slot = (Py_ssize_t)(perturb & (Py_uhash_t)mask);
    for (;;) {
        GlobalsSlot *s = &gt->slots[slot];
        if (s->name == NULL) return NULL;
        if (s->hash == h && strcmp(s->name, name) == 0) return s->value;
        perturb >>= 5;
        slot = (Py_ssize_t)((5 * (Py_uhash_t)slot + 1 + perturb) & (Py_uhash_t)mask);
    }
}

/*
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
 */
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
            const char *fname = (name != NULL && name != Py_None) ? PyUnicode_AsUTF8(name) : "<lambda>";
            menai_raise_eval_errorf(
                "Function '%s' expects at least %d argument%s, got %d",
                fname, min_arity, min_arity == 1 ? "" : "s", arity);
            return -1;
        }
        /* Pack excess args into a MenaiList for the rest parameter. */
        int rest_count = arity - min_arity;
        PyObject **rest_arr = rest_count > 0 ? (PyObject **)PyMem_Malloc(rest_count * sizeof(PyObject *)) : NULL;
        if (rest_count > 0 && !rest_arr) {
            PyErr_NoMemory();
            return -1;
        }
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
    Py_ssize_t ncap = Py_SIZE(func);
    for (Py_ssize_t i = 0; i < ncap; i++) {
        PyObject *cv = func->captures[i];
        reg_set_borrow(regs, callee_base + param_count + (int)i, cv);
    }

    frame_setup_func(new_frame, func, bytecode, callee_base, return_dest);
    return 0;
}

/*
 * Internal execute — called by menai_vm_c_execute after setup.
 * Returns the result value (new reference) or NULL on error.
 * Caller is responsible for calling to_slow() on the result.
 */
static PyObject *
execute_loop(PyObject *code, const GlobalsTable *globals,
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
            PyObject *val = frame->constants_items[src0];
            reg_set_borrow(regs, base + dest, val);
            break;
        }

        case OP_LOAD_NAME: {
            const char *name_str = PyUnicode_AsUTF8(frame->names_items[src0]);
            PyObject *val = globals_lookup(globals, name_str);
            if (val == NULL) {
                /* Build a rich error listing up to 10 available names. */
                Py_ssize_t nk = globals->count;
                Py_ssize_t show = nk < 10 ? nk : 10;
                char buf[1024];
                int off = 0;
                for (Py_ssize_t i = 0; i < show && off < (int)sizeof(buf) - 2; i++) {
                    if (i > 0 && off < (int)sizeof(buf) - 4) {
                        buf[off++] = ',';
                        buf[off++] = ' ';
                    }
                    const char *kn = globals->entries[i].name;
                    int klen = (int)strlen(kn);
                    if (off + klen >= (int)sizeof(buf) - 4) break;
                    memcpy(buf + off, kn, klen);
                    off += klen;
                }
                buf[off] = '\0';
                menai_raise_eval_errorf(
                    "Undefined variable: '%s'\n  Available variables: %s%s",
                    name_str, buf, nk > 10 ? "..." : "");
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
            PyObject *s = menai_string_to_pyunicode(msg);
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
            int arity = src1;

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
                int n_fields = ((MenaiStructType_Object *)raw)->nfields;
                if (arity != (int)n_fields) {
                    PyObject *sname = ((MenaiStructType_Object *)raw)->name;
                    menai_raise_eval_errorf(
                        "Struct constructor '%s' called with wrong number of arguments",
                        sname ? PyUnicode_AsUTF8(sname) : "?");
                    goto error;
                }
                PyObject *instance = menai_struct_alloc(raw, &regs[callee_base], n_fields);
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
                int n_fields = ((MenaiStructType_Object *)raw)->nfields;
                if (n_args != (int)n_fields) {
                    PyObject *sname = ((MenaiStructType_Object *)raw)->name;
                    menai_raise_eval_errorf(
                        "Struct constructor '%s' called with wrong number of arguments",
                        sname ? PyUnicode_AsUTF8(sname) : "?");
                    Py_DECREF(raw);
                    goto error;
                }
                PyObject *instance = menai_struct_alloc(raw, &regs[base + local_count], n_fields);
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
            bool_store(regs, base + dest, na == nb);
            break;
        }

        case OP_SYMBOL_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_symbol_pair(a, b, "symbol!=?")) goto error;
            PyObject *na = menai_symbol_name(a);
            PyObject *nb = menai_symbol_name(b);
            bool_store(regs, base + dest, na != nb);
            break;
        }

        case OP_SYMBOL_TO_STRING: {
            PyObject *a = regs[base + src0];
            if (!require_symbol(a, "symbol->string")) goto error;
            PyObject *name = menai_symbol_name(a);
            PyObject *r = menai_string_from_pyunicode(name);
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
            PyObject *_r = make_integer_from_long(min_a);
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
            MenaiInteger_Object *n_io = (MenaiInteger_Object *)n_obj;
            long n;
            if (!n_io->is_big) {
                n = n_io->small;
            } else {
                if (menai_int_to_long(&n_io->big, &n) < 0) goto error;
            }
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
            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_EQ));
            break;
        }

        case OP_INTEGER_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer!=?")) goto error;
            if (!require_integer(b, "integer!=?")) goto error;
            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_NE));
            break;
        }

        case OP_INTEGER_LT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer<?")) goto error;
            if (!require_integer(b, "integer<?")) goto error;
            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_LT));
            break;
        }

        case OP_INTEGER_GT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer>?")) goto error;
            if (!require_integer(b, "integer>?")) goto error;
            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_GT));
            break;
        }

        case OP_INTEGER_LTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer<=?")) goto error;
            if (!require_integer(b, "integer<=?")) goto error;
            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_LE));
            break;
        }

        case OP_INTEGER_GTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer>=?")) goto error;
            if (!require_integer(b, "integer>=?")) goto error;
            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_GE));
            break;
        }

        case OP_INTEGER_ABS: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-abs")) goto error;
            MenaiInteger_Object *ia = (MenaiInteger_Object *)a;
            if (!ia->is_big) {
                long sv = ia->small;
                long rv = sv < 0 ? -sv : sv;
                /* LONG_MIN has no positive counterpart — promote to bigint. */
                if (sv == LONG_MIN) {
                    MenaiInt tmp, res;
                    menai_int_init(&tmp);
                    menai_int_init(&res);
                    if (menai_int_from_long(sv, &tmp) < 0) goto error;
                    if (menai_int_abs(&tmp, &res) < 0) { menai_int_free(&tmp); goto error; }
                    menai_int_free(&tmp);
                    PyObject *_r = menai_integer_from_bigint(res);
                    if (!_r) goto error;
                    reg_set_own(regs, base + dest, _r);
                    break;
                }
                PyObject *_r = menai_integer_from_long(rv);
                if (!_r) goto error;
                reg_set_own(regs, base + dest, _r);
                break;
            }
            MenaiInt res;
            menai_int_init(&res);
            if (menai_int_abs(&ia->big, &res) < 0) goto error;
            PyObject *_r = menai_integer_from_bigint(res);
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_NEG: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-neg")) goto error;
            MenaiInteger_Object *ia = (MenaiInteger_Object *)a;
            if (!ia->is_big) {
                long sv = ia->small;
                /* LONG_MIN negation overflows — promote to bigint. */
                if (sv == LONG_MIN) {
                    MenaiInt tmp, res;
                    menai_int_init(&tmp);
                    menai_int_init(&res);
                    if (menai_int_from_long(sv, &tmp) < 0) goto error;
                    if (menai_int_neg(&tmp, &res) < 0) { menai_int_free(&tmp); goto error; }
                    menai_int_free(&tmp);
                    PyObject *_r = menai_integer_from_bigint(res);
                    if (!_r) goto error;
                    reg_set_own(regs, base + dest, _r);
                    break;
                }
                PyObject *_r = menai_integer_from_long(-sv);
                if (!_r) goto error;
                reg_set_own(regs, base + dest, _r);
                break;
            }
            MenaiInt res;
            menai_int_init(&res);
            if (menai_int_neg(&ia->big, &res) < 0) goto error;
            PyObject *_r = menai_integer_from_bigint(res);
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_NOT: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-bit-not")) goto error;
            MenaiInteger_Object *ia = (MenaiInteger_Object *)a;
            MenaiInt tmp, res;
            menai_int_init(&tmp);
            menai_int_init(&res);
            if (!ia->is_big) {
                if (menai_int_from_long(ia->small, &tmp) < 0) goto error;
            } else {
                if (menai_int_copy(&ia->big, &tmp) < 0) goto error;
            }
            if (menai_int_not(&tmp, &res) < 0) { menai_int_free(&tmp); goto error; }
            menai_int_free(&tmp);
            PyObject *_r = menai_integer_from_bigint(res);
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_ADD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer+")) goto error;
            if (!require_integer(b, "integer+")) goto error;
            if (!((MenaiInteger_Object *)a)->is_big && !((MenaiInteger_Object *)b)->is_big) {
                long la = ((MenaiInteger_Object *)a)->small;
                long lb = ((MenaiInteger_Object *)b)->small;
                long lr;
                if (!_menai_add_overflow(la, lb, &lr)) {
                    PyObject *_r = menai_integer_from_long(lr);
                    if (!_r) goto error;
                    reg_set_own(regs, base + dest, _r);
                    break;
                }
            }
            MenaiInt av, bv, res;
            menai_int_init(&av); menai_int_init(&bv); menai_int_init(&res);
            if (!((MenaiInteger_Object *)a)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)a)->small, &av) < 0) goto error; }
            else { if (menai_int_copy(&((MenaiInteger_Object *)a)->big, &av) < 0) goto error; }
            if (!((MenaiInteger_Object *)b)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)b)->small, &bv) < 0) { menai_int_free(&av); goto error; } }
            else { if (menai_int_copy(&((MenaiInteger_Object *)b)->big, &bv) < 0) { menai_int_free(&av); goto error; } }
            if (menai_int_add(&av, &bv, &res) < 0) { menai_int_free(&av); menai_int_free(&bv); goto error; }
            menai_int_free(&av); menai_int_free(&bv);
            PyObject *_r = menai_integer_from_bigint(res);
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_SUB: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-")) goto error;
            if (!require_integer(b, "integer-")) goto error;
            if (!((MenaiInteger_Object *)a)->is_big && !((MenaiInteger_Object *)b)->is_big) {
                long la = ((MenaiInteger_Object *)a)->small;
                long lb = ((MenaiInteger_Object *)b)->small;
                long lr;
                if (!_menai_sub_overflow(la, lb, &lr)) {
                    PyObject *_r = menai_integer_from_long(lr);
                    if (!_r) goto error;
                    reg_set_own(regs, base + dest, _r);
                    break;
                }
            }
            MenaiInt av, bv, res;
            menai_int_init(&av); menai_int_init(&bv); menai_int_init(&res);
            if (!((MenaiInteger_Object *)a)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)a)->small, &av) < 0) goto error; }
            else { if (menai_int_copy(&((MenaiInteger_Object *)a)->big, &av) < 0) goto error; }
            if (!((MenaiInteger_Object *)b)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)b)->small, &bv) < 0) { menai_int_free(&av); goto error; } }
            else { if (menai_int_copy(&((MenaiInteger_Object *)b)->big, &bv) < 0) { menai_int_free(&av); goto error; } }
            if (menai_int_sub(&av, &bv, &res) < 0) { menai_int_free(&av); menai_int_free(&bv); goto error; }
            menai_int_free(&av); menai_int_free(&bv);
            PyObject *_r = menai_integer_from_bigint(res);
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_MUL: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer*")) goto error;
            if (!require_integer(b, "integer*")) goto error;
            if (!((MenaiInteger_Object *)a)->is_big && !((MenaiInteger_Object *)b)->is_big) {
                long la = ((MenaiInteger_Object *)a)->small;
                long lb = ((MenaiInteger_Object *)b)->small;
                long lr;
                if (!_menai_mul_overflow(la, lb, &lr)) {
                    PyObject *_r = menai_integer_from_long(lr);
                    if (!_r) goto error;
                    reg_set_own(regs, base + dest, _r);
                    break;
                }
            }
            MenaiInt av, bv, res;
            menai_int_init(&av); menai_int_init(&bv); menai_int_init(&res);
            if (!((MenaiInteger_Object *)a)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)a)->small, &av) < 0) goto error; }
            else { if (menai_int_copy(&((MenaiInteger_Object *)a)->big, &av) < 0) goto error; }
            if (!((MenaiInteger_Object *)b)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)b)->small, &bv) < 0) { menai_int_free(&av); goto error; } }
            else { if (menai_int_copy(&((MenaiInteger_Object *)b)->big, &bv) < 0) { menai_int_free(&av); goto error; } }
            if (menai_int_mul(&av, &bv, &res) < 0) { menai_int_free(&av); menai_int_free(&bv); goto error; }
            menai_int_free(&av); menai_int_free(&bv);
            PyObject *_r = menai_integer_from_bigint(res);
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_DIV: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer/")) goto error;
            if (!require_integer(b, "integer/")) goto error;
            {
                MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
                int b_is_zero = (!ib->is_big && ib->small == 0) ||
                                (ib->is_big && ib->big.sign == 0);
                if (b_is_zero) {
                menai_raise_eval_error("Division by zero in 'integer/'");
                goto error;
                }
            }
            MenaiInt av, bv, res;
            menai_int_init(&av); menai_int_init(&bv); menai_int_init(&res);
            if (!((MenaiInteger_Object *)a)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)a)->small, &av) < 0) goto error; }
            else { if (menai_int_copy(&((MenaiInteger_Object *)a)->big, &av) < 0) goto error; }
            if (!((MenaiInteger_Object *)b)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)b)->small, &bv) < 0) { menai_int_free(&av); goto error; } }
            else { if (menai_int_copy(&((MenaiInteger_Object *)b)->big, &bv) < 0) { menai_int_free(&av); goto error; } }
            if (menai_int_floordiv(&av, &bv, &res) < 0) { menai_int_free(&av); menai_int_free(&bv); goto error; }
            menai_int_free(&av); menai_int_free(&bv);
            PyObject *_r = menai_integer_from_bigint(res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_MOD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer%")) goto error;
            if (!require_integer(b, "integer%")) goto error;
            {
                MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
                int b_is_zero = (!ib->is_big && ib->small == 0) ||
                                (ib->is_big && ib->big.sign == 0);
                if (b_is_zero) {
                menai_raise_eval_error("Modulo by zero in 'integer%'");
                goto error;
                }
            }
            MenaiInt av, bv, res;
            menai_int_init(&av); menai_int_init(&bv); menai_int_init(&res);
            if (!((MenaiInteger_Object *)a)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)a)->small, &av) < 0) goto error; }
            else { if (menai_int_copy(&((MenaiInteger_Object *)a)->big, &av) < 0) goto error; }
            if (!((MenaiInteger_Object *)b)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)b)->small, &bv) < 0) { menai_int_free(&av); goto error; } }
            else { if (menai_int_copy(&((MenaiInteger_Object *)b)->big, &bv) < 0) { menai_int_free(&av); goto error; } }
            if (menai_int_mod(&av, &bv, &res) < 0) { menai_int_free(&av); menai_int_free(&bv); goto error; }
            menai_int_free(&av); menai_int_free(&bv);
            PyObject *_r = menai_integer_from_bigint(res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_EXPN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-expn")) goto error;
            if (!require_integer(b, "integer-expn")) goto error;
            {
                MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
                int b_is_neg = (!ib->is_big && ib->small < 0) ||
                               (ib->is_big && ib->big.sign == -1);
                if (b_is_neg) {
                menai_raise_eval_error("Function 'integer-expn' requires a non-negative exponent");
                goto error;
                }
            }
            MenaiInt av, bv, res;
            menai_int_init(&av); menai_int_init(&bv); menai_int_init(&res);
            if (!((MenaiInteger_Object *)a)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)a)->small, &av) < 0) goto error; }
            else { if (menai_int_copy(&((MenaiInteger_Object *)a)->big, &av) < 0) goto error; }
            if (!((MenaiInteger_Object *)b)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)b)->small, &bv) < 0) { menai_int_free(&av); goto error; } }
            else { if (menai_int_copy(&((MenaiInteger_Object *)b)->big, &bv) < 0) { menai_int_free(&av); goto error; } }
            if (menai_int_pow(&av, &bv, &res) < 0) { menai_int_free(&av); menai_int_free(&bv); goto error; }
            menai_int_free(&av); menai_int_free(&bv);
            PyObject *_r = menai_integer_from_bigint(res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_OR: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-or")) goto error;
            if (!require_integer(b, "integer-bit-or")) goto error;
            MenaiInt av, bv, res;
            menai_int_init(&av); menai_int_init(&bv); menai_int_init(&res);
            if (!((MenaiInteger_Object *)a)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)a)->small, &av) < 0) goto error; }
            else { if (menai_int_copy(&((MenaiInteger_Object *)a)->big, &av) < 0) goto error; }
            if (!((MenaiInteger_Object *)b)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)b)->small, &bv) < 0) { menai_int_free(&av); goto error; } }
            else { if (menai_int_copy(&((MenaiInteger_Object *)b)->big, &bv) < 0) { menai_int_free(&av); goto error; } }
            if (menai_int_or(&av, &bv, &res) < 0) { menai_int_free(&av); menai_int_free(&bv); goto error; }
            menai_int_free(&av); menai_int_free(&bv);
            PyObject *_r = menai_integer_from_bigint(res);
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_AND: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-and")) goto error;
            if (!require_integer(b, "integer-bit-and")) goto error;
            MenaiInt av, bv, res;
            menai_int_init(&av); menai_int_init(&bv); menai_int_init(&res);
            if (!((MenaiInteger_Object *)a)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)a)->small, &av) < 0) goto error; }
            else { if (menai_int_copy(&((MenaiInteger_Object *)a)->big, &av) < 0) goto error; }
            if (!((MenaiInteger_Object *)b)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)b)->small, &bv) < 0) { menai_int_free(&av); goto error; } }
            else { if (menai_int_copy(&((MenaiInteger_Object *)b)->big, &bv) < 0) { menai_int_free(&av); goto error; } }
            if (menai_int_and(&av, &bv, &res) < 0) { menai_int_free(&av); menai_int_free(&bv); goto error; }
            menai_int_free(&av); menai_int_free(&bv);
            PyObject *_r = menai_integer_from_bigint(res);
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_XOR: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-xor")) goto error;
            if (!require_integer(b, "integer-bit-xor")) goto error;
            MenaiInt av, bv, res;
            menai_int_init(&av); menai_int_init(&bv); menai_int_init(&res);
            if (!((MenaiInteger_Object *)a)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)a)->small, &av) < 0) goto error; }
            else { if (menai_int_copy(&((MenaiInteger_Object *)a)->big, &av) < 0) goto error; }
            if (!((MenaiInteger_Object *)b)->is_big) { if (menai_int_from_long(((MenaiInteger_Object *)b)->small, &bv) < 0) { menai_int_free(&av); goto error; } }
            else { if (menai_int_copy(&((MenaiInteger_Object *)b)->big, &bv) < 0) { menai_int_free(&av); goto error; } }
            if (menai_int_xor(&av, &bv, &res) < 0) { menai_int_free(&av); menai_int_free(&bv); goto error; }
            menai_int_free(&av); menai_int_free(&bv);
            PyObject *_r = menai_integer_from_bigint(res);
            if (!_r) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_SHIFT_LEFT: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-shift-left")) goto error;
            if (!require_integer(b, "integer-bit-shift-left")) goto error;
            {
                MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
                long shift;
                if (!ib->is_big) {
                    shift = ib->small;
                } else {
                    if (!menai_int_fits_long(&ib->big)) {
                        menai_raise_eval_error("integer-bit-shift-left: shift amount too large");
                        goto error;
                    }
                    if (menai_int_to_long(&ib->big, &shift) < 0) goto error;
                }
                if (shift < 0) {
                    menai_raise_eval_error("integer-bit-shift-left: shift amount must be non-negative");
                    goto error;
                }
                MenaiInteger_Object *ia = (MenaiInteger_Object *)a;
                MenaiInt av, res;
                menai_int_init(&av); menai_int_init(&res);
                if (!ia->is_big) { if (menai_int_from_long(ia->small, &av) < 0) goto error; }
                else { if (menai_int_copy(&ia->big, &av) < 0) goto error; }
                if (menai_int_shift_left(&av, (Py_ssize_t)shift, &res) < 0) {
                    menai_int_free(&av); goto error;
                }
                menai_int_free(&av);
                PyObject *_r = menai_integer_from_bigint(res);
                if (!_r) goto error;
                reg_set_own(regs, base + dest, _r);
            }
            break;
        }

        case OP_INTEGER_BIT_SHIFT_RIGHT: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-shift-right")) goto error;
            if (!require_integer(b, "integer-bit-shift-right")) goto error;
            {
                MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
                long shift;
                if (!ib->is_big) {
                    shift = ib->small;
                } else {
                    if (!menai_int_fits_long(&ib->big)) {
                        menai_raise_eval_error("integer-bit-shift-right: shift amount too large");
                        goto error;
                    }
                    if (menai_int_to_long(&ib->big, &shift) < 0) goto error;
                }
                if (shift < 0) {
                    menai_raise_eval_error("integer-bit-shift-right: shift amount must be non-negative");
                    goto error;
                }
                MenaiInteger_Object *ia = (MenaiInteger_Object *)a;
                MenaiInt av, res;
                menai_int_init(&av); menai_int_init(&res);
                if (!ia->is_big) { if (menai_int_from_long(ia->small, &av) < 0) goto error; }
                else { if (menai_int_copy(&ia->big, &av) < 0) goto error; }
                if (menai_int_shift_right(&av, (Py_ssize_t)shift, &res) < 0) {
                    menai_int_free(&av); goto error;
                }
                menai_int_free(&av);
                PyObject *_r = menai_integer_from_bigint(res);
                if (!_r) goto error;
                reg_set_own(regs, base + dest, _r);
            }
            break;
        }

        case OP_INTEGER_MIN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-min")) goto error;
            if (!require_integer(b, "integer-min")) goto error;
            reg_set_borrow(regs, base + dest, menai_integer_compare(a, b, Py_LE) ? a : b);
            break;
        }

        case OP_INTEGER_MAX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer-max")) goto error;
            if (!require_integer(b, "integer-max")) goto error;
            reg_set_borrow(regs, base + dest, menai_integer_compare(a, b, Py_GE) ? a : b);
            break;
        }

        case OP_INTEGER_TO_FLOAT: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer->float")) goto error;
            MenaiInteger_Object *ia = (MenaiInteger_Object *)a;
            double d;
            if (!ia->is_big) {
                d = (double)ia->small;
            } else {
                if (menai_int_to_double(&ia->big, &d) < 0) goto error;
            }
            PyObject *_r = make_float(d);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_TO_COMPLEX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer->complex")) goto error;
            if (!require_integer(b, "integer->complex")) goto error;
            MenaiInteger_Object *ia = (MenaiInteger_Object *)a;
            MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
            double re, im;
            if (!ia->is_big) {
                re = (double)ia->small;
            } else {
                if (menai_int_to_double(&ia->big, &re) < 0) goto error;
            }
            if (!ib->is_big) {
                im = (double)ib->small;
            } else {
                if (menai_int_to_double(&ib->big, &im) < 0) goto error;
            }
            PyObject *r = make_complex(re, im);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_INTEGER_TO_STRING: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_integer(a, "integer->string")) goto error;
            if (!require_integer(b, "integer->string")) goto error;
            MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
            long radix;
            if (!ib->is_big) {
                radix = ib->small;
            } else {
                if (menai_int_to_long(&ib->big, &radix) < 0) goto error;
            }
            if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
                menai_raise_eval_errorf("integer->string: radix must be 2, 8, 10, or 16, got %ld", radix);
                goto error;
            }
            MenaiInteger_Object *ia = (MenaiInteger_Object *)a;
            MenaiInt tmp;
            menai_int_init(&tmp);
            if (!ia->is_big) { if (menai_int_from_long(ia->small, &tmp) < 0) goto error; }
            else { if (menai_int_copy(&ia->big, &tmp) < 0) goto error; }
            char *cstr = NULL;
            if (menai_int_to_string(&tmp, (int)radix, &cstr) < 0) {
                menai_int_free(&tmp); goto error;
            }
            menai_int_free(&tmp);
            PyObject *r = menai_string_from_utf8(cstr, (Py_ssize_t)strlen(cstr));
            PyMem_Free(cstr);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_INTEGER_CODEPOINT_TO_STRING: {
            PyObject *a = regs[base + src0];
            if (!require_integer(a, "integer-codepoint->string")) goto error;
            MenaiInteger_Object *ia = (MenaiInteger_Object *)a;
            long cp;
            if (!ia->is_big) {
                cp = ia->small;
            } else {
                if (menai_int_to_long(&ia->big, &cp) < 0) goto error;
            }
            if (cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF)) {
                menai_raise_eval_errorf(
                    "integer-codepoint->string: invalid Unicode scalar value %ld", cp);
                goto error;
            }
            PyObject *r = menai_string_from_codepoint((uint32_t)cp);
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
            MenaiInt res;
            menai_int_init(&res);
            if (menai_int_from_double(trunc(v), &res) < 0) goto error;
            PyObject *_r = menai_integer_from_bigint(res);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_TO_COMPLEX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_float(a, "float->complex")) goto error;
            if (!require_float(b, "float->complex")) goto error;
            PyObject *r = make_complex(menai_float_value(a), menai_float_value(b));
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_FLOAT_TO_STRING: {
            PyObject *a = regs[base + src0];
            if (!require_float(a, "float->string")) goto error;
            char *_fsbuf = PyOS_double_to_string(menai_float_value(a), 'r', 0,
                                                 Py_DTSF_ADD_DOT_0, NULL);
            if (_fsbuf == NULL) goto error;
            PyObject *r = menai_string_from_utf8(_fsbuf, (Py_ssize_t)strlen(_fsbuf));
            PyMem_Free(_fsbuf);
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
             * All metadata is read from the ClosureCache struct stored in a
             * PyCapsule built once by menai_convert_code_object — zero
             * PyTuple_GET_ITEM or PyLong_AsLong calls on this path.
             */
            if (frame->closure_caches == NULL) {
                menai_raise_eval_error("MAKE_CLOSURE: _code_caches not set on code object");
                goto error;
            }

            PyObject *capsule = frame->closure_caches_items[src0];
            const ClosureCache *cc = (const ClosureCache *)PyCapsule_GetPointer(capsule, CLOSURE_CACHE_CAPSULE_NAME);
            if (cc == NULL) goto error;
            PyObject *func = menai_function_alloc(cc, Menai_NONE);
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
            PyObject *val = regs[base + src2];
            MenaiFunction_Object *fn = (MenaiFunction_Object *)closure;
            PyObject *old = fn->captures[src1];
            Py_INCREF(val);
            fn->captures[src1] = val;
            Py_DECREF(old);
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
                int n_fields = ((MenaiStructType_Object *)raw_func)->nfields;
                if (arity != (int)n_fields) {
                    menai_raise_eval_error("Struct constructor called with wrong number of arguments");
                    goto error;
                }

                PyObject *instance = menai_struct_alloc(raw_func, elements, n_fields);
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
                int n_fields = ((MenaiStructType_Object *)raw_func)->nfields;
                if (arity != (int)n_fields) {
                    Py_DECREF(raw_func);
                    Py_DECREF(raw_args);
                    menai_raise_eval_error("Struct constructor called with wrong number of arguments");
                    goto error;
                }

                PyObject *retval = menai_struct_alloc(raw_func, elements, n_fields);
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
            bool_store(regs, base + dest,
                ((MenaiComplex_Object *)a)->real == ((MenaiComplex_Object *)b)->real &&
                ((MenaiComplex_Object *)a)->imag == ((MenaiComplex_Object *)b)->imag);
            break;
        }

        case OP_COMPLEX_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex!=?")) goto error;
            if (!require_complex(b, "complex!=?")) goto error;
            bool_store(regs, base + dest,
                ((MenaiComplex_Object *)a)->real != ((MenaiComplex_Object *)b)->real ||
                ((MenaiComplex_Object *)a)->imag != ((MenaiComplex_Object *)b)->imag);
            break;
        }

        case OP_COMPLEX_REAL: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-real")) goto error;
            PyObject *_fr = make_float(((MenaiComplex_Object *)a)->real);
            if (_fr == NULL) goto error;
            reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_IMAG: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-imag")) goto error;
            PyObject *_fr = make_float(((MenaiComplex_Object *)a)->imag);
            if (_fr == NULL) goto error;
            reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_ABS: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-abs")) goto error;
            double re = ((MenaiComplex_Object *)a)->real;
            double im = ((MenaiComplex_Object *)a)->imag;
            PyObject *_fr = make_float(sqrt(re * re + im * im));
            if (_fr == NULL) goto error;
            reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_NEG: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-neg")) goto error;
            PyObject *_r = make_complex(-((MenaiComplex_Object *)a)->real,
                                        -((MenaiComplex_Object *)a)->imag);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_ADD: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex+")) goto error;
            if (!require_complex(b, "complex+")) goto error;
            PyObject *_r = make_complex(
                ((MenaiComplex_Object *)a)->real + ((MenaiComplex_Object *)b)->real,
                ((MenaiComplex_Object *)a)->imag + ((MenaiComplex_Object *)b)->imag);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SUB: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex-")) goto error;
            if (!require_complex(b, "complex-")) goto error;
            PyObject *_r = make_complex(
                ((MenaiComplex_Object *)a)->real - ((MenaiComplex_Object *)b)->real,
                ((MenaiComplex_Object *)a)->imag - ((MenaiComplex_Object *)b)->imag);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_MUL: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex*")) goto error;
            if (!require_complex(b, "complex*")) goto error;
            double ar = ((MenaiComplex_Object *)a)->real, ai = ((MenaiComplex_Object *)a)->imag;
            double br = ((MenaiComplex_Object *)b)->real, bi = ((MenaiComplex_Object *)b)->imag;
            PyObject *_r = make_complex(ar * br - ai * bi, ar * bi + ai * br);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_DIV: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex/")) goto error;
            if (!require_complex(b, "complex/")) goto error;
            double ar = ((MenaiComplex_Object *)a)->real, ai = ((MenaiComplex_Object *)a)->imag;
            double br = ((MenaiComplex_Object *)b)->real, bi = ((MenaiComplex_Object *)b)->imag;
            if (br == 0.0 && bi == 0.0) {
                menai_raise_eval_error("Division by zero in 'complex/'");
                goto error;
            }
            double denom = br * br + bi * bi;
            PyObject *_r = make_complex(
                (ar * br + ai * bi) / denom,
                (ai * br - ar * bi) / denom);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_EXPN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex-expn")) goto error;
            if (!require_complex(b, "complex-expn")) goto error;
            mc_t za = mc(((MenaiComplex_Object *)a)->real, ((MenaiComplex_Object *)a)->imag);
            mc_t zb = mc(((MenaiComplex_Object *)b)->real, ((MenaiComplex_Object *)b)->imag);
            mc_t cr = mc_pow(za, zb);
            PyObject *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_EXP: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-exp")) goto error;
            mc_t z = mc(((MenaiComplex_Object *)a)->real, ((MenaiComplex_Object *)a)->imag);
            mc_t cr = mc_exp(z);
            PyObject *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOG: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-log")) goto error;
            mc_t z = mc(((MenaiComplex_Object *)a)->real, ((MenaiComplex_Object *)a)->imag);
            mc_t cr = mc_log(z);
            PyObject *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOG10: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-log10")) goto error;
            mc_t z = mc(((MenaiComplex_Object *)a)->real, ((MenaiComplex_Object *)a)->imag);
            mc_t cr = mc_log10(z);
            PyObject *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SIN: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-sin")) goto error;
            mc_t z = mc(((MenaiComplex_Object *)a)->real, ((MenaiComplex_Object *)a)->imag);
            mc_t cr = mc_sin(z);
            PyObject *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_COS: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-cos")) goto error;
            mc_t z = mc(((MenaiComplex_Object *)a)->real, ((MenaiComplex_Object *)a)->imag);
            mc_t cr = mc_cos(z);
            PyObject *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_TAN: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-tan")) goto error;
            mc_t z = mc(((MenaiComplex_Object *)a)->real, ((MenaiComplex_Object *)a)->imag);
            mc_t cr = mc_tan(z);
            PyObject *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SQRT: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex-sqrt")) goto error;
            mc_t z = mc(((MenaiComplex_Object *)a)->real, ((MenaiComplex_Object *)a)->imag);
            mc_t cr = mc_sqrt(z);
            PyObject *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOGN: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_complex(a, "complex-logn")) goto error;
            if (!require_complex(b, "complex-logn")) goto error;
            mc_t za = mc(((MenaiComplex_Object *)a)->real, ((MenaiComplex_Object *)a)->imag);
            mc_t zb = mc(((MenaiComplex_Object *)b)->real, ((MenaiComplex_Object *)b)->imag);
            if (mc_zero(zb)) {
                menai_raise_eval_error("Function 'complex-logn' requires a non-zero base");
                goto error;
            }
            mc_t cr = mc_logn(za, zb);
            PyObject *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_TO_STRING: {
            PyObject *a = regs[base + src0];
            if (!require_complex(a, "complex->string")) goto error;
            PyObject *py_str = MenaiComplex_describe(a, NULL);
            if (py_str == NULL) goto error;
            PyObject *r = menai_string_from_pyunicode(py_str);
            Py_DECREF(py_str);
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
            bool_store(regs, base + dest, menai_string_equal(a, b));
            break;
        }

        case OP_STRING_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string!=?")) goto error;
            if (!require_string(b, "string!=?")) goto error;
            bool_store(regs, base + dest, !menai_string_equal(a, b));
            break;
        }

        case OP_STRING_LT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string<?")) goto error;
            if (!require_string(b, "string<?")) goto error;
            bool_store(regs, base + dest, menai_string_compare(a, b) < 0);
            break;
        }

        case OP_STRING_GT_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string>?")) goto error;
            if (!require_string(b, "string>?")) goto error;
            bool_store(regs, base + dest, menai_string_compare(a, b) > 0);
            break;
        }

        case OP_STRING_LTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string<=?")) goto error;
            if (!require_string(b, "string<=?")) goto error;
            bool_store(regs, base + dest, menai_string_compare(a, b) <= 0);
            break;
        }

        case OP_STRING_GTE_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string>=?")) goto error;
            if (!require_string(b, "string>=?")) goto error;
            bool_store(regs, base + dest, menai_string_compare(a, b) >= 0);
            break;
        }

        case OP_STRING_LENGTH: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-length")) goto error;
            PyObject *_r = make_integer_from_ssize_t(menai_string_length(a));
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_STRING_UPCASE: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-upcase")) goto error;
            PyObject *r = menai_string_upcase(a);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_DOWNCASE: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-downcase")) goto error;
            PyObject *r = menai_string_downcase(a);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_TRIM: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-trim")) goto error;
            PyObject *r = menai_string_trim(a);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_TRIM_LEFT: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-trim-left")) goto error;
            PyObject *r = menai_string_trim_left(a);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_TRIM_RIGHT: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string-trim-right")) goto error;
            PyObject *r = menai_string_trim_right(a);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_CONCAT: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-concat")) goto error;
            if (!require_string(b, "string-concat")) goto error;
            PyObject *r = menai_string_concat(a, b);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_PREFIX_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-prefix?")) goto error;
            if (!require_string(b, "string-prefix?")) goto error;
            bool_store(regs, base + dest, menai_string_has_prefix(a, b));
            break;
        }

        case OP_STRING_SUFFIX_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-suffix?")) goto error;
            if (!require_string(b, "string-suffix?")) goto error;
            bool_store(regs, base + dest, menai_string_has_suffix(a, b));
            break;
        }

        case OP_STRING_REF: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-ref")) goto error;
            if (!IS_MENAI_INTEGER(b)) {
                menai_raise_eval_error("string-ref: index must be integer");
                goto error;
            }
            MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
            long idx_l;
            if (!ib->is_big) { idx_l = ib->small; }
            else { if (menai_int_to_long(&ib->big, &idx_l) < 0) goto error; }
            Py_ssize_t idx = (Py_ssize_t)idx_l;
            Py_ssize_t slen = menai_string_length(a);
            if (idx < 0 || idx >= slen) {
                menai_raise_eval_errorf("string-ref index out of range: %zd", idx);
                goto error;
            }
            PyObject *r = menai_string_ref(a, idx);
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
            MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
            MenaiInteger_Object *ic = (MenaiInteger_Object *)c;
            long start_l, end_l;
            if (!ib->is_big) { start_l = ib->small; } else { if (menai_int_to_long(&ib->big, &start_l) < 0) goto error; }
            if (!ic->is_big) { end_l = ic->small; } else { if (menai_int_to_long(&ic->big, &end_l) < 0) goto error; }
            Py_ssize_t start = (Py_ssize_t)start_l, end = (Py_ssize_t)end_l;
            Py_ssize_t slen = menai_string_length(a);
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
            PyObject *r = menai_string_slice(a, start, end);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_REPLACE: {
            PyObject *a = regs[base + src0], *b = regs[base + src1], *c = regs[base + src2];
            if (!require_string(a, "string-replace")) goto error;
            if (!require_string(b, "string-replace")) goto error;
            if (!require_string(c, "string-replace")) goto error;
            PyObject *r = menai_string_replace(a, b, c);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_INDEX: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string-index")) goto error;
            if (!require_string(b, "string-index")) goto error;
            Py_ssize_t idx = menai_string_find(a, b);
            if (idx == -2) goto error;
            if (idx == -1) {
                reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                PyObject *_r = make_integer_from_ssize_t(idx);
                if (_r == NULL) goto error;
                reg_set_own(regs, base + dest, _r);
            }
            break;
        }

        case OP_STRING_TO_INTEGER_CODEPOINT: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string->integer-codepoint")) goto error;
            Py_ssize_t slen = menai_string_length(a);
            if (slen != 1) {
                menai_raise_eval_error("string->integer-codepoint: requires single-character string");
                goto error;
            }
            PyObject *_r = make_integer_from_long((long)menai_string_get(a, 0));
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
            MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
            long radix;
            if (!ib->is_big) { radix = ib->small; }
            else { if (menai_int_to_long(&ib->big, &radix) < 0) goto error; }
            if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
                menai_raise_eval_errorf("string->integer radix must be 2, 8, 10, or 16, got %ld", radix);
                goto error;
            }
            PyObject *trimmed = menai_string_trim(a);
            if (trimmed == NULL) goto error;
            MenaiInt sti_tmp;
            menai_int_init(&sti_tmp);
            int sti_ok = menai_int_from_codepoints(
                menai_string_data(trimmed),
                menai_string_length(trimmed),
                (int)radix, &sti_tmp);
            Py_DECREF(trimmed);
            if (sti_ok < 0) {
                PyErr_Clear();
                reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                PyObject *_r = menai_integer_from_bigint(sti_tmp);
                if (_r == NULL) goto error;
                reg_set_own(regs, base + dest, _r);
            }
            break;
        }

        case OP_STRING_TO_NUMBER: {
            PyObject *a = regs[base + src0];
            if (!require_string(a, "string->number")) goto error;
            /* Scan codepoints directly to classify the string. */
            Py_ssize_t slen = menai_string_length(a);
            const uint32_t *sdata = menai_string_data(a);
            int has_j = 0, has_dot = 0, has_e = 0;
            for (Py_ssize_t _i = 0; _i < slen; _i++) {
                uint32_t _cp = sdata[_i];
                if (_cp == 'j' || _cp == 'J') has_j = 1;
                else if (_cp == '.') has_dot = 1;
                else if (_cp == 'e' || _cp == 'E') has_e = 1;
            }
            if (!has_dot && !has_e && !has_j) {
                /* Try integer parse directly on the codepoint array. */
                MenaiInt stn_tmp;
                menai_int_init(&stn_tmp);
                if (menai_int_from_codepoints(sdata, slen, 10, &stn_tmp) == 0) {
                    PyObject *r = menai_integer_from_bigint(stn_tmp);
                    if (r == NULL) goto error;
                    reg_set_own(regs, base + dest, r);
                    break;
                }
                PyErr_Clear();
            }
            if (has_j) {
                /*
                 * Complex literal parse — still uses Python's complex()
                 * constructor as a C-native complex literal parser would be
                 * non-trivial to implement correctly.
                 */
                PyObject *sa_j = menai_string_to_pyunicode(a);
                if (sa_j == NULL) goto error;
                PyObject *cplx = PyObject_CallOneArg((PyObject *)&PyComplex_Type, sa_j);
                Py_DECREF(sa_j);
                if (cplx != NULL) {
                    PyObject *r = make_complex(PyComplex_RealAsDouble(cplx),
                                               PyComplex_ImagAsDouble(cplx));
                    Py_DECREF(cplx);
                    if (r == NULL) goto error;
                    reg_set_own(regs, base + dest, r);
                    break;
                }
                PyErr_Clear();
            }
            {
                /* Float parse via strtod on a temporary UTF-8 buffer.
                 * Valid float literals are ASCII-only so this is safe. */
                char *stn_fbuf = (char *)PyMem_Malloc((size_t)(slen + 1));
                if (!stn_fbuf) { PyErr_NoMemory(); goto error; }
                int stn_ascii_ok = 1;
                for (Py_ssize_t _i = 0; _i < slen; _i++) {
                    if (sdata[_i] > 0x7F) { stn_ascii_ok = 0; break; }
                    stn_fbuf[_i] = (char)sdata[_i];
                }
                stn_fbuf[slen] = '\0';
                if (!stn_ascii_ok) {
                    PyMem_Free(stn_fbuf);
                    reg_set_borrow(regs, base + dest, Menai_NONE);
                    break;
                }
                char *stn_end = NULL;
                double stn_dv = strtod(stn_fbuf, &stn_end);
                int stn_ok = (stn_end != stn_fbuf && *stn_end == '\0');
                PyMem_Free(stn_fbuf);
                if (stn_ok) {
                    PyObject *_r = make_float(stn_dv);
                    if (_r == NULL) goto error;
                    reg_set_own(regs, base + dest, _r);
                } else {
                    reg_set_borrow(regs, base + dest, Menai_NONE);
                }
            }
            break;
        }

        case OP_STRING_TO_LIST: {
            /* src0=string, src1=delimiter string */
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_string(a, "string->list")) goto error;
            if (!require_string(b, "string->list")) goto error;
            Py_ssize_t alen = menai_string_length(a);
            Py_ssize_t blen = menai_string_length(b);
            const uint32_t *adata = menai_string_data(a);
            const uint32_t *bdata = menai_string_data(b);
            PyObject *r;
            if (blen == 0) {
                /* Split into individual codepoints */
                PyObject **stl_arr = alen > 0
                    ? (PyObject **)PyMem_Malloc(alen * sizeof(PyObject *)) : NULL;
                if (alen > 0 && !stl_arr) {
                    PyErr_NoMemory();
                    goto error;
                }
                for (Py_ssize_t i = 0; i < alen; i++) {
                    stl_arr[i] = menai_string_from_codepoint(adata[i]);
                    if (!stl_arr[i]) {
                        for (Py_ssize_t k = 0; k < i; k++) Py_DECREF(stl_arr[k]);
                        PyMem_Free(stl_arr);
                        goto error;
                    }
                }
                r = menai_list_from_array_steal(stl_arr, alen);
            } else {
                /* Split on delimiter — find occurrences and build list */
                Py_ssize_t count = 0;
                for (Py_ssize_t i = 0; i <= alen - blen; ) {
                    if (memcmp(adata + i, bdata, (size_t)blen * sizeof(uint32_t)) == 0) {
                        count++;
                        i += blen;
                    } else {
                        i++;
                    }
                }
                Py_ssize_t nparts = count + 1;
                PyObject **parts2 = (PyObject **)PyMem_Malloc(nparts * sizeof(PyObject *));
                if (!parts2) {
                    PyErr_NoMemory();
                    goto error;
                }
                Py_ssize_t seg_start = 0, pi2 = 0;
                for (Py_ssize_t i = 0; i <= alen; ) {
                    int match = (i <= alen - blen) &&
                        (memcmp(adata + i, bdata, (size_t)blen * sizeof(uint32_t)) == 0);
                    if (match || i == alen) {
                        parts2[pi2] = menai_string_from_codepoints(adata + seg_start, i - seg_start);
                        if (!parts2[pi2]) {
                            for (Py_ssize_t k = 0; k < pi2; k++) Py_DECREF(parts2[k]);
                            PyMem_Free(parts2);
                            goto error;
                        }
                        pi2++;
                        if (match) { seg_start = i + blen; i += blen; }
                        else break;
                    } else {
                        i++;
                    }
                }
                r = menai_list_from_array_steal(parts2, pi2);
            }
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
            int eq = menai_value_equal(a, b);
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_LIST_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_list(a, "list!=?")) goto error;
            if (!require_list(b, "list!=?")) goto error;
            int eq = menai_value_equal(a, b);
            bool_store(regs, base + dest, !eq);
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
            PyObject *_r = make_integer_from_ssize_t(n);
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
            if (((MenaiList_Object *)a)->length == 0) {
                menai_raise_eval_error("Function 'list-rest' requires a non-empty list");
                goto error;
            }
            PyObject *r = menai_list_rest(a);
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
            MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
            long idx_l;
            if (!ib->is_big) { idx_l = ib->small; } else { if (menai_int_to_long(&ib->big, &idx_l) < 0) goto error; }
            Py_ssize_t idx = (Py_ssize_t)idx_l;
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
            if (!pre_arr) {
                PyErr_NoMemory();
                goto error;
            }
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
            if (!app_arr) {
                PyErr_NoMemory();
                goto error;
            }
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
            if (n > 0 && !rev_arr) {
                PyErr_NoMemory();
                goto error;
            }
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
            if (nc > 0 && !cat_arr) {
                PyErr_NoMemory();
                goto error;
            }
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
                int eq = menai_value_equal(lst_mem->elements[i], item);
                if (eq) {
                    mem_found = 1;
                    break;
                }
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
                int eq = menai_value_equal(lst_idx->elements[i], item);
                if (eq) {
                    found = i;
                    break;
                }
            }
            if (found == -1) {
                reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                PyObject *_r = make_integer_from_ssize_t(found);
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
            MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
            MenaiInteger_Object *ic = (MenaiInteger_Object *)c;
            long start_l, end_l;
            if (!ib->is_big) { start_l = ib->small; } else { if (menai_int_to_long(&ib->big, &start_l) < 0) goto error; }
            if (!ic->is_big) { end_l = ic->small; } else { if (menai_int_to_long(&ic->big, &end_l) < 0) goto error; }
            Py_ssize_t start = (Py_ssize_t)start_l, end = (Py_ssize_t)end_l;
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
            PyObject *r = menai_list_slice(a, start, end);
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
                int eq = menai_value_equal(lst_rm->elements[i], item);
                if (!eq) keep++;
            }
            PyObject **rm_arr = keep > 0
                ? (PyObject **)PyMem_Malloc(keep * sizeof(PyObject *)) : NULL;
            if (keep > 0 && !rm_arr) {
                PyErr_NoMemory();
                goto error;
            }
            Py_ssize_t j = 0;
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *e = lst_rm->elements[i];
                int eq = menai_value_equal(e, item);
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
            Py_ssize_t n = lst_ts->length;
            /* Validate all elements are strings first. */
            for (Py_ssize_t i = 0; i < n; i++) {
                if (!IS_MENAI_STRING(lst_ts->elements[i])) {
                    menai_raise_eval_error("list->string: all elements must be strings");
                    goto error;
                }
            }
            /* Compute total output length. */
            Py_ssize_t sep_len = menai_string_length(b);
            const uint32_t *sep_data = menai_string_data(b);
            Py_ssize_t total = (n > 0) ? (n - 1) * sep_len : 0;
            for (Py_ssize_t i = 0; i < n; i++)
                total += menai_string_length(lst_ts->elements[i]);
            MenaiString_Object *obj = (MenaiString_Object *)PyObject_NewVar(
                MenaiString_Object, &MenaiString_Type, total);
            if (!obj) goto error;
            obj->hash = -1;
            uint32_t *dst = obj->data;
            for (Py_ssize_t i = 0; i < n; i++) {
                if (i > 0 && sep_len > 0) {
                    memcpy(dst, sep_data, (size_t)sep_len * sizeof(uint32_t));
                    dst += sep_len;
                }
                Py_ssize_t elen = menai_string_length(lst_ts->elements[i]);
                if (elen > 0) {
                    memcpy(dst, menai_string_data(lst_ts->elements[i]),
                           (size_t)elen * sizeof(uint32_t));
                    dst += elen;
                }
            }
            PyObject *r = (PyObject *)obj;
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_TO_SET: {
            PyObject *a = regs[base + src0];
            if (!require_list_singular(a, "list->set")) goto error;
            MenaiList_Object *lst = (MenaiList_Object *)a;
            Py_ssize_t n = lst->length;
            PyObject **nelems = n > 0 ? (PyObject **)PyMem_Malloc(n * sizeof(PyObject *)) : NULL;
            Py_hash_t *nhashes = n > 0 ? (Py_hash_t *)PyMem_Malloc(n * sizeof(Py_hash_t)) : NULL;
            if (n > 0 && (!nelems || !nhashes)) {
                PyMem_Free(nelems);
                PyMem_Free(nhashes);
                PyErr_NoMemory();
                goto error;
            }
            MenaiHashTable lts_seen;
            int lts_err = 0;
            if (n > 0 && menai_ht_init(&lts_seen, n) < 0) {
                PyMem_Free(nelems);
                PyMem_Free(nhashes);
                goto error;
            }
            Py_ssize_t out = 0;
            for (Py_ssize_t i = 0; i < n && !lts_err; i++) {
                PyObject *elem = lst->elements[i];
                Py_hash_t h = menai_value_hash(elem);
                if (h == -1) {
                    lts_err = 1;
                    break;
                }
                Py_ssize_t existing = menai_ht_lookup(&lts_seen, elem, h);
                if (existing == -2) {
                    lts_err = 1;
                    break;
                }
                if (existing < 0) {
                    menai_ht_insert(&lts_seen, elem, h, out);
                    Py_INCREF(elem);
                    nelems[out] = elem;
                    nhashes[out] = h;
                    out++;
                }
            }
            if (n > 0) menai_ht_free(&lts_seen);
            if (lts_err) {
                for (Py_ssize_t k = 0; k < out; k++) Py_DECREF(nelems[k]);
                PyMem_Free(nelems);
                PyMem_Free(nhashes);
                goto error;
            }
            PyObject *r = menai_set_from_arrays_steal(nelems, nhashes, out);
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
            MenaiDict_Object *da = (MenaiDict_Object *)a;
            MenaiDict_Object *db = (MenaiDict_Object *)b;
            int eq = (da->length == db->length);
            for (Py_ssize_t i = 0; eq && i < da->length; i++) {
                if (da->hashes[i] != db->hashes[i]) {
                    eq = 0;
                    break;
                }
                int keq = menai_value_equal(da->keys[i], db->keys[i]);
                if (!keq) {
                    eq = 0;
                    break;
                }
                int veq = menai_value_equal(da->values[i], db->values[i]);
                if (!veq) {
                    eq = 0;
                    break;
                }
            }
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_DICT_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_dict(a, "dict!=?")) goto error;
            if (!require_dict(b, "dict!=?")) goto error;
            MenaiDict_Object *da = (MenaiDict_Object *)a;
            MenaiDict_Object *db = (MenaiDict_Object *)b;
            int neq = (da->length != db->length);
            for (Py_ssize_t i = 0; !neq && i < da->length; i++) {
                if (da->hashes[i] != db->hashes[i]) {
                    neq = 1;
                    break;
                }
                int keq = menai_value_equal(da->keys[i], db->keys[i]);
                if (!keq) {
                    neq = 1;
                    break;
                }
                int veq = menai_value_equal(da->values[i], db->values[i]);
                if (!veq) {
                    neq = 1;
                    break;
                }
            }
            bool_store(regs, base + dest, neq);
            break;
        }

        case OP_DICT_LENGTH: {
            PyObject *a = regs[base + src0];
            if (!require_dict(a, "dict-length")) goto error;
            PyObject *_r = make_integer_from_ssize_t(((MenaiDict_Object *)a)->length);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_DICT_KEYS: {
            PyObject *a = regs[base + src0];
            if (!require_dict(a, "dict-keys")) goto error;
            MenaiDict_Object *d = (MenaiDict_Object *)a;
            Py_ssize_t n = d->length;
            PyObject **dk_arr = n > 0
                ? (PyObject **)PyMem_Malloc(n * sizeof(PyObject *)) : NULL;
            if (n > 0 && !dk_arr) {
                PyErr_NoMemory();
                goto error;
            }
            for (Py_ssize_t i = 0; i < n; i++) {
                Py_INCREF(d->keys[i]);
                dk_arr[i] = d->keys[i];
            }
            PyObject *r = menai_list_from_array_steal(dk_arr, n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_VALUES: {
            PyObject *a = regs[base + src0];
            if (!require_dict(a, "dict-values")) goto error;
            MenaiDict_Object *d = (MenaiDict_Object *)a;
            Py_ssize_t n = d->length;
            PyObject **dv_arr = n > 0
                ? (PyObject **)PyMem_Malloc(n * sizeof(PyObject *)) : NULL;
            if (n > 0 && !dv_arr) {
                PyErr_NoMemory();
                goto error;
            }
            for (Py_ssize_t i = 0; i < n; i++) {
                Py_INCREF(d->values[i]);
                dv_arr[i] = d->values[i];
            }
            PyObject *r = menai_list_from_array_steal(dv_arr, n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_HAS_P: {
            PyObject *a = regs[base + src0], *key = regs[base + src1];
            if (!require_dict(a, "dict-has?")) goto error;
            MenaiDict_Object *d = (MenaiDict_Object *)a;
            Py_hash_t h = menai_value_hash(key);
            if (h == -1) goto error;
            int has = (menai_ht_lookup(&d->ht, key, h) >= 0);
            bool_store(regs, base + dest, has);
            break;
        }

        case OP_DICT_GET: {
            /* src0=dict, src1=key, src2=default */
            PyObject *a = regs[base + src0], *key = regs[base + src1], *def = regs[base + src2];
            if (!require_dict(a, "dict-get")) goto error;
            MenaiDict_Object *d = (MenaiDict_Object *)a;
            Py_hash_t h = menai_value_hash(key);
            if (h == -1) goto error;
            Py_ssize_t idx = menai_ht_lookup(&d->ht, key, h);
            if (idx == -2) goto error;
            if (idx >= 0) {
                reg_set_borrow(regs, base + dest, d->values[idx]);
            } else {
                reg_set_borrow(regs, base + dest, def);
            }
            break;
        }

        case OP_DICT_SET: {
            /* src0=dict, src1=key, src2=value */
            PyObject *a = regs[base + src0], *key = regs[base + src1], *val = regs[base + src2];
            if (!require_dict(a, "dict-set")) goto error;
            MenaiDict_Object *d = (MenaiDict_Object *)a;
            Py_hash_t h = menai_value_hash(key);
            if (h == -1) goto error;
            Py_ssize_t replace_idx = menai_ht_lookup(&d->ht, key, h);
            if (replace_idx == -2) goto error;
            Py_ssize_t n = d->length;
            Py_ssize_t new_n = (replace_idx >= 0) ? n : n + 1;
            PyObject **nkeys = (PyObject **)PyMem_Malloc(new_n * sizeof(PyObject *));
            PyObject **nvals = (PyObject **)PyMem_Malloc(new_n * sizeof(PyObject *));
            Py_hash_t *nhashes = (Py_hash_t *)PyMem_Malloc(new_n * sizeof(Py_hash_t));
            if (!nkeys || !nvals || !nhashes) {
                PyMem_Free(nkeys);
                PyMem_Free(nvals);
                PyMem_Free(nhashes);
                PyErr_NoMemory();
                goto error;
            }
            if (replace_idx >= 0) {
                for (Py_ssize_t i = 0; i < n; i++) {
                    if (i == replace_idx) {
                        Py_INCREF(key);
                        nkeys[i] = key;
                        Py_INCREF(val);
                        nvals[i] = val;
                        nhashes[i] = h;
                    } else {
                        Py_INCREF(d->keys[i]);
                        nkeys[i] = d->keys[i];
                        Py_INCREF(d->values[i]);
                        nvals[i] = d->values[i];
                        nhashes[i] = d->hashes[i];
                    }
                }
            } else {
                for (Py_ssize_t i = 0; i < n; i++) {
                    Py_INCREF(d->keys[i]);
                    nkeys[i] = d->keys[i];
                    Py_INCREF(d->values[i]);
                    nvals[i] = d->values[i];
                    nhashes[i] = d->hashes[i];
                }
                Py_INCREF(key);
                nkeys[n] = key;
                Py_INCREF(val);
                nvals[n] = val;
                nhashes[n] = h;
            }
            PyObject *result = menai_dict_from_arrays_steal(nkeys, nvals, nhashes, new_n);
            if (result == NULL) goto error;
            reg_set_own(regs, base + dest, result);
            break;
        }

        case OP_DICT_REMOVE: {
            PyObject *a = regs[base + src0], *key = regs[base + src1];
            if (!require_dict(a, "dict-remove")) goto error;
            MenaiDict_Object *d = (MenaiDict_Object *)a;
            Py_hash_t h = menai_value_hash(key);
            if (h == -1) goto error;
            Py_ssize_t remove_idx = menai_ht_lookup(&d->ht, key, h);
            if (remove_idx == -2) goto error;
            if (remove_idx < 0) {
                reg_set_borrow(regs, base + dest, a);
                break;
            }
            Py_ssize_t n = d->length;
            Py_ssize_t new_n = n - 1;
            PyObject **nkeys = new_n > 0 ? (PyObject **)PyMem_Malloc(new_n * sizeof(PyObject *)) : NULL;
            PyObject **nvals = new_n > 0 ? (PyObject **)PyMem_Malloc(new_n * sizeof(PyObject *)) : NULL;
            Py_hash_t *nhashes = new_n > 0 ? (Py_hash_t *)PyMem_Malloc(new_n * sizeof(Py_hash_t)) : NULL;
            if (new_n > 0 && (!nkeys || !nvals || !nhashes)) {
                PyMem_Free(nkeys);
                PyMem_Free(nvals);
                PyMem_Free(nhashes);
                PyErr_NoMemory();
                goto error;
            }
            for (Py_ssize_t i = 0, j = 0; i < n; i++) {
                if (i == remove_idx) continue;
                Py_INCREF(d->keys[i]);
                nkeys[j] = d->keys[i];
                Py_INCREF(d->values[i]);
                nvals[j] = d->values[i];
                nhashes[j] = d->hashes[i];
                j++;
            }
            PyObject *r = menai_dict_from_arrays_steal(nkeys, nvals, nhashes, new_n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_MERGE: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_dict(a, "dict-merge")) goto error;
            if (!require_dict(b, "dict-merge")) goto error;
            MenaiDict_Object *da = (MenaiDict_Object *)a;
            MenaiDict_Object *db = (MenaiDict_Object *)b;
            Py_ssize_t na = da->length, nb = db->length;
            Py_ssize_t cap = na + nb;
            PyObject **nkeys = cap > 0 ? (PyObject **)PyMem_Malloc(cap * sizeof(PyObject *)) : NULL;
            PyObject **nvals = cap > 0 ? (PyObject **)PyMem_Malloc(cap * sizeof(PyObject *)) : NULL;
            Py_hash_t *nhashes = cap > 0 ? (Py_hash_t *)PyMem_Malloc(cap * sizeof(Py_hash_t)) : NULL;
            if (cap > 0 && (!nkeys || !nvals || !nhashes)) {
                PyMem_Free(nkeys);
                PyMem_Free(nvals);
                PyMem_Free(nhashes);
                PyErr_NoMemory();
                goto error;
            }
            Py_ssize_t out = 0;
            /* Add a's entries, using b's value where b overrides */
            for (Py_ssize_t i = 0; i < na; i++) {
                Py_ssize_t bi = menai_ht_lookup(&db->ht, da->keys[i], da->hashes[i]);
                if (bi == -2) {
                    for (Py_ssize_t k = 0; k < out; k++) {
                        Py_DECREF(nkeys[k]);
                        Py_DECREF(nvals[k]);
                    }
                    PyMem_Free(nkeys);
                    PyMem_Free(nvals);
                    PyMem_Free(nhashes);
                    goto error;
                }
                Py_INCREF(da->keys[i]);
                nkeys[out] = da->keys[i];
                nhashes[out] = da->hashes[i];
                if (bi >= 0) {
                    Py_INCREF(db->values[bi]);
                    nvals[out] = db->values[bi];
                } else {
                    Py_INCREF(da->values[i]);
                    nvals[out] = da->values[i];
                }
                out++;
            }
            /* Add b's entries not in a */
            for (Py_ssize_t i = 0; i < nb; i++) {
                Py_ssize_t ai = menai_ht_lookup(&da->ht, db->keys[i], db->hashes[i]);
                if (ai == -2) {
                    for (Py_ssize_t k = 0; k < out; k++) {
                        Py_DECREF(nkeys[k]);
                        Py_DECREF(nvals[k]);
                    }
                    PyMem_Free(nkeys);
                    PyMem_Free(nvals);
                    PyMem_Free(nhashes);
                    goto error;
                }
                if (ai < 0) {
                    Py_INCREF(db->keys[i]);
                    nkeys[out] = db->keys[i];
                    Py_INCREF(db->values[i]);
                    nvals[out] = db->values[i];
                    nhashes[out] = db->hashes[i];
                    out++;
                }
            }
            PyObject *r = menai_dict_from_arrays_steal(nkeys, nvals, nhashes, out);
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
            MenaiSet_Object *sa = (MenaiSet_Object *)a;
            MenaiSet_Object *sb = (MenaiSet_Object *)b;
            int eq = (sa->length == sb->length);
            for (Py_ssize_t i = 0; eq && i < sa->length; i++) {
                Py_ssize_t idx = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (idx == -2) goto error;
                if (idx < 0) {
                    eq = 0;
                    break;
                }
            }
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_SET_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set!=?")) goto error;
            if (!require_set(b, "set!=?")) goto error;
            MenaiSet_Object *sa = (MenaiSet_Object *)a;
            MenaiSet_Object *sb = (MenaiSet_Object *)b;
            int neq = (sa->length != sb->length);
            for (Py_ssize_t i = 0; !neq && i < sa->length; i++) {
                Py_ssize_t idx = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (idx == -2) goto error;
                if (idx < 0) {
                    neq = 1;
                    break;
                }
            }
            bool_store(regs, base + dest, neq);
            break;
        }

        case OP_SET_LENGTH: {
            PyObject *a = regs[base + src0];
            if (!require_set_singular(a, "set-length")) goto error;
            PyObject *_r = make_integer_from_ssize_t(((MenaiSet_Object *)a)->length);
            if (_r == NULL) goto error;
            reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_SET_MEMBER_P: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_set_singular(a, "set-member?")) goto error;
            MenaiSet_Object *s = (MenaiSet_Object *)a;
            Py_hash_t h = menai_value_hash(item);
            if (h == -1) goto error;
            Py_ssize_t idx = menai_ht_lookup(&s->ht, item, h);
            if (idx == -2) goto error;
            bool_store(regs, base + dest, idx >= 0);
            break;
        }

        case OP_SET_ADD: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_set_singular(a, "set-add")) goto error;
            MenaiSet_Object *s = (MenaiSet_Object *)a;
            Py_hash_t h = menai_value_hash(item);
            if (h == -1) goto error;
            Py_ssize_t existing = menai_ht_lookup(&s->ht, item, h);
            if (existing == -2) goto error;
            if (existing >= 0) {
                reg_set_borrow(regs, base + dest, a);
            } else {
                Py_ssize_t n = s->length;
                PyObject **nelems = (PyObject **)PyMem_Malloc((n + 1) * sizeof(PyObject *));
                Py_hash_t *nhashes = (Py_hash_t *)PyMem_Malloc((n + 1) * sizeof(Py_hash_t));
                if (!nelems || !nhashes) {
                    PyMem_Free(nelems);
                    PyMem_Free(nhashes);
                    PyErr_NoMemory();
                    goto error;
                }
                for (Py_ssize_t i = 0; i < n; i++) {
                    Py_INCREF(s->elements[i]);
                    nelems[i] = s->elements[i];
                    nhashes[i] = s->hashes[i];
                }
                Py_INCREF(item);
                nelems[n] = item;
                nhashes[n] = h;
                PyObject *r = menai_set_from_arrays_steal(nelems, nhashes, n + 1);
                if (r == NULL) goto error;
                reg_set_own(regs, base + dest, r);
            }
            break;
        }

        case OP_SET_REMOVE: {
            PyObject *a = regs[base + src0], *item = regs[base + src1];
            if (!require_set_singular(a, "set-remove")) goto error;
            MenaiSet_Object *s = (MenaiSet_Object *)a;
            Py_hash_t h = menai_value_hash(item);
            if (h == -1) goto error;
            Py_ssize_t remove_idx = menai_ht_lookup(&s->ht, item, h);
            if (remove_idx == -2) goto error;
            if (remove_idx < 0) {
                reg_set_borrow(regs, base + dest, a);
                break;
            }
            Py_ssize_t n = s->length;
            Py_ssize_t new_n = n - 1;
            PyObject **nelems = new_n > 0 ? (PyObject **)PyMem_Malloc(new_n * sizeof(PyObject *)) : NULL;
            Py_hash_t *nhashes = new_n > 0 ? (Py_hash_t *)PyMem_Malloc(new_n * sizeof(Py_hash_t)) : NULL;
            if (new_n > 0 && (!nelems || !nhashes)) {
                PyMem_Free(nelems);
                PyMem_Free(nhashes);
                PyErr_NoMemory();
                goto error;
            }
            for (Py_ssize_t i = 0, j = 0; i < n; i++) {
                if (i == remove_idx) continue;
                Py_INCREF(s->elements[i]);
                nelems[j] = s->elements[i];
                nhashes[j] = s->hashes[i];
                j++;
            }
            PyObject *r = menai_set_from_arrays_steal(nelems, nhashes, new_n);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_UNION: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-union")) goto error;
            if (!require_set(b, "set-union")) goto error;
            MenaiSet_Object *sa = (MenaiSet_Object *)a;
            MenaiSet_Object *sb = (MenaiSet_Object *)b;
            Py_ssize_t na = sa->length, nb = sb->length;
            Py_ssize_t cap = na + nb;
            PyObject **nelems = cap > 0 ? (PyObject **)PyMem_Malloc(cap * sizeof(PyObject *)) : NULL;
            Py_hash_t *nhashes = cap > 0 ? (Py_hash_t *)PyMem_Malloc(cap * sizeof(Py_hash_t)) : NULL;
            if (cap > 0 && (!nelems || !nhashes)) {
                PyMem_Free(nelems);
                PyMem_Free(nhashes);
                PyErr_NoMemory();
                goto error;
            }
            Py_ssize_t out = 0;
            for (Py_ssize_t i = 0; i < na; i++) {
                Py_INCREF(sa->elements[i]);
                nelems[out] = sa->elements[i];
                nhashes[out] = sa->hashes[i];
                out++;
            }
            for (Py_ssize_t i = 0; i < nb; i++) {
                Py_ssize_t in_a = menai_ht_lookup(&sa->ht, sb->elements[i], sb->hashes[i]);
                if (in_a == -2) {
                    for (Py_ssize_t k = 0; k < out; k++) {
                        Py_DECREF(nelems[k]);
                    }
                    PyMem_Free(nelems);
                    PyMem_Free(nhashes);
                    goto error;
                }
                if (in_a < 0) {
                    Py_INCREF(sb->elements[i]);
                    nelems[out] = sb->elements[i];
                    nhashes[out] = sb->hashes[i];
                    out++;
                }
            }
            PyObject *r = menai_set_from_arrays_steal(nelems, nhashes, out);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_INTERSECTION: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-intersection")) goto error;
            if (!require_set(b, "set-intersection")) goto error;
            MenaiSet_Object *sa = (MenaiSet_Object *)a;
            MenaiSet_Object *sb = (MenaiSet_Object *)b;
            Py_ssize_t na = sa->length;
            PyObject **nelems = na > 0 ? (PyObject **)PyMem_Malloc(na * sizeof(PyObject *)) : NULL;
            Py_hash_t *nhashes = na > 0 ? (Py_hash_t *)PyMem_Malloc(na * sizeof(Py_hash_t)) : NULL;
            if (na > 0 && (!nelems || !nhashes)) {
                PyMem_Free(nelems);
                PyMem_Free(nhashes);
                PyErr_NoMemory();
                goto error;
            }
            Py_ssize_t out = 0;
            for (Py_ssize_t i = 0; i < na; i++) {
                Py_ssize_t in_b = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (in_b == -2) {
                    for (Py_ssize_t k = 0; k < out; k++) {
                        Py_DECREF(nelems[k]);
                    }
                    PyMem_Free(nelems);
                    PyMem_Free(nhashes);
                    goto error;
                }
                if (in_b >= 0) {
                    Py_INCREF(sa->elements[i]);
                    nelems[out] = sa->elements[i];
                    nhashes[out] = sa->hashes[i];
                    out++;
                }
            }
            PyObject *r = menai_set_from_arrays_steal(nelems, nhashes, out);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_DIFFERENCE: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-difference")) goto error;
            if (!require_set(b, "set-difference")) goto error;
            MenaiSet_Object *sa = (MenaiSet_Object *)a;
            MenaiSet_Object *sb = (MenaiSet_Object *)b;
            Py_ssize_t na = sa->length;
            PyObject **nelems = na > 0 ? (PyObject **)PyMem_Malloc(na * sizeof(PyObject *)) : NULL;
            Py_hash_t *nhashes = na > 0 ? (Py_hash_t *)PyMem_Malloc(na * sizeof(Py_hash_t)) : NULL;
            if (na > 0 && (!nelems || !nhashes)) {
                PyMem_Free(nelems);
                PyMem_Free(nhashes);
                PyErr_NoMemory();
                goto error;
            }
            Py_ssize_t out = 0;
            for (Py_ssize_t i = 0; i < na; i++) {
                Py_ssize_t in_b = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (in_b == -2) {
                    for (Py_ssize_t k = 0; k < out; k++) {
                        Py_DECREF(nelems[k]);
                    }
                    PyMem_Free(nelems);
                    PyMem_Free(nhashes);
                    goto error;
                }
                if (in_b < 0) {
                    Py_INCREF(sa->elements[i]); nelems[out] = sa->elements[i];
                    nhashes[out] = sa->hashes[i];
                    out++;
                }
            }
            PyObject *r = menai_set_from_arrays_steal(nelems, nhashes, out);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_SUBSET_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_set(a, "set-subset?")) goto error;
            if (!require_set(b, "set-subset?")) goto error;
            MenaiSet_Object *sa = (MenaiSet_Object *)a;
            MenaiSet_Object *sb = (MenaiSet_Object *)b;
            if (sa->length > sb->length) {
                bool_store(regs, base + dest, 0);
                break;
            }
            int is_subset = 1;
            for (Py_ssize_t i = 0; i < sa->length; i++) {
                Py_ssize_t idx = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (idx == -2) goto error;
                if (idx < 0) {
                    is_subset = 0;
                    break;
                }
            }
            bool_store(regs, base + dest, is_subset);
            break;
        }

        case OP_SET_TO_LIST: {
            PyObject *a = regs[base + src0];
            if (!require_set_singular(a, "set->list")) goto error;
            MenaiSet_Object *s = (MenaiSet_Object *)a;
            Py_ssize_t set_n = s->length;
            PyObject **stl_arr = set_n > 0
                ? (PyObject **)PyMem_Malloc(set_n * sizeof(PyObject *)) : NULL;
            if (set_n > 0 && !stl_arr) {
                PyErr_NoMemory();
                goto error;
            }
            for (Py_ssize_t i = 0; i < set_n; i++) {
                Py_INCREF(s->elements[i]);
                stl_arr[i] = s->elements[i];
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
            MenaiInteger_Object *ia = (MenaiInteger_Object *)ra;
            MenaiInteger_Object *ib = (MenaiInteger_Object *)rb;
            MenaiInteger_Object *ic = (MenaiInteger_Object *)rc;
            long start, end, step;
            if (!ia->is_big) { start = ia->small; } else { if (menai_int_to_long(&ia->big, &start) < 0) goto error; }
            if (!ib->is_big) { end = ib->small; } else { if (menai_int_to_long(&ib->big, &end) < 0) goto error; }
            if (!ic->is_big) { step = ic->small; } else { if (menai_int_to_long(&ic->big, &step) < 0) goto error; }
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
            if (n > 0 && !rng_arr) {
                PyErr_NoMemory();
                goto error;
            }
            long val = start;
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *mi = make_integer_from_long(val);
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
            PyObject *instance = menai_struct_alloc(struct_type, &regs[base + src0 + 1], n_fields);
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
            int fi = menai_struct_field_index((MenaiStructType_Object *)stype, name);
            if (fi < 0) {
                menai_raise_eval_errorf(
                    "'struct-get': struct '%s' has no field '%s'",
                    PyUnicode_AsUTF8(((MenaiStructType_Object *)stype)->name),
                    PyUnicode_AsUTF8(name));
                goto error;
            }
            PyObject *fv = ((MenaiStruct_Object *)val)->items[fi];
            reg_set_borrow(regs, base + dest, fv);
            break;
        }

        case OP_STRUCT_GET_IMM: {
            /* src1 holds a MenaiInteger field index */
            PyObject *val = regs[base + src0], *fidx = regs[base + src1];
            if (!require_struct(val, "struct-get-imm")) goto error;
            if (!require_integer(fidx, "struct-get-imm")) goto error;
            MenaiInteger_Object *fi_io = (MenaiInteger_Object *)fidx;
            long fi_l;
            if (!fi_io->is_big) { fi_l = fi_io->small; } else { if (menai_int_to_long(&fi_io->big, &fi_l) < 0) goto error; }
            Py_ssize_t fi = (Py_ssize_t)fi_l;
            PyObject *fv = ((MenaiStruct_Object *)val)->items[fi];
            reg_set_borrow(regs, base + dest, fv);
            break;
        }

        case OP_STRUCT_SET: {
            PyObject *val = regs[base + src0], *field_sym = regs[base + src1], *new_val = regs[base + src2];
            if (!require_struct(val, "struct-set")) goto error;
            if (!require_symbol(field_sym, "struct-set")) goto error;
            PyObject *stype = ((MenaiStruct_Object *)val)->struct_type;
            PyObject *name = menai_symbol_name(field_sym);
            int fi = menai_struct_field_index((MenaiStructType_Object *)stype, name);
            if (fi < 0) {
                menai_raise_eval_errorf(
                    "'struct-set': struct '%s' has no field '%s'",
                    PyUnicode_AsUTF8(((MenaiStructType_Object *)stype)->name),
                    PyUnicode_AsUTF8(name));
                goto error;
            }
            Py_ssize_t nf = Py_SIZE(val);
            PyObject **tmp = (PyObject **)PyMem_Malloc(nf * sizeof(PyObject *));
            if (!tmp) {
                PyErr_NoMemory();
                goto error;
            }

            for (Py_ssize_t i = 0; i < nf; i++) tmp[i] = (i == fi) ? new_val : ((MenaiStruct_Object *)val)->items[i];

            PyObject *r = menai_struct_alloc(stype, tmp, nf);
            PyMem_Free(tmp);
            if (r == NULL) goto error;

            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRUCT_SET_IMM: {
            PyObject *val = regs[base + src0], *fidx = regs[base + src1], *new_val = regs[base + src2];
            if (!require_struct(val, "struct-set-imm")) goto error;
            if (!require_integer(fidx, "struct-set-imm")) goto error;
            MenaiInteger_Object *fi_io = (MenaiInteger_Object *)fidx;
            long fi_l;
            if (!fi_io->is_big) { fi_l = fi_io->small; } else { if (menai_int_to_long(&fi_io->big, &fi_l) < 0) goto error; }
            Py_ssize_t fi = (Py_ssize_t)fi_l;
            PyObject *stype = ((MenaiStruct_Object *)val)->struct_type;
            Py_ssize_t nf = Py_SIZE(val);
            PyObject **tmp = (PyObject **)PyMem_Malloc(nf * sizeof(PyObject *));
            if (!tmp) {
                PyErr_NoMemory();
                goto error;
            }
            for (Py_ssize_t i = 0; i < nf; i++) {
                tmp[i] = (i == fi) ? new_val : ((MenaiStruct_Object *)val)->items[i];
            }
            PyObject *r = menai_struct_alloc(stype, tmp, nf);
            PyMem_Free(tmp);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRUCT_EQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_struct(a, "struct=?")) goto error;
            if (!require_struct(b, "struct=?")) goto error;
            MenaiStruct_Object *sa = (MenaiStruct_Object *)a;
            MenaiStruct_Object *sb = (MenaiStruct_Object *)b;
            int eq = (((MenaiStructType_Object *)sa->struct_type)->tag ==
                      ((MenaiStructType_Object *)sb->struct_type)->tag);
            Py_ssize_t nf = Py_SIZE(sa);
            for (Py_ssize_t i = 0; eq && i < nf; i++) {
                eq = menai_value_equal(sa->items[i], sb->items[i]);
            }
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_STRUCT_NEQ_P: {
            PyObject *a = regs[base + src0], *b = regs[base + src1];
            if (!require_struct(a, "struct!=?")) goto error;
            if (!require_struct(b, "struct!=?")) goto error;
            MenaiStruct_Object *sa = (MenaiStruct_Object *)a;
            MenaiStruct_Object *sb = (MenaiStruct_Object *)b;
            int neq = (((MenaiStructType_Object *)sa->struct_type)->tag !=
                       ((MenaiStructType_Object *)sb->struct_type)->tag);
            if (!neq) {
                Py_ssize_t nf = Py_SIZE(sa);
                for (Py_ssize_t i = 0; i < nf; i++) {
                    int eq = menai_value_equal(sa->items[i], sb->items[i]);
                    if (!eq) {
                        neq = 1;
                        break;
                    }
                }
            }
            bool_store(regs, base + dest, neq);
            break;
        }

        case OP_STRUCT_TYPE: {
            PyObject *val = regs[base + src0];
            if (!require_struct(val, "struct-type")) goto error;
            reg_set_borrow(regs, base + dest, ((MenaiStruct_Object *)val)->struct_type);
            break;
        }

        case OP_STRUCT_TYPE_NAME: {
            PyObject *val = regs[base + src0];
            if (!require_structtype(val, "struct-type-name")) goto error;
            PyObject *name = ((MenaiStructType_Object *)val)->name;
            PyObject *r = menai_string_from_pyunicode(name);
            if (r == NULL) goto error;
            reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRUCT_FIELDS: {
            PyObject *val = regs[base + src0];
            if (!require_structtype(val, "struct-fields")) goto error;
            MenaiStructType_Object *st = (MenaiStructType_Object *)val;
            int n = st->nfields;
            PyObject **sf_arr = n > 0
                ? (PyObject **)PyMem_Malloc(n * sizeof(PyObject *)) : NULL;
            if (n > 0 && !sf_arr) {
                PyErr_NoMemory();
                goto error;
            }
            for (int i = 0; i < n; i++) {
                PyObject *sym = menai_symbol_alloc(st->fields[i].name);
                if (sym == NULL) {
                    for (int k = 0; k < i; k++) Py_DECREF(sf_arr[k]);
                    PyMem_Free(sf_arr);
                    goto error;
                }
                sf_arr[i] = sym;
            }
            PyObject *r = menai_list_from_array_steal(sf_arr, (Py_ssize_t)n);
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

    if (!PyArg_ParseTuple(args, "OOO", &code, &constants_dict, &prelude_dict)) return NULL;

    /* Convert compiler-world constants in the code object tree to fast C types. */
    if (menai_convert_code_object(code) == NULL) return NULL;

    /* Build the globals table (constants + prelude), converting values to fast C types. */
    GlobalsTable globals;
    if (globals_build(&globals, constants_dict, prelude_dict) < 0)
        return NULL;

    /* Compute the register window size. */
    int max_locals = max_local_count(code);
    if (max_locals < 0) {
        globals_free(&globals);
        return NULL;
    }

    /* Also scan prelude functions for their max_local_count. */
    for (Py_ssize_t i = 0; i < globals.count; i++) {
        PyObject *val = globals.entries[i].value;
        if (IS_MENAI_FUNCTION(val)) {
            PyObject *bc = ((MenaiFunction_Object *)val)->bytecode;
            int n = max_local_count(bc);
            if (n < 0) {
                globals_free(&globals);
                return NULL;
            }
            if (n > max_locals)
                max_locals = n;
        }
    }

    /* Allocate the register array. */
    PyObject **regs = regs_alloc(MAX_FRAME_DEPTH, max_locals);
    if (regs == NULL) {
        globals_free(&globals);
        return NULL;
    }

    /* Run the VM. */
    PyObject *result = execute_loop(code, &globals, regs, max_locals);

    /* Clean up. */
    regs_free(regs, MAX_FRAME_DEPTH, max_locals);
    globals_free(&globals);

    if (result == NULL)
        return NULL;

    /* Convert fast C types back to compiler-world types. */
    PyObject *slow_result = menai_to_slow(result);
    Py_DECREF(result);
    return slow_result;
}

/*
 * Module definition
 */
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
