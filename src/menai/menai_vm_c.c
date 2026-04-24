/*
 * menai_vm_c.c — C implementation of the Menai VM execute loop.
 *
 * Exposes a single Python-callable function:
 *
 * menai_vm_c.execute(code, globals_dict) -> MenaiValue *
 *
 * The MenaiVM Python class in menai_vm.py calls this in place of its Python
 * execute loop when this extension is available.
 *
 * Build:
 *   python setup.py build_ext --inplace
 */
#include <math.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_value.h"
#include "menai_vm_hashtable.h"
#include "menai_vm_code.h"
#include "menai_vm_memory.h"
#include "menai_vm_bigint.h"
#include "menai_vm_alloc.h"
#include "menai_vm_none.h"
#include "menai_vm_boolean.h"
#include "menai_vm_float.h"
#include "menai_vm_complex.h"
#include "menai_vm_function.h"
#include "menai_vm_string.h"
#include "menai_vm_symbol.h"
#include "menai_vm_struct.h"
#include "menai_vm_integer.h"
#include "menai_vm_dict.h"
#include "menai_vm_list.h"
#include "menai_vm_set.h"
#include "menai_vm_bridge.h"
#include "menai_vm_format.h"

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

static inline mc_t
mc(double re, double im)
{
    mc_t z = {re, im};
    return z;
}

static inline int
mc_zero(mc_t z)
{
    return z.re == 0.0 && z.im == 0.0;
}

static inline mc_t
mc_mul(mc_t a, mc_t b)
{
    return mc(a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re);
}

static inline mc_t
mc_div(mc_t a, mc_t b)
{
    double d = b.re * b.re + b.im * b.im;
    return mc((a.re * b.re + a.im * b.im) / d, (a.im * b.re - a.re * b.im) / d);
}

static inline mc_t
mc_exp(mc_t z)
{
    double e = exp(z.re);
    return mc(e * cos(z.im), e * sin(z.im));
}

static inline mc_t
mc_log(mc_t z)
{
    return mc(log(hypot(z.re, z.im)), atan2(z.im, z.re));
}

static inline mc_t
mc_pow(mc_t a, mc_t b)
{
    return mc_zero(a) ? mc(0.0, 0.0) : mc_exp(mc_mul(b, mc_log(a)));
}

static inline mc_t
mc_sqrt(mc_t z)
{
    double r = hypot(z.re, z.im);
    double s = sqrt((r + z.re) / 2.0);
    double t = (z.im >= 0.0 ? 1.0 : -1.0) * sqrt((r - z.re) / 2.0);
    return mc(s, t);
}

static inline mc_t
mc_sin(mc_t z)
{
    return mc(sin(z.re) * cosh(z.im), cos(z.re) * sinh(z.im));
}

static inline mc_t
mc_cos(mc_t z)
{
    return mc(cos(z.re) * cosh(z.im), -sin(z.re) * sinh(z.im));
}

static inline mc_t
mc_tan(mc_t z)
{
    return mc_div(mc_sin(z), mc_cos(z));
}

static inline mc_t
mc_log10(mc_t z)
{
    mc_t l = mc_log(z);
    double s = 1.0 / log(10.0);
    return mc(l.re * s, l.im * s);
}

static inline mc_t
mc_logn(mc_t a, mc_t b)
{
    return mc_div(mc_log(a), mc_log(b)); 
}

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
static inline int
_menai_add_overflow(long a, long b, long *r) {
    unsigned long ua = (unsigned long)a, ub = (unsigned long)b;
    unsigned long ur = ua + ub;
    *r = (long)ur;
    return (a > 0 && b > 0 && *r < 0) || (a < 0 && b < 0 && *r > 0);
}

static inline int
_menai_sub_overflow(long a, long b, long *r) {
    unsigned long ua = (unsigned long)a, ub = (unsigned long)b;
    unsigned long ur = ua - ub;
    *r = (long)ur;
    return (b < 0 && a > 0 && *r < 0) || (b > 0 && a < 0 && *r > 0);
}

static inline int
_menai_mul_overflow(long a, long b, long *r) {
    /* Conservative: use double to detect overflow. */
    double d = (double)a * (double)b;
    *r = (long)((unsigned long)a * (unsigned long)b);
    return d > (double)LONG_MAX || d < (double)LONG_MIN;
}

#endif

/*
 * Limits
 */
#define MAX_FRAME_DEPTH 1024

/*
 * Cancellation check interval.
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
 * Singleton values fetched from menai_vm_bridge at init time.
 */
MenaiValue *Menai_NONE = NULL;
MenaiValue *Menai_TRUE = NULL;
MenaiValue *Menai_FALSE = NULL;
MenaiValue *Menai_EMPTY_LIST = NULL;
MenaiValue *Menai_EMPTY_DICT = NULL;
MenaiValue *Menai_EMPTY_SET = NULL;

/*
 * Module-level state fetched at init
 */
static PyObject *MenaiEvalError_type = NULL;
static PyObject *MenaiCancelledException_type = NULL;

/*
 * Fast type-check macros
 */
#define IS_MENAI_NONE(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_NONE)
#define IS_MENAI_BOOLEAN(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_BOOLEAN)
#define IS_MENAI_INTEGER(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_INTEGER)
#define IS_MENAI_FLOAT(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_FLOAT)
#define IS_MENAI_COMPLEX(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_COMPLEX)
#define IS_MENAI_STRING(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_STRING)
#define IS_MENAI_SYMBOL(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_SYMBOL)
#define IS_MENAI_LIST(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_LIST)
#define IS_MENAI_DICT(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_DICT)
#define IS_MENAI_SET(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_SET)
#define IS_MENAI_FUNCTION(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_FUNCTION)
#define IS_MENAI_STRUCTTYPE(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_STRUCTTYPE)
#define IS_MENAI_STRUCT(o) (((MenaiValue *)(o))->ob_type == MENAITYPE_STRUCT)


static inline int
menai_boolean_value(MenaiValue *o)
{
    return ((MenaiBoolean *)o)->value;
}

static inline double
menai_float_value(MenaiValue *o)
{
    return ((MenaiFloat *)o)->value;
}

static inline MenaiValue *
menai_symbol_name(MenaiValue *o)
{
    return ((MenaiSymbol *)o)->name;
}

/*
 * menai_integer_compare — compare two MenaiInteger objects using MenaiInt.
 *
 * Fast path for the common case where both are small (is_big == 0): plain C
 * comparison of the long values.  Falls back to menai_bigint_* for big integers.
 *
 * op must be one of Py_EQ, Py_NE, Py_LT, Py_GT, Py_LE, Py_GE.
 * Never fails.
 */
static inline int
menai_integer_compare(MenaiValue* a, MenaiValue* b, int op)
{
    MenaiInteger *ia = (MenaiInteger *)a;
    MenaiInteger *ib = (MenaiInteger *)b;
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

    const MenaiBigInt *ma = ia->is_big ? &ia->big : NULL;
    const MenaiBigInt *mb = ib->is_big ? &ib->big : NULL;
    MenaiBigInt tmp_a, tmp_b;
    menai_bigint_init(&tmp_a);
    menai_bigint_init(&tmp_b);
    if (!ia->is_big) {
        menai_bigint_from_long(ia->small, &tmp_a);
    }

    if (!ib->is_big) {
        menai_bigint_from_long(ib->small, &tmp_b);
    }

    const MenaiBigInt *pa = ia->is_big ? ma : &tmp_a;
    const MenaiBigInt *pb = ib->is_big ? mb : &tmp_b;
    int result;
    switch (op) {
    case Py_EQ: result = menai_bigint_eq(pa, pb); break;
    case Py_NE: result = menai_bigint_ne(pa, pb); break;
    case Py_LT: result = menai_bigint_lt(pa, pb); break;
    case Py_GT: result = menai_bigint_gt(pa, pb); break;
    case Py_LE: result = menai_bigint_le(pa, pb); break;
    case Py_GE: result = menai_bigint_ge(pa, pb); break;
    default: result = 0; break;
    }

    menai_bigint_free(&tmp_a);
    menai_bigint_free(&tmp_b);
    return result;
}

/*
 * make_integer_from_ssize_t — create a MenaiInteger from a ssize_t.
 *
 * ssize_t fits in a long on all supported platforms, so this is a direct
 * delegation to menai_integer_from_long.
 */
static inline MenaiValue *
make_integer_from_ssize_t(ssize_t n)
{
    return menai_integer_from_long((long)n);
}

static inline MenaiValue *make_integer_from_long(long n)
{
    return menai_integer_from_long(n);
}

static inline MenaiValue *make_float(double v)
{
    return menai_float_alloc(v);
}

static inline MenaiValue *make_complex(double real, double imag)
{
    return menai_complex_alloc(real, imag);
}

static inline void bool_store(MenaiValue **regs, int slot, int cond)
{
    menai_reg_set_borrow(regs, slot, cond ? Menai_TRUE : Menai_FALSE);
}

static PyObject *menai_raise_eval_error(const char *message);
static PyObject *menai_raise_eval_errorf(const char *fmt, ...);

static inline int
require_type_impl(int ok, MenaiValue *val, const char *op_name, const char *noun)
{
    if (ok) {
        return 1;
    }

    menai_raise_eval_errorf("Function '%s' requires %s, got %s", op_name, noun, menai_short_type_name(val->ob_type));
    return 0;
}

static inline int require_integer(MenaiValue *val, const char *op_name)
{
    return require_type_impl(IS_MENAI_INTEGER(val), val, op_name, "integer arguments");
}

static inline int require_float(MenaiValue* val, const char *op_name)
{
    return require_type_impl(IS_MENAI_FLOAT(val), val, op_name, "float arguments");
}

static inline int require_complex(MenaiValue* val, const char *op_name)
{
    return require_type_impl(IS_MENAI_COMPLEX(val), val, op_name, "complex arguments");
}

static inline int require_string(MenaiValue* val, const char *op_name)
{
    return require_type_impl(IS_MENAI_STRING(val), val, op_name, "string arguments");
}

static inline int require_list(MenaiValue* val, const char *op_name)
{
    return require_type_impl(IS_MENAI_LIST(val), val, op_name, "list arguments");
}

static inline int require_list_singular(MenaiValue *val, const char *op_name)
{
    return require_type_impl(IS_MENAI_LIST(val), val, op_name, "a list argument");
}

static inline int require_dict(MenaiValue *val, const char *op_name)
{
    return require_type_impl(IS_MENAI_DICT(val), val, op_name, "dict arguments");
}

static inline int require_set(MenaiValue *val, const char *op_name)
{
    return require_type_impl(IS_MENAI_SET(val), val, op_name, "set arguments");
}

static inline int require_set_singular(MenaiValue *val, const char *op_name)
{
    return require_type_impl(IS_MENAI_SET(val), val, op_name, "a set argument");
}

static inline int require_boolean(MenaiValue *val, const char *op_name)
{
    return require_type_impl(IS_MENAI_BOOLEAN(val), val, op_name, "boolean arguments");
}

static inline int require_function(MenaiValue *val, const char *op_name)
{
    return require_type_impl(IS_MENAI_FUNCTION(val), val, op_name, "function arguments");
}

static inline int require_function_singular(MenaiValue *val, const char *op_name)
{
    return require_type_impl(IS_MENAI_FUNCTION(val), val, op_name, "a function argument");
}

static inline int require_struct(MenaiValue *val, const char *op_name)
{
    return require_type_impl(IS_MENAI_STRUCT(val), val, op_name, "a struct argument");
}

static inline int require_structtype(MenaiValue *val, const char *op_name)
{
    return require_type_impl(IS_MENAI_STRUCTTYPE(val), val, op_name, "a struct type argument");
}

static inline int require_symbol(MenaiValue *val, const char *op_name)
{
    if (IS_MENAI_SYMBOL(val)) {
        return 1;
    }

    menai_raise_eval_errorf("%s: argument must be a symbol", op_name);
    return 0;
}

static inline int require_symbol_pair(MenaiValue *a, MenaiValue *b, const char *op_name)
{
    if (IS_MENAI_SYMBOL(a) && IS_MENAI_SYMBOL(b)) {
        return 1;
    }

    menai_raise_eval_errorf("%s: arguments must be symbols", op_name);
    return 0;
}

static PyObject *
menai_raise_eval_error(const char *message)
{
    PyErr_SetString(MenaiEvalError_type, message);
    return NULL;
}

static PyObject *
menai_raise_eval_errorf(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    PyObject *msg = PyUnicode_FromFormatV(fmt, args);
    va_end(args);
    if (msg == NULL) {
        return NULL;
    }
 
    PyErr_SetObject(MenaiEvalError_type, msg);
    Py_DECREF(msg);
    return NULL;
}

/*
 * parse_complex_string — parse a null-terminated ASCII string as a complex
 * number, matching Python's complex() constructor semantics.
 *
 * Grammar (after stripping leading/trailing whitespace):
 *
 *   complex  := float
 *             | imag_part
 *             | float imag_part
 *
 *   imag_part := sign? coefficient? ('j' | 'J')
 *   coefficient := float_magnitude
 *
 * where float is parsed by strtod (handles inf, nan, signs, scientific
 * notation), and a bare 'j'/'+j'/'-j' with no coefficient means 1j/-1j.
 *
 * Stores the parsed real and imaginary parts in *out_real and *out_imag.
 * Returns 1 on success, 0 on parse failure.
 */
static int
parse_complex_string(const char *s, double *out_real, double *out_imag)
{
    /* Skip leading whitespace. */
    while (*s == ' ' || *s == '\t') {
        s++;
    }

    /* Find end, skip trailing whitespace. */
    size_t len = strlen(s);
    while (len > 0 && (s[len - 1] == ' ' || s[len - 1] == '\t')) {
        len--;
    }

    if (len == 0) {
        return 0;
    }

    /* Work on a null-terminated copy of the trimmed string. */
    char buf[64];
    if (len >= sizeof(buf)) {
        return 0;
    }

    memcpy(buf, s, len);
    buf[len] = '\0';

    char *p = buf;
    char *end;
    double real = 0.0;
    double imag = 0.0;

    /*
     * Try to parse a leading float.  strtod consumes an optional sign,
     * digits, decimal point, exponent, and the special strings inf/nan.
     * If it consumes nothing (end == p), there is no real part.
     */
    double first = strtod(p, &end);
    if (end == p) {
        /* No leading float — must be a bare sign + 'j'. */
        first = 0.0;
    } else {
        p = end;
    }

    /* Check for end of string (pure real: "1.5", "inf", etc.) */
    if (*p == '\0') {
        *out_real = first;
        *out_imag = 0.0;
        return 1;
    }

    /* Check for imaginary suffix 'j'/'J' immediately (pure imaginary). */
    if (*p == 'j' || *p == 'J') {
        if (*(p + 1) != '\0') {
            return 0;
        }

        *out_real = 0.0;
        *out_imag = (end == buf) ? 1.0 : first;
        return 1;
    }

    /*
     * We have a real part (first) followed by an imaginary part.
     * The imaginary part starts with '+' or '-'.
     */
    if (*p != '+' && *p != '-') {
        return 0;
    }

    real = first;

    /*
     * Peek ahead: if the next character after the sign is 'j'/'J', this
     * is a bare +j or -j (coefficient 1).
     */
    if ((p[1] == 'j' || p[1] == 'J') && p[2] == '\0') {
        imag = (*p == '-') ? -1.0 : 1.0;
        *out_real = real;
        *out_imag = imag;
        return 1;
    }

    /* Parse the imaginary coefficient. */
    double imag_coeff = strtod(p, &end);
    if (end == p) {
        return 0;
    }

    p = end;
    if (*p != 'j' && *p != 'J') {
        return 0;
    }

    if (*(p + 1) != '\0') {
        return 0;
    }

    *out_real = real;
    *out_imag = imag_coeff;
    return 1;
}

static PyTypeObject *_py_code_object_type = NULL;

/*
 * Frame struct
 *
 * All fields are plain C.  code_obj is a retained MenaiCodeObject *; all
 * other pointers are borrowed from it and live as long as code_obj does.
 */
typedef struct {
    MenaiCodeObject *code_obj;       /* retained — owns all frame metadata */
    MenaiValue **constants_items;    /* borrowed from code_obj->constants */
    ssize_t nconst;                  /* borrowed from code_obj->nconst */
    const char **names_items;        /* borrowed from code_obj->names */
    hash_t *name_hashes;             /* borrowed from code_obj->name_hashes */
    ssize_t nnames;                  /* borrowed from code_obj->nnames */
    MenaiCodeObject **children;      /* borrowed from code_obj->children */
    ssize_t nchildren;               /* borrowed from code_obj->nchildren */
    uint64_t *instrs;                /* borrowed from code_obj->instrs */
    int code_len;
    int local_count;
    int ip;
    int base;
    int return_dest;
    int is_sentinel;
} Frame;

/*
 * frame_setup
 *
 * Populates a Frame from a MenaiCodeObject.  Takes a retain on co.
 */
static void
frame_setup(Frame *f, MenaiCodeObject *co, int base, int return_dest)
{
    menai_code_object_retain(co);
    if (f->code_obj) {
        menai_code_object_release(f->code_obj);
    }

    f->code_obj = co;
    f->constants_items = co->constants;
    f->nconst = co->nconst;
    f->names_items = co->names;
    f->name_hashes = co->name_hashes;
    f->nnames = co->nnames;
    f->children = co->children;
    f->nchildren = co->nchildren;
    f->instrs = co->instrs;
    f->code_len = co->code_len;
    f->local_count = co->local_count;
    f->ip = 0;
    f->base = base;
    f->return_dest = return_dest;
    f->is_sentinel = 0;
}

/* ---------------------------------------------------------------------------
 * Register array helpers
 *
 * The register array is a flat MenaiValue * array:
 *   regs[depth * max_locals + slot]
 * All slots are initialised to Menai_NONE (borrowed — the singleton is
 * kept alive by the module).  menai_reg_set_own/menai_reg_set_borrow manage reference counts correctly.
 * ------------------------------------------------------------------------- */

/*
 * GlobalsTable — open-addressing hash table for O(1) name lookup.
 *
 * Built once before execution starts from the globals dict.
 * Never mutated during execution.  Values are owned references.
 *
 * Lookup takes the UTF-8 string from frame->names_items[src0] (extracted once
 * at build time via PyUnicode_AsUTF8, cached in each slot).  The hash is a
 * FNV-1a string hash so the hot path cost is one hash + one strcmp per probe.
 * The slot count is the smallest power of 2 satisfying slot_count * 2 / 3 >= count.
 */
typedef struct {
    const char *name;   /* UTF-8 — points into PyUnicode internal buffer; NULL = empty */
    Py_hash_t hash;     /* FNV-1a hash of name */
    MenaiValue *value;  /* owned reference — valid only when name != NULL */
} GlobalsSlot;

typedef struct {
    const char *name;   /* UTF-8 — points into PyUnicode internal buffer */
    MenaiValue *value;  /* owned reference */
} GlobalsEntry;

typedef struct {
    GlobalsSlot *slots;     /* hash table — slot_count entries */
    GlobalsEntry *entries;  /* flat array — count entries, for iteration */
    ssize_t slot_count;     /* power of 2 */
    ssize_t count;          /* number of live entries */
    int owns_names;         /* 1 if entries[i].name are strdup'd and must be freed */
} GlobalsTable;

static void
globals_free(GlobalsTable *gt)
{
    for (ssize_t i = 0; i < gt->count; i++) {
        if (gt->owns_names) {
            free((char *)gt->entries[i].name);
        }
        menai_xrelease(gt->entries[i].value);
    }

    free(gt->slots);
    free(gt->entries);
    gt->slots = NULL;
    gt->entries = NULL;
    gt->slot_count = 0;
    gt->count = 0;
}

/*
 * Cached globals GlobalsTable.
 *
 * The globals dict (prelude functions and constants) is built once at Menai
 * startup and never changes.  We cache the converted GlobalsTable keyed by
 * the identity of the Python dict object.  On every subsequent execute() call
 * with the same dict pointer we reuse the cached table directly.
 */
static PyObject *_cached_globals_dict = NULL;
static GlobalsTable _cached_globals_gt;
static int _cached_globals_gt_valid = 0;

/* Forward declaration — execute_loop is defined later in this file. */
static MenaiValue *execute_loop(MenaiCodeObject *code, const GlobalsTable *globals,
                                MenaiValue **regs, int max_locals);

/*
 * globals_get — return a pointer to the cached GlobalsTable, building it the
 * first time a given globals_dict is seen.
 *
 * globals_key is either a Python dict of slow MenaiValue objects, or a Python
 * CodeObject representing the prelude.  When it is a CodeObject the prelude is
 * executed here once and the resulting dict is unpacked into the GlobalsTable;
 * subsequent calls with the same CodeObject identity reuse the cached table.
 * Returns NULL on error with a Python exception set.
 */
static const GlobalsTable *
globals_get(PyObject *globals_key)
{
    if (globals_key == _cached_globals_dict && _cached_globals_gt_valid) {
        return &_cached_globals_gt;
    }

    if (_cached_globals_gt_valid) {
        globals_free(&_cached_globals_gt);
        _cached_globals_gt_valid = 0;
        Py_DECREF(_cached_globals_dict);
        _cached_globals_dict = NULL;
    }

    _cached_globals_gt.slots = NULL;
    _cached_globals_gt.entries = NULL;
    _cached_globals_gt.slot_count = 0;
    _cached_globals_gt.count = 0;
    _cached_globals_gt.owns_names = 1;

    if (_py_code_object_type && Py_TYPE(globals_key) == _py_code_object_type) {
        /*
         * globals_key is a prelude CodeObject.  Execute it to obtain a
         * MenaiDict of fast values, then unpack directly into the GlobalsTable
         * without any slow round-trip.
         */
        MenaiCodeObject *prelude_co = menai_code_object_from_python(globals_key);
        if (!prelude_co) {
            return NULL;
        }

        int max_locals = menai_code_object_max_locals(prelude_co);
        MenaiValue **regs = menai_regs_alloc((size_t)(MAX_FRAME_DEPTH + 1) * max_locals, Menai_NONE);
        if (!regs) {
            menai_code_object_release(prelude_co);
            return NULL;
        }

        GlobalsTable empty;
        empty.slots = NULL;
        empty.entries = NULL;
        empty.slot_count = 0;
        empty.count = 0;
        MenaiValue *result = execute_loop(prelude_co, &empty, regs, max_locals);
        menai_regs_free(regs, (size_t)(MAX_FRAME_DEPTH + 1) * max_locals);
        menai_code_object_release(prelude_co);
        if (!result) {
            return NULL;
        }

        if (!IS_MENAI_DICT(result)) {
            menai_release(result);
            PyErr_SetString(PyExc_TypeError, "Prelude must evaluate to a dict");
            return NULL;
        }

        MenaiDict *d = (MenaiDict *)result;
        ssize_t n = d->length;
        if (n > 0) {
            _cached_globals_gt.entries = (GlobalsEntry *)malloc(n * sizeof(GlobalsEntry));
            if (!_cached_globals_gt.entries) {
                menai_release(result);
                PyErr_NoMemory();
                return NULL;
            }

            for (ssize_t i = 0; i < n; i++) {
                MenaiValue *k = d->keys[i];
                if (!IS_MENAI_STRING(k)) {
                    menai_release(result);
                    globals_free(&_cached_globals_gt);
                    PyErr_SetString(PyExc_TypeError, "Prelude dict keys must be strings");
                    return NULL;
                }

                menai_retain(d->values[i]);
                PyObject *py_key = menai_string_to_pyunicode(k);
                if (!py_key) {
                    menai_release(result);
                    globals_free(&_cached_globals_gt);
                    return NULL;
                }

                const char *utf8 = PyUnicode_AsUTF8(py_key);
                char *name_copy = utf8 ? strdup(utf8) : NULL;
                Py_DECREF(py_key);
                if (!name_copy) {
                    menai_release(result);
                    globals_free(&_cached_globals_gt);
                    PyErr_NoMemory();
                    return NULL;
                }

                _cached_globals_gt.entries[i].name = name_copy;
                _cached_globals_gt.entries[i].value = d->values[i];
                _cached_globals_gt.count++;
            }
        }

        menai_release(result);
    } else {
        /* globals_key is a Python dict of slow MenaiValue objects. */
        ssize_t n = PyDict_Size(globals_key);
        if (n > 0) {
            _cached_globals_gt.entries = (GlobalsEntry *)malloc(n * sizeof(GlobalsEntry));
            if (!_cached_globals_gt.entries) {
                PyErr_NoMemory();
                return NULL;
            }

            PyObject *key, *val;
            ssize_t pos = 0;
            while (PyDict_Next(globals_key, &pos, &key, &val)) {
                const char *name_utf8 = PyUnicode_AsUTF8(key);
                if (!name_utf8) {
                    globals_free(&_cached_globals_gt);
                    return NULL;
                }

                char *name_copy = strdup(name_utf8);
                if (!name_copy) {
                    globals_free(&_cached_globals_gt);
                    PyErr_NoMemory();
                    return NULL;
                }

                MenaiValue *fast_val = menai_convert_value(val);
                if (!fast_val) {
                    free(name_copy);
                    globals_free(&_cached_globals_gt);
                    return NULL;
                }

                _cached_globals_gt.entries[_cached_globals_gt.count].name = name_copy;
                _cached_globals_gt.entries[_cached_globals_gt.count].value = fast_val;
                _cached_globals_gt.count++;
            }
        }
    }

    Py_INCREF(globals_key);
    _cached_globals_dict = globals_key;
    _cached_globals_gt_valid = 1;
    return &_cached_globals_gt;
}

/*
 * globals_build — build a GlobalsTable from the cached globals GlobalsTable.
 *
 * All entries are already fast MenaiValue * objects retained and copied
 * directly from the cached table.  Returns 0 on success, -1 on error with a
 * Python exception set.
 */
static int
globals_build(GlobalsTable *gt, const GlobalsTable *globals_gt)
{
    ssize_t total = globals_gt ? globals_gt->count : 0;

    gt->slots = NULL;
    gt->entries = NULL;
    gt->slot_count = 0;
    gt->count = 0;
    gt->owns_names = 0;

    if (total > 0) {
        gt->entries = (GlobalsEntry *)malloc(total * sizeof(GlobalsEntry));
        if (gt->entries == NULL) {
            return -1;
        }

        ssize_t min_slots = (total * 3 + 1) / 2;
        ssize_t sc = 4;
        while (sc < min_slots) {
            sc <<= 1;
        }

        gt->slots = (GlobalsSlot *)malloc(sc * sizeof(GlobalsSlot));
        if (gt->slots == NULL) {
            free(gt->entries);
            gt->entries = NULL;
            return -1;
        }

        memset(gt->slots, 0, sc * sizeof(GlobalsSlot));
        gt->slot_count = sc;
    }

    for (ssize_t i = 0; i < total; i++) {
        menai_retain(globals_gt->entries[i].value);
        gt->entries[gt->count].name = globals_gt->entries[i].name;
        gt->entries[gt->count].value = globals_gt->entries[i].value;
        gt->count++;
    }

    /* Populate the hash table from the entries array. */
    ssize_t mask = gt->slot_count - 1;
    for (ssize_t i = 0; i < gt->count; i++) {
        const char *name = gt->entries[i].name;
        hash_t h = menai_name_str_hash(name);
        uhash_t perturb = (uhash_t)h;
        ssize_t slot = (ssize_t)(perturb & (uhash_t)mask);
        for (;;) {
            if (gt->slots[slot].name == NULL) {
                gt->slots[slot].name = name;
                gt->slots[slot].hash = h;
                gt->slots[slot].value = gt->entries[i].value;
                break;
            }

            perturb >>= 5;
            slot = (ssize_t)((5 * (uhash_t)slot + 1 + perturb) & (uhash_t)mask);
        }
    }

    return 0;
}

static MenaiValue *
globals_lookup_h(const GlobalsTable *gt, const char *name, hash_t h)
{
    if (gt->slot_count == 0) {
        return NULL;
    }

    ssize_t mask = gt->slot_count - 1;
    uhash_t perturb = (uhash_t)h;
    ssize_t slot = (ssize_t)(perturb & (uhash_t)mask);
    for (;;) {
        GlobalsSlot *s = &gt->slots[slot];
        if (s->name == NULL) {
            return NULL;
        }

        if (s->hash == h && strcmp(s->name, name) == 0) {
            return s->value;
        }

        perturb >>= 5;
        slot = (ssize_t)((5 * (uhash_t)slot + 1 + perturb) & (uhash_t)mask);
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
call_setup(Frame *new_frame, MenaiValue *func_obj, MenaiValue **regs, int callee_base, int arity, int return_dest)
{
    MenaiFunction *func = (MenaiFunction *)func_obj;
    MenaiCodeObject *co = func->bytecode;
    int param_count = co->param_count;
    int is_variadic = co->is_variadic;

    if (is_variadic) {
        int min_arity = param_count - 1;
        if (arity < min_arity) {
            const char *fname = co->name ? co->name : "<lambda>";
            menai_raise_eval_errorf(
                "Function '%s' expects at least %d argument%s, got %d",
                fname, min_arity, min_arity == 1 ? "" : "s", arity);
            return -1;
        }

        /* Pack excess args into a MenaiList for the rest parameter. */
        int rest_count = arity - min_arity;
        MenaiValue **rest_arr = rest_count > 0 ? (MenaiValue **)menai_alloc((size_t)rest_count * sizeof(MenaiValue *)) : NULL;
        if (rest_count > 0 && !rest_arr) {
            return -1;
        }

        for (int k = 0; k < rest_count; k++) {
            rest_arr[k] = regs[callee_base + min_arity + k];
            menai_retain(rest_arr[k]);
        }

        MenaiValue *rest_list = menai_list_from_array_steal(rest_arr, rest_count);
        if (rest_list == NULL) {
            return -1;
        }

        menai_reg_set_own(regs, callee_base + min_arity, rest_list);
    } else if (arity != param_count) {
        const char *fname = co->name ? co->name : "<lambda>";
        menai_raise_eval_errorf(
            "Function '%s' expects %d argument%s, got %d",
            fname, param_count, param_count == 1 ? "" : "s", arity);
        return -1;
    }

    /* Populate capture slots: regs[callee_base + param_count + i] */
    ssize_t ncap = func->ncap;
    for (ssize_t i = 0; i < ncap; i++) {
        MenaiValue *cv = func->captures[i];
        menai_reg_set_borrow(regs, callee_base + param_count + (int)i, cv);
    }

    frame_setup(new_frame, co, callee_base, return_dest);
    return 0;
}

/*
 * Internal execute — called by menai_vm_c_execute after setup.
 * Returns the result value (new reference) or NULL on error.
 */
static MenaiValue *
execute_loop(MenaiCodeObject *code, const GlobalsTable *globals,
             MenaiValue **regs, int max_locals)
{
    /* Frame stack — depth 0 is the sentinel. */
    Frame frames[MAX_FRAME_DEPTH + 1];
    frames[0] = (Frame){
        .is_sentinel = 1,
        .code_obj = NULL,
        .constants_items = NULL,
        .instrs = NULL,
    };
    frames[1] = (Frame){
        .is_sentinel = 0,
        .code_obj = NULL,
        .constants_items = NULL,
        .instrs = NULL,
    };

    /* Set up frame at depth 1 for the top-level code object. */
    frame_setup(&frames[1], code, 0, 0);

    int frame_depth = 1;
    Frame *frame = &frames[1];
    int instr_count = 0;

    while (1) {
        /* Cancellation check */
        if ((++instr_count & (CANCEL_CHECK_INTERVAL - 1)) == 0) {
            instr_count = 0;
            if (PyErr_CheckSignals() < 0) {
                goto error;
            }
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
        int base = frame->base;

        switch (opcode) {
        case OP_LOAD_NONE:
            menai_reg_set_borrow(regs, base + dest, Menai_NONE);
            break;

        case OP_LOAD_TRUE:
            menai_reg_set_borrow(regs, base + dest, Menai_TRUE);
            break;

        case OP_LOAD_FALSE:
            menai_reg_set_borrow(regs, base + dest, Menai_FALSE);
            break;

        case OP_LOAD_EMPTY_LIST:
            menai_reg_set_borrow(regs, base + dest, Menai_EMPTY_LIST);
            break;

        case OP_LOAD_EMPTY_DICT:
            menai_reg_set_borrow(regs, base + dest, Menai_EMPTY_DICT);
            break;

        case OP_LOAD_EMPTY_SET:
            menai_reg_set_borrow(regs, base + dest, Menai_EMPTY_SET);
            break;

        case OP_LOAD_CONST: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *val = frame->constants_items[src0];
            menai_reg_set_borrow(regs, base + dest, val);
            break;
        }

        case OP_LOAD_NAME: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            const char *name_str = frame->names_items[src0];
            hash_t name_hash = frame->name_hashes[src0];
            MenaiValue *val = globals_lookup_h(globals, name_str, name_hash);
            if (val == NULL) {
                /* Build a rich error listing up to 10 available names. */
                ssize_t nk = globals->count;
                ssize_t show = nk < 10 ? nk : 10;
                char buf[1024];
                int off = 0;
                for (ssize_t i = 0; i < show && off < (int)sizeof(buf) - 2; i++) {
                    if (i > 0 && off < (int)sizeof(buf) - 4) {
                        buf[off++] = ',';
                        buf[off++] = ' ';
                    }

                    const char *kn = globals->entries[i].name;
                    int klen = (int)strlen(kn);
                    if (off + klen >= (int)sizeof(buf) - 4) {
                        break;
                    }

                    memcpy(buf + off, kn, klen);
                    off += klen;
                }

                buf[off] = '\0';
                menai_raise_eval_errorf(
                    "Undefined variable: '%s'\n  Available variables: %s%s",
                    name_str, buf, nk > 10 ? "..." : "");
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, val);
            break;
        }

        case OP_MOVE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            menai_reg_set_borrow(regs, base + dest, regs[base +src0]);
            break;
        }

        case OP_JUMP: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            frame->ip = src0;
            break;
        }

        case OP_JUMP_IF_FALSE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *cond = regs[base + src0];
            if (!IS_MENAI_BOOLEAN(cond)) {
                menai_raise_eval_error("If condition must be boolean");
                goto error;
            }

            if (!menai_boolean_value(cond)) {
                frame->ip = src1;
            }

            break;
        }

        case OP_JUMP_IF_TRUE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *cond = regs[base + src0];
            if (!IS_MENAI_BOOLEAN(cond)) {
                menai_raise_eval_error("If condition must be boolean");
                goto error;
            }

            if (menai_boolean_value(cond)) {
                frame->ip = src1;
            }

            break;
        }

        case OP_RAISE_ERROR: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *msg = regs[base + src0];
            if (!IS_MENAI_STRING(msg)) {
                menai_raise_eval_error("error: message must be a string");
                goto error;
            }

            PyObject *s = menai_string_to_pyunicode(msg);
            if (s == NULL) {
                goto error;
            }

            PyErr_SetObject(MenaiEvalError_type, s);
            Py_DECREF(s);
            goto error;
        }

        case OP_RETURN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *retval = regs[base + src0];
            menai_retain(retval);

            int saved_return_dest = frame->return_dest;
            menai_code_object_release(frame->code_obj);
            frame_depth--;
            Frame *caller = &frames[frame_depth];

            if (caller->is_sentinel) {
                /* Top-level return — exit the loop. */
                return retval;
            }

            /* Store result into caller's register window. */
            menai_reg_set_own(regs, caller->base + saved_return_dest, retval);

            frame = caller;
            break;
        }

        case OP_CALL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *raw = regs[base + src0];
            int arity = src1;

            int callee_base = base + frame->local_count;

            if (IS_MENAI_FUNCTION(raw)) {
                if (frame_depth >= MAX_FRAME_DEPTH) {
                    menai_raise_eval_error("Maximum call depth exceeded");
                    goto error;
                }

                frame_depth++;
                Frame *new_frame = &frames[frame_depth];
                new_frame->code_obj = NULL;
                new_frame->constants_items = NULL;
                new_frame->instrs = NULL;

                if (call_setup(new_frame, raw, regs, callee_base, arity, dest) < 0) {
                    frame_depth--;
                    goto error;
                }

                frame = new_frame;
                break;
            }

            if (IS_MENAI_STRUCTTYPE(raw)) {
                /* Struct constructor call */
                int n_fields = ((MenaiStructType *)raw)->nfields;
                if (arity != (int)n_fields) {
                    PyObject *sname = menai_string_to_pyunicode(((MenaiStructType *)raw)->name);
                    menai_raise_eval_errorf(
                        "Struct constructor '%s' called with wrong number of arguments",
                        sname ? PyUnicode_AsUTF8(sname) : "?");
                    Py_XDECREF(sname);
                    goto error;
                }

                MenaiValue *instance = menai_struct_alloc(raw, &regs[callee_base], n_fields);
                if (instance == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, instance);
                break;
            }

            menai_raise_eval_error("Cannot call non-function value");
            goto error;
        }

        case OP_TAIL_CALL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *raw = regs[base + src0];
            int n_args = src1;

            /* Take an owned reference before the arg-moving loop.
             * The loop may overwrite regs[base+src0] if src0 < n_args,
             * which would decrement raw's refcount to zero and free it. */
            menai_retain(raw);

            int local_count = frame->local_count;

            if (IS_MENAI_FUNCTION(raw)) {
                /* Move outgoing args down to base+0..n_args-1 in place. */
                for (int i = 0; i < n_args; i++) {
                    MenaiValue *v = regs[base + local_count + i];
                    menai_reg_set_borrow(regs, base + i, v);
                }

                /* Reuse current frame — release old code_obj and instructions. */
                menai_code_object_release(frame->code_obj);
                frame->code_obj = NULL;  /* frame_setup will retain the new one */

                int saved_return_dest = frame->return_dest;
                if (call_setup(frame, raw, regs, base, n_args, saved_return_dest) < 0) {
                    menai_release(raw);
                    goto error;
                }

                menai_release(raw);
                break;
            }

            if (IS_MENAI_STRUCTTYPE(raw)) {
                int n_fields = ((MenaiStructType *)raw)->nfields;
                if (n_args != (int)n_fields) {
                    PyObject *sname = menai_string_to_pyunicode(((MenaiStructType *)raw)->name);
                    menai_raise_eval_errorf(
                        "Struct constructor '%s' called with wrong number of arguments",
                        sname ? PyUnicode_AsUTF8(sname) : "?");
                    Py_XDECREF(sname);
                    menai_release(raw);
                    goto error;
                }

                MenaiValue *instance = menai_struct_alloc(raw, &regs[base + local_count], n_fields);
                if (instance == NULL) {
                    menai_release(raw);
                    goto error;
                }

                /* Tail-return the struct: pop frame and deliver to caller. */
                MenaiValue *retval = instance;
                int saved_return_dest = frame->return_dest;
                menai_code_object_release(frame->code_obj);
                frame_depth--;
                Frame *caller = &frames[frame_depth];
                if (caller->is_sentinel) {
                    menai_release(raw);
                    return retval;
                }

                menai_reg_set_own(regs, caller->base + saved_return_dest, retval);
                menai_release(raw);
                frame = caller;
                break;
            }

            menai_release(raw);
            menai_raise_eval_error("Cannot call non-function value");
            goto error;
        }

        case OP_APPLY: {
            /*
             * APPLY dest, src0, src1:
             * src0 = function register, src1 = arg_list register.
             * Scatters the list into the callee's register window and pushes a frame.
             */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *raw_func = regs[base + src0];
            MenaiValue *raw_args = regs[base + src1];

            if (!IS_MENAI_LIST(raw_args)) {
                menai_raise_eval_error("apply: second argument must be a list");
                goto error;
            }

            MenaiValue **elements = ((MenaiList *)raw_args)->elements;
            int arity = (int)((MenaiList *)raw_args)->length;

            if (IS_MENAI_FUNCTION(raw_func)) {
                if (frame_depth >= MAX_FRAME_DEPTH) {
                    menai_raise_eval_error("Maximum call depth exceeded");
                    goto error;
                }

                int callee_base = base + frame->local_count;

                /* Scatter list elements into the callee window */
                for (int i = 0; i < arity; i++) {
                    menai_reg_set_borrow(regs, callee_base + i, elements[i]);
                }

                frame_depth++;
                Frame *new_frame = &frames[frame_depth];
                new_frame->code_obj = NULL;
                new_frame->constants_items = NULL;
                new_frame->instrs = NULL;

                if (call_setup(new_frame, raw_func, regs, callee_base, arity, dest) < 0) {
                    frame_depth--;
                    goto error;
                }

                frame = new_frame;
                break;
            }

            if (IS_MENAI_STRUCTTYPE(raw_func)) {
                int n_fields = ((MenaiStructType *)raw_func)->nfields;
                if (arity != (int)n_fields) {
                    menai_raise_eval_error("Struct constructor called with wrong number of arguments");
                    goto error;
                }

                MenaiValue *instance = menai_struct_alloc(raw_func, elements, n_fields);
                if (instance == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, instance);
                break;
            }

            menai_raise_eval_error("apply: first argument must be a function");
            goto error;
        }

        case OP_TAIL_APPLY: {
            /*
             * TAIL_APPLY src0, src1:
             * src0 = function register, src1 = arg_list register.
             * Reuses current frame (tail position).
             */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *raw_func = regs[base + src0];
            MenaiValue *raw_args = regs[base + src1];
            /* Own raw_func before the scatter loop which may overwrite its slot. */
            /* Own raw_args for the same reason — src1 may be < arity. */
            menai_retain(raw_func);
            menai_retain(raw_args);

            if (!IS_MENAI_LIST(raw_args)) {
                menai_release(raw_func);
                menai_release(raw_args);
                menai_raise_eval_error("apply: second argument must be a list");
                goto error;
            }

            MenaiValue **elements = ((MenaiList *)raw_args)->elements;
            int arity = (int)((MenaiList *)raw_args)->length;

            if (IS_MENAI_FUNCTION(raw_func)) {
                /* Scatter args into base+0..arity-1 (reusing current frame's base) */
                for (int i = 0; i < arity; i++) {
                    menai_reg_set_borrow(regs, base + i, elements[i]);
                }

                menai_release(raw_args);

                /* Release old code_obj and instructions, reuse frame. */
                menai_code_object_release(frame->code_obj);
                frame->code_obj = NULL;  /* frame_setup will retain the new one */

                int saved_return_dest = frame->return_dest;
                if (call_setup(frame, raw_func, regs, base, arity, saved_return_dest) < 0) {
                    menai_release(raw_func);
                    goto error;
                }

                menai_release(raw_func);
                break;
            }

            if (IS_MENAI_STRUCTTYPE(raw_func)) {
                int n_fields = ((MenaiStructType *)raw_func)->nfields;
                if (arity != (int)n_fields) {
                    menai_release(raw_func);
                    menai_release(raw_args);
                    menai_raise_eval_error("Struct constructor called with wrong number of arguments");
                    goto error;
                }

                MenaiValue *retval = menai_struct_alloc(raw_func, elements, n_fields);
                if (retval == NULL) {
                    menai_release(raw_args);
                    menai_release(raw_func);
                    goto error;
                }

                int saved_return_dest = frame->return_dest;
                menai_code_object_release(frame->code_obj);
                frame_depth--;
                Frame *caller = &frames[frame_depth];
                if (caller->is_sentinel) {
                    menai_release(raw_args);
                    menai_release(raw_func);
                    return retval;
                }

                menai_reg_set_own(regs, caller->base + saved_return_dest, retval);
                menai_release(raw_args);
                menai_release(raw_func);
                frame = caller;
                break;
            }

            menai_release(raw_func);
            menai_release(raw_args);
            menai_raise_eval_error("apply: first argument must be a function");
            goto error;
        }

        case OP_EMIT_TRACE:
            /* Trace is a no-op in the C VM — no watcher support yet. */
            break;

        case OP_NONE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_NONE(regs[base + src0]));
            break;
        }

        case OP_BOOLEAN_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_BOOLEAN(regs[base + src0]));
            break;
        }

        case OP_BOOLEAN_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_boolean(a, "boolean=?")) {
                goto error;
            }

            if (!require_boolean(b, "boolean=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_boolean_value(a) == menai_boolean_value(b));
            break;
        }

        case OP_BOOLEAN_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_boolean(a, "boolean!=?")) {
                goto error;
            }

            if (!require_boolean(b, "boolean!=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_boolean_value(a) != menai_boolean_value(b));
            break;
        }

        case OP_BOOLEAN_NOT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_boolean(a, "boolean-not")) {
                goto error;
            }

            bool_store(regs, base + dest, !menai_boolean_value(a));
            break;
        }

        case OP_SYMBOL_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_SYMBOL(regs[base + src0]));
            break;
        }

        case OP_SYMBOL_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_symbol_pair(a, b, "symbol=?")) {
                goto error;
            }

            bool_store(regs, base + dest,
                menai_string_equal(menai_symbol_name(a), menai_symbol_name(b)));
            break;
        }

        case OP_SYMBOL_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_symbol_pair(a, b, "symbol!=?")) {
                goto error;
            }

            bool_store(regs, base + dest,
                !menai_string_equal(menai_symbol_name(a), menai_symbol_name(b)));
            break;
        }

        case OP_SYMBOL_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_symbol(a, "symbol->string")) {
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, menai_symbol_name(a));
            break;
        }

        case OP_FUNCTION_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_FUNCTION(regs[base + src0]));
            break;
        }

        case OP_FUNCTION_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_function(a, "function=?")) {
                goto error;
            }

            if (!require_function(b, "function=?")) {
                goto error;
            }

            bool_store(regs, base + dest, a == b);
            break;
        }

        case OP_FUNCTION_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_function(a, "function!=?")) {
                goto error;
            }

            if (!require_function(b, "function!=?")) {
                goto error;
            }

            bool_store(regs, base + dest, a != b);
            break;
        }

        case OP_FUNCTION_MIN_ARITY: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *f = regs[base + src0];
            if (!require_function_singular(f, "function-min-arity")) {
                goto error;
            }

            MenaiFunction *fn = (MenaiFunction *)f;
            int min_a = fn->bytecode->is_variadic ? fn->bytecode->param_count - 1 : fn->bytecode->param_count;
            MenaiValue *_r = make_integer_from_long(min_a);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FUNCTION_VARIADIC_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *f = regs[base + src0];
            if (!require_function_singular(f, "function-variadic?")) {
                goto error;
            }

            bool_store(regs, base + dest, ((MenaiFunction *)f)->bytecode->is_variadic);
            break;
        }

        case OP_FUNCTION_ACCEPTS_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *f = regs[base + src0];
            MenaiValue *n_obj = regs[base + src1];
            if (!require_function_singular(f, "function-accepts?")) {
                goto error;
            }

            if (!require_integer(n_obj, "function-accepts?")) {
                goto error;
            }

            MenaiFunction *fn = (MenaiFunction *)f;
            int pc = fn->bytecode->param_count;
            int is_var = fn->bytecode->is_variadic;
            MenaiInteger *n_io = (MenaiInteger *)n_obj;
            long n;
            if (!n_io->is_big) {
                n = n_io->small;
            } else {
                if (menai_bigint_to_long(&n_io->big, &n) < 0) {
                    goto error;
                }
            }

            int accepts = is_var ? (n >= pc - 1) : (n == pc);
            bool_store(regs, base + dest, accepts);
            break;
        }

        case OP_INTEGER_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_INTEGER(regs[base + src0]));
            break;
        }

        case OP_INTEGER_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer=?")) {
                goto error;
            }

            if (!require_integer(b, "integer=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_EQ));
            break;
        }

        case OP_INTEGER_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer!=?")) {
                goto error;
            }

            if (!require_integer(b, "integer!=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_NE));
            break;
        }

        case OP_INTEGER_LT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer<?")) {
                goto error;
            }

            if (!require_integer(b, "integer<?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_LT));
            break;
        }

        case OP_INTEGER_GT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer>?")) {
                goto error;
            }

            if (!require_integer(b, "integer>?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_GT));
            break;
        }

        case OP_INTEGER_LTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer<=?")) {
                goto error;
            }

            if (!require_integer(b, "integer<=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_LE));
            break;
        }

        case OP_INTEGER_GTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer>=?")) {
                goto error;
            }

            if (!require_integer(b, "integer>=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_integer_compare(a, b, Py_GE));
            break;
        }

        case OP_INTEGER_ABS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_integer(a, "integer-abs")) {
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            if (!ia->is_big) {
                long sv = ia->small;
                long rv = sv < 0 ? -sv : sv;
                /* LONG_MIN has no positive counterpart — promote to bigint. */
                if (sv == LONG_MIN) {
                    MenaiBigInt tmp, res;
                    menai_bigint_init(&tmp);
                    menai_bigint_init(&res);
                    if (menai_bigint_from_long(sv, &tmp) < 0) {
                        goto error;
                    }

                    if (menai_bigint_abs(&tmp, &res) < 0) {
                        menai_bigint_free(&tmp); goto error;
                    }

                    menai_bigint_free(&tmp);
                    MenaiValue *_r = menai_integer_from_bigint(res);
                    if (!_r) {
                        goto error;
                    }

                    menai_reg_set_own(regs, base + dest, _r);
                    break;
                }

                MenaiValue *_r = menai_integer_from_long(rv);
                if (!_r) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
                break;
            }

            MenaiBigInt res;
            menai_bigint_init(&res);
            if (menai_bigint_abs(&ia->big, &res) < 0) {
                goto error;
            }

            MenaiValue *_r = menai_integer_from_bigint(res);
            if (!_r) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_NEG: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_integer(a, "integer-neg")) {
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            if (!ia->is_big) {
                long sv = ia->small;
                /* LONG_MIN negation overflows — promote to bigint. */
                if (sv == LONG_MIN) {
                    MenaiBigInt tmp, res;
                    menai_bigint_init(&tmp);
                    menai_bigint_init(&res);
                    if (menai_bigint_from_long(sv, &tmp) < 0) {
                        goto error;
                    }

                    if (menai_bigint_neg(&tmp, &res) < 0) {
                        menai_bigint_free(&tmp); goto error;
                    }

                    menai_bigint_free(&tmp);
                    MenaiValue *_r = menai_integer_from_bigint(res);
                    if (!_r) {
                        goto error;
                    }

                    menai_reg_set_own(regs, base + dest, _r);
                    break;
                }

                MenaiValue *_r = menai_integer_from_long(-sv);
                if (!_r) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
                break;
            }

            MenaiBigInt res;
            menai_bigint_init(&res);
            if (menai_bigint_neg(&ia->big, &res) < 0) {
                goto error;
            }

            MenaiValue *_r = menai_integer_from_bigint(res);
            if (!_r) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_NOT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_integer(a, "integer-bit-not")) {
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiBigInt tmp, res;
            menai_bigint_init(&tmp);
            menai_bigint_init(&res);
            if (!ia->is_big) {
                if (menai_bigint_from_long(ia->small, &tmp) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&ia->big, &tmp) < 0) {
                    goto error;
                }
            }

            if (menai_bigint_not(&tmp, &res) < 0) {
                menai_bigint_free(&tmp); goto error;
            }

            menai_bigint_free(&tmp);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (!_r) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_ADD: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer+")) {
                goto error;
            }

            if (!require_integer(b, "integer+")) {
                goto error;
            }

            if (!((MenaiInteger *)a)->is_big && !((MenaiInteger *)b)->is_big) {
                long la = ((MenaiInteger *)a)->small;
                long lb = ((MenaiInteger *)b)->small;
                long lr;
                if (!_menai_add_overflow(la, lb, &lr)) {
                    MenaiValue *_r = menai_integer_from_long(lr);
                    if (!_r) {
                        goto error;
                    }

                    menai_reg_set_own(regs, base + dest, _r);
                    break;
                }
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            if (!((MenaiInteger *)a)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)a)->small, &av) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)a)->big, &av) < 0) {
                    goto error;
                }
            }

            if (!((MenaiInteger *)b)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)b)->small, &bv) < 0) {
                    menai_bigint_free(&av);
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)b)->big, &bv) < 0) {
                    menai_bigint_free(&av);
                    goto error;
                }
            }

            if (menai_bigint_add(&av, &bv, &res) < 0) {
                menai_bigint_free(&av);
                menai_bigint_free(&bv);
                goto error;
            }

            menai_bigint_free(&av);
            menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (!_r) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_SUB: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer-")) {
                goto error;
            }

            if (!require_integer(b, "integer-")) {
                goto error;
            }

            if (!((MenaiInteger *)a)->is_big && !((MenaiInteger *)b)->is_big) {
                long la = ((MenaiInteger *)a)->small;
                long lb = ((MenaiInteger *)b)->small;
                long lr;
                if (!_menai_sub_overflow(la, lb, &lr)) {
                    MenaiValue *_r = menai_integer_from_long(lr);
                    if (!_r) {
                        goto error;
                    }

                    menai_reg_set_own(regs, base + dest, _r);
                    break;
                }
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av); menai_bigint_init(&bv); menai_bigint_init(&res);
            if (!((MenaiInteger *)a)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)a)->small, &av) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)a)->big, &av) < 0) {
                    goto error;
                }
            }

            if (!((MenaiInteger *)b)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)b)->small, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)b)->big, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            }

            if (menai_bigint_sub(&av, &bv, &res) < 0) {
                menai_bigint_free(&av); menai_bigint_free(&bv); goto error;
            }

            menai_bigint_free(&av); menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (!_r) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_MUL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer*")) {
                goto error;
            }

            if (!require_integer(b, "integer*")) {
                goto error;
            }

            if (!((MenaiInteger *)a)->is_big && !((MenaiInteger *)b)->is_big) {
                long la = ((MenaiInteger *)a)->small;
                long lb = ((MenaiInteger *)b)->small;
                long lr;
                if (!_menai_mul_overflow(la, lb, &lr)) {
                    MenaiValue *_r = menai_integer_from_long(lr);
                    if (!_r) {
                        goto error;
                    }

                    menai_reg_set_own(regs, base + dest, _r);
                    break;
                }
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av); menai_bigint_init(&bv); menai_bigint_init(&res);
            if (!((MenaiInteger *)a)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)a)->small, &av) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)a)->big, &av) < 0) {
                    goto error;
                }
            }

            if (!((MenaiInteger *)b)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)b)->small, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)b)->big, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            }

            if (menai_bigint_mul(&av, &bv, &res) < 0) {
                menai_bigint_free(&av); menai_bigint_free(&bv); goto error;
            }

            menai_bigint_free(&av); menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (!_r) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_DIV: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer/")) {
                goto error;
            }

            if (!require_integer(b, "integer/")) {
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            int b_is_zero = (!ib->is_big && ib->small == 0) || (ib->is_big && ib->big.sign == 0);
            if (b_is_zero) {
                menai_raise_eval_error("Division by zero in 'integer/'");
                goto error;
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av); menai_bigint_init(&bv); menai_bigint_init(&res);
            if (!((MenaiInteger *)a)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)a)->small, &av) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)a)->big, &av) < 0) {
                    goto error;
                }
            }

            if (!((MenaiInteger *)b)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)b)->small, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)b)->big, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            }

            if (menai_bigint_floordiv(&av, &bv, &res) < 0) {
                menai_bigint_free(&av); menai_bigint_free(&bv); goto error;
            }

            menai_bigint_free(&av); menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_MOD: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer%")) {
                goto error;
            }

            if (!require_integer(b, "integer%")) {
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            int b_is_zero = (!ib->is_big && ib->small == 0) || (ib->is_big && ib->big.sign == 0);
            if (b_is_zero) {
                menai_raise_eval_error("Modulo by zero in 'integer%'");
                goto error;
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            if (!((MenaiInteger *)a)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)a)->small, &av) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)a)->big, &av) < 0) {
                    goto error;
                }
            }

            if (!((MenaiInteger *)b)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)b)->small, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)b)->big, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            }

            if (menai_bigint_mod(&av, &bv, &res) < 0) {
                menai_bigint_free(&av); menai_bigint_free(&bv); goto error;
            }

            menai_bigint_free(&av); menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_EXPN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer-expn")) {
                goto error;
            }

            if (!require_integer(b, "integer-expn")) {
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            int b_is_neg = (!ib->is_big && ib->small < 0) || (ib->is_big && ib->big.sign == -1);
            if (b_is_neg) {
                menai_raise_eval_error("Function 'integer-expn' requires a non-negative exponent");
                goto error;
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            if (!((MenaiInteger *)a)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)a)->small, &av) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)a)->big, &av) < 0) {
                    goto error;
                }
            }

            if (!((MenaiInteger *)b)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)b)->small, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)b)->big, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            }

            if (menai_bigint_pow(&av, &bv, &res) < 0) {
                menai_bigint_free(&av); menai_bigint_free(&bv); goto error;
            }

            menai_bigint_free(&av); menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_OR: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-or")) {
                goto error;
            }

            if (!require_integer(b, "integer-bit-or")) {
                goto error;
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            if (!((MenaiInteger *)a)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)a)->small, &av) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)a)->big, &av) < 0) {
                    goto error;
                }
            }

            if (!((MenaiInteger *)b)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)b)->small, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)b)->big, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            }

            if (menai_bigint_or(&av, &bv, &res) < 0) {
                menai_bigint_free(&av); menai_bigint_free(&bv); goto error;
            }

            menai_bigint_free(&av); menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (!_r) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_AND: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-and")) {
                goto error;
            }

            if (!require_integer(b, "integer-bit-and")) {
                goto error;
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            if (!((MenaiInteger *)a)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)a)->small, &av) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)a)->big, &av) < 0) {
                    goto error;
                }
            }

            if (!((MenaiInteger *)b)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)b)->small, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)b)->big, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            }

            if (menai_bigint_and(&av, &bv, &res) < 0) {
                menai_bigint_free(&av); menai_bigint_free(&bv); goto error;
            }

            menai_bigint_free(&av); menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (!_r) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_XOR: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-xor")) {
                goto error;
            }

            if (!require_integer(b, "integer-bit-xor")) {
                goto error;
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            if (!((MenaiInteger *)a)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)a)->small, &av) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)a)->big, &av) < 0) {
                    goto error;
                }
            }

            if (!((MenaiInteger *)b)->is_big) {
                if (menai_bigint_from_long(((MenaiInteger *)b)->small, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            } else {
                if (menai_bigint_copy(&((MenaiInteger *)b)->big, &bv) < 0) {
                    menai_bigint_free(&av); goto error;
                }
            }

            if (menai_bigint_xor(&av, &bv, &res) < 0) {
                menai_bigint_free(&av); menai_bigint_free(&bv); goto error;
            }

            menai_bigint_free(&av); menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (!_r) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_SHIFT_LEFT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-shift-left")) {
                goto error;
            }

            if (!require_integer(b, "integer-bit-shift-left")) {
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            long shift;
            if (!ib->is_big) {
                shift = ib->small;
            } else {
                if (!menai_bigint_fits_long(&ib->big)) {
                    menai_raise_eval_error("integer-bit-shift-left: shift amount too large");
                    goto error;
                }

                if (menai_bigint_to_long(&ib->big, &shift) < 0) {
                    goto error;
                }
            }

            if (shift < 0) {
                menai_raise_eval_error("integer-bit-shift-left: shift amount must be non-negative");
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiBigInt av, res;
            menai_bigint_init(&av); menai_bigint_init(&res);
            if (!ia->is_big) {
                if (menai_bigint_from_long(ia->small, &av) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&ia->big, &av) < 0) {
                    goto error;
                }
            }

            if (menai_bigint_shift_left(&av, (ssize_t)shift, &res) < 0) {
                menai_bigint_free(&av); goto error;
            }

            menai_bigint_free(&av);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (!_r) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_SHIFT_RIGHT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer-bit-shift-right")) {
                goto error;
            }

            if (!require_integer(b, "integer-bit-shift-right")) {
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            long shift;
            if (!ib->is_big) {
                shift = ib->small;
            } else {
                if (!menai_bigint_fits_long(&ib->big)) {
                    menai_raise_eval_error("integer-bit-shift-right: shift amount too large");
                    goto error;
                }

                if (menai_bigint_to_long(&ib->big, &shift) < 0) {
                    goto error;
                }
            }

            if (shift < 0) {
                menai_raise_eval_error("integer-bit-shift-right: shift amount must be non-negative");
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiBigInt av, res;
            menai_bigint_init(&av); menai_bigint_init(&res);
            if (!ia->is_big) {
                if (menai_bigint_from_long(ia->small, &av) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&ia->big, &av) < 0) {
                    goto error;
                }
            }

            if (menai_bigint_shift_right(&av, (ssize_t)shift, &res) < 0) {
                menai_bigint_free(&av); goto error;
            }

            menai_bigint_free(&av);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (!_r) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_MIN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer-min")) {
                goto error;
            }

            if (!require_integer(b, "integer-min")) {
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, menai_integer_compare(a, b, Py_LE) ? a : b);
            break;
        }

        case OP_INTEGER_MAX: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer-max")) {
                goto error;
            }

            if (!require_integer(b, "integer-max")) {
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, menai_integer_compare(a, b, Py_GE) ? a : b);
            break;
        }

        case OP_INTEGER_TO_FLOAT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_integer(a, "integer->float")) {
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            double d;
            if (!ia->is_big) {
                d = (double)ia->small;
            } else {
                if (menai_bigint_to_double(&ia->big, &d) < 0) {
                    goto error;
                }
            }

            MenaiValue *_r = make_float(d);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_TO_COMPLEX: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer->complex")) {
                goto error;
            }

            if (!require_integer(b, "integer->complex")) {
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            double re, im;
            if (!ia->is_big) {
                re = (double)ia->small;
            } else {
                if (menai_bigint_to_double(&ia->big, &re) < 0) {
                    goto error;
                }
            }

            if (!ib->is_big) {
                im = (double)ib->small;
            } else {
                if (menai_bigint_to_double(&ib->big, &im) < 0) {
                    goto error;
                }
            }

            MenaiValue *r = make_complex(re, im);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_INTEGER_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_integer(a, "integer->string")) {
                goto error;
            }

            if (!require_integer(b, "integer->string")) {
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            long radix;
            if (!ib->is_big) {
                radix = ib->small;
            } else {
                if (menai_bigint_to_long(&ib->big, &radix) < 0) {
                    goto error;
                }
            }

            if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
                menai_raise_eval_errorf("integer->string: radix must be 2, 8, 10, or 16, got %ld", radix);
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiBigInt tmp;
            menai_bigint_init(&tmp);
            if (!ia->is_big) {
                if (menai_bigint_from_long(ia->small, &tmp) < 0) {
                    goto error;
                }
            } else {
                if (menai_bigint_copy(&ia->big, &tmp) < 0) {
                    goto error;
                }
            }

            char *cstr = NULL;
            if (menai_bigint_to_string(&tmp, (int)radix, &cstr) < 0) {
                menai_bigint_free(&tmp); goto error;
            }

            menai_bigint_free(&tmp);
            MenaiValue *r = menai_string_from_utf8(cstr, (ssize_t)strlen(cstr));
            free(cstr);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_INTEGER_CODEPOINT_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_integer(a, "integer-codepoint->string")) {
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            long cp;
            if (!ia->is_big) {
                cp = ia->small;
            } else {
                if (menai_bigint_to_long(&ia->big, &cp) < 0) {
                    goto error;
                }
            }

            if (cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF)) {
                menai_raise_eval_errorf(
                    "integer-codepoint->string: invalid Unicode scalar value %ld", cp);
                goto error;
            }

            MenaiValue *r = menai_string_from_codepoint((uint32_t)cp);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_FLOAT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_FLOAT(regs[base + src0]));
            break;
        }

        case OP_FLOAT_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float=?")) {
                goto error;
            }

            if (!require_float(b, "float=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) == menai_float_value(b));
            break;
        }

        case OP_FLOAT_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float!=?")) {
                goto error;
            }

            if (!require_float(b, "float!=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) != menai_float_value(b));
            break;
        }

        case OP_FLOAT_LT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float<?")) {
                goto error;
            }

            if (!require_float(b, "float<?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) < menai_float_value(b));
            break;
        }

        case OP_FLOAT_GT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float>?")) {
                goto error;
            }

            if (!require_float(b, "float>?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) > menai_float_value(b));
            break;
        }

        case OP_FLOAT_LTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float<=?")) {
                goto error;
            }

            if (!require_float(b, "float<=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) <= menai_float_value(b));
            break;
        }

        case OP_FLOAT_GTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float>=?")) {
                goto error;
            }

            if (!require_float(b, "float>=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) >= menai_float_value(b));
            break;
        }

        case OP_FLOAT_NEG: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-neg")) {
                goto error;
            }

            MenaiValue *_r = make_float(-menai_float_value(a));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_ABS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-abs")) {
                goto error;
            }

            double v = menai_float_value(a);
            {
                MenaiValue *_r = make_float(fabs(v));
                if (_r == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
            }

            break;
        }

        case OP_FLOAT_ADD: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float+")) {
                goto error;
            }

            if (!require_float(b, "float+")) {
                goto error;
            }

            MenaiValue *_r = make_float(menai_float_value(a) + menai_float_value(b));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_SUB: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float-")) {
                goto error;
            }

            if (!require_float(b, "float-")) {
                goto error;
            }

            MenaiValue *_r = make_float(menai_float_value(a) - menai_float_value(b));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_MUL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float*")) {
                goto error;
            }

            if (!require_float(b, "float*")) {
                goto error;
            }

            MenaiValue *_r = make_float(menai_float_value(a) * menai_float_value(b));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_DIV: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float/")) {
                goto error;
            }

            if (!require_float(b, "float/")) {
                goto error;
            }

            double bv = menai_float_value(b);
            if (bv == 0.0) {
                menai_raise_eval_error("Division by zero in 'float/'");
                goto error;
            }

            MenaiValue *_r = make_float(menai_float_value(a) / bv);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_FLOOR_DIV: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float//")) {
                goto error;
            }

            if (!require_float(b, "float//")) {
                goto error;
            }

            double bv = menai_float_value(b);
            if (bv == 0.0) {
                menai_raise_eval_error("Division by zero in 'float//'");
                goto error;
            }

            MenaiValue *_r = make_float(floor(menai_float_value(a) / bv));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_MOD: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float%")) {
                goto error;
            }

            if (!require_float(b, "float%")) {
                goto error;
            }

            double bv = menai_float_value(b);
            if (bv == 0.0) {
                menai_raise_eval_error("Modulo by zero in 'float%'");
                goto error;
            }

            MenaiValue *_r = make_float(fmod(menai_float_value(a), bv));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_EXP: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-exp")) {
                goto error;
            }

            MenaiValue *_r = make_float(exp(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_EXPN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float-expn")) {
                goto error;
            }

            if (!require_float(b, "float-expn")) {
                goto error;
            }

            MenaiValue *_r = make_float(pow(menai_float_value(a), menai_float_value(b)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_LOG: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-log")) {
                goto error;
            }

            double v = menai_float_value(a);
            if (v < 0.0) {
                menai_raise_eval_error("float-log: argument must be non-negative");
                goto error;
            }

            MenaiValue *_r = make_float(v == 0.0 ? -INFINITY : log(v));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_LOG10: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-log10")) {
                goto error;
            }

            double v = menai_float_value(a);
            if (v < 0.0) {
                menai_raise_eval_error("float-log10: argument must be non-negative");
                goto error;
            }

            MenaiValue *_r = make_float(v == 0.0 ? -INFINITY : log10(v));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_LOG2: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-log2")) {
                goto error;
            }

            double v = menai_float_value(a);
            if (v < 0.0) {
                menai_raise_eval_error("float-log2: argument must be non-negative");
                goto error;
            }

            MenaiValue *_r = make_float(v == 0.0 ? -INFINITY : log2(v));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_LOGN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float-logn")) {
                goto error;
            }

            if (!require_float(b, "float-logn")) {
                goto error;
            }

            double av = menai_float_value(a), bv = menai_float_value(b);
            if (bv <= 0.0 || bv == 1.0) {
                menai_raise_eval_error("Function 'float-logn' requires a positive base not equal to 1");
                goto error;
            }

            if (av < 0.0) {
                menai_raise_eval_error("float-logn: argument must be non-negative");
                goto error;
            }

            MenaiValue *_r = make_float(av == 0.0 ? -INFINITY : log(av) / log(bv));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_SIN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-sin")) {
                goto error;
            }

            MenaiValue *_r = make_float(sin(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_COS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-cos")) {
                goto error;
            }

            MenaiValue *_r = make_float(cos(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_TAN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-tan")) {
                goto error;
            }

            MenaiValue *_r = make_float(tan(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_SQRT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-sqrt")) {
                goto error;
            }

            double v = menai_float_value(a);
            if (v < 0.0) {
                menai_raise_eval_error("float-sqrt: argument must be non-negative");
                goto error;
            }

            MenaiValue *_r = make_float(sqrt(v));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_FLOOR: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-floor")) {
                goto error;
            }

            MenaiValue *_r = make_float(floor(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_CEIL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-ceil")) {
                goto error;
            }

            MenaiValue *_r = make_float(ceil(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_ROUND: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float-round")) {
                goto error;
            }

            MenaiValue *_r = make_float(round(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_MIN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float-min")) {
                goto error;
            }

            if (!require_float(b, "float-min")) {
                goto error;
            }

            double av = menai_float_value(a), bv = menai_float_value(b);
            MenaiValue *_r = make_float(av <= bv ? av : bv);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_MAX: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float-max")) {
                goto error;
            }

            if (!require_float(b, "float-max")) {
                goto error;
            }

            double av = menai_float_value(a), bv = menai_float_value(b);
            MenaiValue *_r = make_float(av >= bv ? av : bv);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_TO_INTEGER: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float->integer")) {
                goto error;
            }

            double v = menai_float_value(a);
            MenaiBigInt res;
            menai_bigint_init(&res);
            if (menai_bigint_from_double(trunc(v), &res) < 0) {
                goto error;
            }

            MenaiValue *_r = menai_integer_from_bigint(res);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_TO_COMPLEX: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_float(a, "float->complex")) {
                goto error;
            }

            if (!require_float(b, "float->complex")) {
                goto error;
            }

            MenaiValue *r = make_complex(menai_float_value(a), menai_float_value(b));
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_FLOAT_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_float(a, "float->string")) {
                goto error;
            }

            MenaiValue *r = menai_format_float(menai_float_value(a));
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_MAKE_CLOSURE: {
            /*
             * MAKE_CLOSURE dest, src0:
             * src0 is the index into code_objects of the child CodeObject.
             * Creates a MenaiFunction with capture slots initialised to None,
             * ready for PATCH_CLOSURE to fill in.
             */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            if (src0 >= (int)frame->nchildren) {
                menai_raise_eval_error("MAKE_CLOSURE: child index out of range");
                goto error;
            }

            MenaiValue *func = menai_function_alloc(frame->children[src0], Menai_NONE);
            if (func == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, func);
            break;
        }

        case OP_PATCH_CLOSURE: {
            /*
             * PATCH_CLOSURE src0, src1, src2:
             * src0 = closure register, src1 = capture slot index, src2 = value register.
             */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *closure = regs[base + src0];
            if (!IS_MENAI_FUNCTION(closure)) {
                menai_raise_eval_error("PATCH_CLOSURE requires a function");
                goto error;
            }

            MenaiValue *val = regs[base + src2];
            MenaiFunction *fn = (MenaiFunction *)closure;
            MenaiValue *old = fn->captures[src1];
            menai_retain(val);
            fn->captures[src1] = val;
            menai_release(old);
            break;
        }

        case OP_COMPLEX_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_COMPLEX(regs[base + src0]));
            break;
        }

        case OP_COMPLEX_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_complex(a, "complex=?")) {
                goto error;
            }

            if (!require_complex(b, "complex=?")) {
                goto error;
            }

            bool_store(regs, base + dest,
                ((MenaiComplex *)a)->real == ((MenaiComplex *)b)->real &&
                ((MenaiComplex *)a)->imag == ((MenaiComplex *)b)->imag);
            break;
        }

        case OP_COMPLEX_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_complex(a, "complex!=?")) {
                goto error;
            }

            if (!require_complex(b, "complex!=?")) {
                goto error;
            }

            bool_store(regs, base + dest,
                ((MenaiComplex *)a)->real != ((MenaiComplex *)b)->real ||
                ((MenaiComplex *)a)->imag != ((MenaiComplex *)b)->imag);
            break;
        }

        case OP_COMPLEX_REAL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex-real")) {
                goto error;
            }

            MenaiValue *_fr = make_float(((MenaiComplex *)a)->real);
            if (_fr == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_IMAG: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex-imag")) {
                goto error;
            }

            MenaiValue *_fr = make_float(((MenaiComplex *)a)->imag);
            if (_fr == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_ABS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex-abs")) {
                goto error;
            }

            double re = ((MenaiComplex *)a)->real;
            double im = ((MenaiComplex *)a)->imag;
            MenaiValue *_fr = make_float(sqrt(re * re + im * im));
            if (_fr == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_NEG: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex-neg")) {
                goto error;
            }

            MenaiValue *_r = make_complex(-((MenaiComplex *)a)->real,
                                        -((MenaiComplex *)a)->imag);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_ADD: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_complex(a, "complex+")) {
                goto error;
            }

            if (!require_complex(b, "complex+")) {
                goto error;
            }

            MenaiValue *_r = make_complex(
                ((MenaiComplex *)a)->real + ((MenaiComplex *)b)->real,
                ((MenaiComplex *)a)->imag + ((MenaiComplex *)b)->imag);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SUB: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_complex(a, "complex-")) {
                goto error;
            }

            if (!require_complex(b, "complex-")) {
                goto error;
            }

            MenaiValue *_r = make_complex(
                ((MenaiComplex *)a)->real - ((MenaiComplex *)b)->real,
                ((MenaiComplex *)a)->imag - ((MenaiComplex *)b)->imag);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_MUL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_complex(a, "complex*")) {
                goto error;
            }

            if (!require_complex(b, "complex*")) {
                goto error;
            }

            double ar = ((MenaiComplex *)a)->real, ai = ((MenaiComplex *)a)->imag;
            double br = ((MenaiComplex *)b)->real, bi = ((MenaiComplex *)b)->imag;
            MenaiValue *_r = make_complex(ar * br - ai * bi, ar * bi + ai * br);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_DIV: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_complex(a, "complex/")) {
                goto error;
            }

            if (!require_complex(b, "complex/")) {
                goto error;
            }

            double ar = ((MenaiComplex *)a)->real, ai = ((MenaiComplex *)a)->imag;
            double br = ((MenaiComplex *)b)->real, bi = ((MenaiComplex *)b)->imag;
            if (br == 0.0 && bi == 0.0) {
                menai_raise_eval_error("Division by zero in 'complex/'");
                goto error;
            }

            double denom = br * br + bi * bi;
            MenaiValue *_r = make_complex(
                (ar * br + ai * bi) / denom,
                (ai * br - ar * bi) / denom);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_EXPN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_complex(a, "complex-expn")) {
                goto error;
            }

            if (!require_complex(b, "complex-expn")) {
                goto error;
            }

            mc_t za = mc(((MenaiComplex *)a)->real, ((MenaiComplex *)a)->imag);
            mc_t zb = mc(((MenaiComplex *)b)->real, ((MenaiComplex *)b)->imag);
            mc_t cr = mc_pow(za, zb);
            MenaiValue *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_EXP: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex-exp")) {
                goto error;
            }

            mc_t z = mc(((MenaiComplex *)a)->real, ((MenaiComplex *)a)->imag);
            mc_t cr = mc_exp(z);
            MenaiValue *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOG: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex-log")) {
                goto error;
            }

            mc_t z = mc(((MenaiComplex *)a)->real, ((MenaiComplex *)a)->imag);
            mc_t cr = mc_log(z);
            MenaiValue *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOG10: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex-log10")) {
                goto error;
            }

            mc_t z = mc(((MenaiComplex *)a)->real, ((MenaiComplex *)a)->imag);
            mc_t cr = mc_log10(z);
            MenaiValue *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SIN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex-sin")) {
                goto error;
            }

            mc_t z = mc(((MenaiComplex *)a)->real, ((MenaiComplex *)a)->imag);
            mc_t cr = mc_sin(z);
            MenaiValue *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_COS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex-cos")) {
                goto error;
            }

            mc_t z = mc(((MenaiComplex *)a)->real, ((MenaiComplex *)a)->imag);
            mc_t cr = mc_cos(z);
            MenaiValue *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_TAN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex-tan")) {
                goto error;
            }

            mc_t z = mc(((MenaiComplex *)a)->real, ((MenaiComplex *)a)->imag);
            mc_t cr = mc_tan(z);
            MenaiValue *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SQRT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex-sqrt")) {
                goto error;
            }

            mc_t z = mc(((MenaiComplex *)a)->real, ((MenaiComplex *)a)->imag);
            mc_t cr = mc_sqrt(z);
            MenaiValue *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOGN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_complex(a, "complex-logn")) {
                goto error;
            }

            if (!require_complex(b, "complex-logn")) {
                goto error;
            }

            mc_t za = mc(((MenaiComplex *)a)->real, ((MenaiComplex *)a)->imag);
            mc_t zb = mc(((MenaiComplex *)b)->real, ((MenaiComplex *)b)->imag);
            if (mc_zero(zb)) {
                menai_raise_eval_error("Function 'complex-logn' requires a non-zero base");
                goto error;
            }

            mc_t cr = mc_logn(za, zb);
            MenaiValue *_r = make_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_complex(a, "complex->string")) {
                goto error;
            }

            MenaiComplex *c = (MenaiComplex *)a;
            MenaiValue *r = menai_format_complex(c->real, c->imag);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_STRING(regs[base + src0]));
            break;
        }

        case OP_STRING_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string=?")) {
                goto error;
            }

            if (!require_string(b, "string=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_string_equal(a, b));
            break;
        }

        case OP_STRING_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string!=?")) {
                goto error;
            }

            if (!require_string(b, "string!=?")) {
                goto error;
            }

            bool_store(regs, base + dest, !menai_string_equal(a, b));
            break;
        }

        case OP_STRING_LT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string<?")) {
                goto error;
            }

            if (!require_string(b, "string<?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_string_compare(a, b) < 0);
            break;
        }

        case OP_STRING_GT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string>?")) {
                goto error;
            }

            if (!require_string(b, "string>?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_string_compare(a, b) > 0);
            break;
        }

        case OP_STRING_LTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string<=?")) {
                goto error;
            }

            if (!require_string(b, "string<=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_string_compare(a, b) <= 0);
            break;
        }

        case OP_STRING_GTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string>=?")) {
                goto error;
            }

            if (!require_string(b, "string>=?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_string_compare(a, b) >= 0);
            break;
        }

        case OP_STRING_LENGTH: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_string(a, "string-length")) {
                goto error;
            }

            MenaiValue *_r = make_integer_from_ssize_t(menai_string_length(a));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_STRING_UPCASE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_string(a, "string-upcase")) {
                goto error;
            }

            MenaiValue *r = menai_string_upcase(a);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_DOWNCASE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_string(a, "string-downcase")) {
                goto error;
            }

            MenaiValue *r = menai_string_downcase(a);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_TRIM: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_string(a, "string-trim")) {
                goto error;
            }

            MenaiValue *r = menai_string_trim(a);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_TRIM_LEFT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_string(a, "string-trim-left")) {
                goto error;
            }

            MenaiValue *r = menai_string_trim_left(a);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_TRIM_RIGHT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_string(a, "string-trim-right")) {
                goto error;
            }

            MenaiValue *r = menai_string_trim_right(a);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_CONCAT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string-concat")) {
                goto error;
            }

            if (!require_string(b, "string-concat")) {
                goto error;
            }

            MenaiValue *r = menai_string_concat(a, b);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_PREFIX_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string-prefix?")) {
                goto error;
            }

            if (!require_string(b, "string-prefix?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_string_has_prefix(a, b));
            break;
        }

        case OP_STRING_SUFFIX_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string-suffix?")) {
                goto error;
            }

            if (!require_string(b, "string-suffix?")) {
                goto error;
            }

            bool_store(regs, base + dest, menai_string_has_suffix(a, b));
            break;
        }

        case OP_STRING_REF: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string-ref")) {
                goto error;
            }

            if (!IS_MENAI_INTEGER(b)) {
                menai_raise_eval_error("string-ref: index must be integer");
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            long idx_l;
            if (!ib->is_big) {
                idx_l = ib->small;
            } else {
                if (menai_bigint_to_long(&ib->big, &idx_l) < 0) {
                    goto error;
                }
            }

            ssize_t idx = (ssize_t)idx_l;
            ssize_t slen = menai_string_length(a);
            if (idx < 0 || idx >= slen) {
                menai_raise_eval_errorf("string-ref index out of range: %zd", idx);
                goto error;
            }

            MenaiValue *r = menai_string_ref(a, idx);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_SLICE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            MenaiValue *c = regs[base + src2];
            if (!require_string(a, "string-slice")) {
                goto error;
            }

            if (!IS_MENAI_INTEGER(b) || !IS_MENAI_INTEGER(c)) {
                menai_raise_eval_error("string-slice: indices must be integers");
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            MenaiInteger *ic = (MenaiInteger *)c;
            long start_l, end_l;
            if (!ib->is_big) {
                start_l = ib->small;
            } else {
                if (menai_bigint_to_long(&ib->big, &start_l) < 0) {
                    goto error;
                }
            }

            if (!ic->is_big) {
                end_l = ic->small;
            } else {
                if (menai_bigint_to_long(&ic->big, &end_l) < 0) {
                    goto error;
                }
            }

            ssize_t start = (ssize_t)start_l, end = (ssize_t)end_l;
            ssize_t slen = menai_string_length(a);
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

            MenaiValue *r = menai_string_slice(a, start, end);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_REPLACE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            MenaiValue *c = regs[base + src2];
            if (!require_string(a, "string-replace")) {
                goto error;
            }

            if (!require_string(b, "string-replace")) {
                goto error;
            }

            if (!require_string(c, "string-replace")) {
                goto error;
            }

            MenaiValue *r = menai_string_replace(a, b, c);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRING_INDEX: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string-index")) {
                goto error;
            }

            if (!require_string(b, "string-index")) {
                goto error;
            }

            ssize_t idx = menai_string_find(a, b);
            if (idx == -2) {
                goto error;
            }

            if (idx == -1) {
                menai_reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                MenaiValue *_r = make_integer_from_ssize_t(idx);
                if (_r == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
            }

            break;
        }

        case OP_STRING_TO_INTEGER_CODEPOINT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_string(a, "string->integer-codepoint")) {
                goto error;
            }

            ssize_t slen = menai_string_length(a);
            if (slen != 1) {
                menai_raise_eval_error("string->integer-codepoint: requires single-character string");
                goto error;
            }

            MenaiValue *_r = make_integer_from_long((long)menai_string_get(a, 0));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_STRING_TO_INTEGER: {
            /* src0=string, src1=radix(integer) */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string->integer")) {
                goto error;
            }

            if (!IS_MENAI_INTEGER(b)) {
                menai_raise_eval_error("string->integer: radix must be integer");
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            long radix;
            if (!ib->is_big) {
                radix = ib->small;
            } else {
                if (menai_bigint_to_long(&ib->big, &radix) < 0) {
                    goto error;
                }
            }

            if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
                menai_raise_eval_errorf("string->integer radix must be 2, 8, 10, or 16, got %ld", radix);
                goto error;
            }

            MenaiValue *trimmed = menai_string_trim(a);
            if (trimmed == NULL) {
                goto error;
            }

            MenaiBigInt sti_tmp;
            menai_bigint_init(&sti_tmp);
            int sti_ok = menai_bigint_from_codepoints(
                menai_string_data(trimmed),
                menai_string_length(trimmed),
                (int)radix, &sti_tmp);
            menai_release(trimmed);
            if (sti_ok < 0) {
                PyErr_Clear();
                menai_reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                MenaiValue *_r = menai_integer_from_bigint(sti_tmp);
                if (_r == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
            }

            break;
        }

        case OP_STRING_TO_NUMBER: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_string(a, "string->number")) {
                goto error;
            }

            ssize_t slen = menai_string_length(a);
            const uint32_t *sdata = menai_string_data(a);

            /*
             * Copy codepoints to a stack-allocated ASCII buffer.
             * Any non-ASCII codepoint means the string cannot be a number.
             * The buffer limit of 64 is generous for any valid numeric literal.
             */
            char stn_buf[64];
            if (slen >= (ssize_t)(sizeof(stn_buf))) {
                menai_reg_set_borrow(regs, base + dest, Menai_NONE);
                break;
            }

            int stn_ascii_ok = 1;
            int stn_has_j = 0;
            for (ssize_t _i = 0; _i < slen; _i++) {
                if (sdata[_i] > 0x7F) {
                    stn_ascii_ok = 0;
                    break;
                }

                stn_buf[_i] = (char)sdata[_i];
                if (sdata[_i] == 'j' || sdata[_i] == 'J') {
                    stn_has_j = 1;
                }
            }

            stn_buf[slen] = '\0';

            if (!stn_ascii_ok) {
                menai_reg_set_borrow(regs, base + dest, Menai_NONE);
                break;
            }

            /* Try integer first: fast path for the common case. */
            MenaiBigInt stn_tmp;
            menai_bigint_init(&stn_tmp);
            if (menai_bigint_from_codepoints(sdata, slen, 10, &stn_tmp) == 0) {
                MenaiValue *r = menai_integer_from_bigint(stn_tmp);
                if (r == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, r);
                break;
            }

            PyErr_Clear();
            /* Try complex if the string contains 'j' or 'J'. */
            if (stn_has_j) {
                double stn_re, stn_im;
                if (parse_complex_string(stn_buf, &stn_re, &stn_im)) {
                    MenaiValue *r = make_complex(stn_re, stn_im);
                    if (r == NULL) {
                        goto error;
                    }

                    menai_reg_set_own(regs, base + dest, r);
                    break;
                }
            }

            /* Try float. */
            char *stn_end = NULL;
            double stn_dv = strtod(stn_buf, &stn_end);
            int stn_ok = (stn_end != stn_buf && *stn_end == '\0');
            if (stn_ok) {
                MenaiValue *_r = make_float(stn_dv);
                if (_r == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
            } else {
                menai_reg_set_borrow(regs, base + dest, Menai_NONE);
            }

            break;
        }

        case OP_STRING_TO_LIST: {
            /* src0=string, src1=delimiter string */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_string(a, "string->list")) {
                goto error;
            }

            if (!require_string(b, "string->list")) {
                goto error;
            }

            ssize_t alen = menai_string_length(a);
            ssize_t blen = menai_string_length(b);
            const uint32_t *adata = menai_string_data(a);
            const uint32_t *bdata = menai_string_data(b);
            MenaiValue *r;
            if (blen == 0) {
                /* Split into individual codepoints */
                MenaiValue **stl_arr = alen > 0
                    ? (MenaiValue **)menai_alloc((size_t)alen * sizeof(MenaiValue *)) : NULL;

                if (alen > 0 && !stl_arr) {
                    PyErr_NoMemory();
                    goto error;
                }

                for (ssize_t i = 0; i < alen; i++) {
                    stl_arr[i] = menai_string_from_codepoint(adata[i]);
                    if (!stl_arr[i]) {
                        for (ssize_t k = 0; k < i; k++) {
                            menai_release(stl_arr[k]);
                        }

                        menai_free(stl_arr, (size_t)alen * sizeof(MenaiValue *));
                        goto error;
                    }
                }

                r = menai_list_from_array_steal(stl_arr, alen);
            } else {
                /* Split on delimiter — find occurrences and build list */
                ssize_t count = 0;
                for (ssize_t i = 0; i <= alen - blen; ) {
                    if (memcmp(adata + i, bdata, (size_t)blen * sizeof(uint32_t)) == 0) {
                        count++;
                        i += blen;
                    } else {
                        i++;
                    }
                }

                ssize_t nparts = count + 1;
                MenaiValue **parts2 = (MenaiValue **)menai_alloc((size_t)nparts * sizeof(MenaiValue *));
                if (!parts2) {
                    PyErr_NoMemory();
                    goto error;
                }

                ssize_t seg_start = 0, pi2 = 0;
                for (ssize_t i = 0; i <= alen; ) {
                    int match = (i <= alen - blen) &&
                        (memcmp(adata + i, bdata, (size_t)blen * sizeof(uint32_t)) == 0);
                    if (match || i == alen) {
                        parts2[pi2] = menai_string_from_codepoints(adata + seg_start, i - seg_start);
                        if (!parts2[pi2]) {
                            for (ssize_t k = 0; k < pi2; k++) {
                                menai_release(parts2[k]);
                            }

                            menai_free(parts2, (size_t)nparts * sizeof(MenaiValue *));
                            goto error;
                        }

                        pi2++;
                        if (match) {
                            seg_start = i + blen; i += blen;
                        } else {
                            break;
                        }
                    } else {
                        i++;
                    }
                }

                r = menai_list_from_array_steal(parts2, pi2);
            }

            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_LIST(regs[base + src0]));
            break;
        }

        case OP_LIST_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_list(a, "list=?")) {
                goto error;
            }

            if (!require_list(b, "list=?")) {
                goto error;
            }

            int eq = menai_value_equal(a, b);
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_LIST_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_list(a, "list!=?")) {
                goto error;
            }

            if (!require_list(b, "list!=?")) {
                goto error;
            }

            int eq = menai_value_equal(a, b);
            bool_store(regs, base + dest, !eq);
            break;
        }

        case OP_LIST_NULL_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_list(a, "list-null?")) {
                goto error;
            }

            int is_null = (((MenaiList *)a)->length == 0);
            bool_store(regs, base + dest, is_null);
            break;
        }

        case OP_LIST_LENGTH: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_list(a, "list-length")) {
                goto error;
            }

            ssize_t n = ((MenaiList *)a)->length;
            MenaiValue *_r = make_integer_from_ssize_t(n);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_LIST_FIRST: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_list(a, "list-first")) {
                goto error;
            }

            MenaiList *lst_f = (MenaiList *)a;
            if (lst_f->length == 0) {
                menai_raise_eval_error("Function 'list-first' requires a non-empty list");
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, lst_f->elements[0]);
            break;
        }

        case OP_LIST_REST: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_list(a, "list-rest")) {
                goto error;
            }

            if (((MenaiList *)a)->length == 0) {
                menai_raise_eval_error("Function 'list-rest' requires a non-empty list");
                goto error;
            }

            MenaiValue *r = menai_list_rest(a);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_LAST: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_list(a, "list-last")) {
                goto error;
            }

            MenaiList *lst_l = (MenaiList *)a;
            ssize_t n = lst_l->length;
            if (n == 0) {
                menai_raise_eval_error("Function 'list-last' requires a non-empty list");
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, lst_l->elements[n - 1]);
            break;
        }

        case OP_LIST_REF: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_list(a, "list-ref")) {
                goto error;
            }

            if (!IS_MENAI_INTEGER(b)) {
                menai_raise_eval_error("list-ref: index must be integer");
                goto error;
            }

            MenaiList *lst_ref = (MenaiList *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            long idx_l;
            if (!ib->is_big) {
                idx_l = ib->small;
            } else {
                if (menai_bigint_to_long(&ib->big, &idx_l) < 0) {
                    goto error;
                }
            }

            ssize_t idx = (ssize_t)idx_l;
            ssize_t n = lst_ref->length;
            if (idx < 0 || idx >= n) {
                menai_raise_eval_errorf("list-ref: index out of range: %zd", idx);
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, lst_ref->elements[idx]);
            break;
        }

        case OP_LIST_PREPEND: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *item = regs[base + src1];
            if (!require_list(a, "list-prepend")) {
                goto error;
            }

            MenaiList *lst_pre = (MenaiList *)a;
            ssize_t n = lst_pre->length;
            MenaiValue **pre_arr = (MenaiValue **)menai_alloc((size_t)(n + 1) * sizeof(MenaiValue *));
            if (!pre_arr) {
                PyErr_NoMemory();
                goto error;
            }

            pre_arr[0] = item;
            menai_retain(item);
            for (ssize_t i = 0; i < n; i++) {
                pre_arr[i + 1] = lst_pre->elements[i];
                menai_retain(pre_arr[i + 1]);
            }

            MenaiValue *r = menai_list_from_array_steal(pre_arr, n + 1);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_APPEND: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *item = regs[base + src1];
            if (!require_list(a, "list-append")) {
                goto error;
            }

            MenaiList *lst_app = (MenaiList *)a;
            ssize_t n = lst_app->length;
            MenaiValue **app_arr = (MenaiValue **)menai_alloc((size_t)(n + 1) * sizeof(MenaiValue *));
            if (!app_arr) {
                PyErr_NoMemory();
                goto error;
            }

            for (ssize_t i = 0; i < n; i++) {
                app_arr[i] = lst_app->elements[i];
                menai_retain(app_arr[i]);
            }

            app_arr[n] = item;
            menai_retain(item);
            MenaiValue *r = menai_list_from_array_steal(app_arr, n + 1);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_REVERSE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_list(a, "list-reverse")) {
                goto error;
            }

            MenaiList *lst_rev = (MenaiList *)a;
            ssize_t n = lst_rev->length;
            MenaiValue **rev_arr = n > 0
                ? (MenaiValue **)menai_alloc((size_t)n * sizeof(MenaiValue *)) : NULL;

            if (n > 0 && !rev_arr) {
                PyErr_NoMemory();
                goto error;
            }

            for (ssize_t i = 0; i < n; i++) {
                rev_arr[i] = lst_rev->elements[n - 1 - i];
                menai_retain(rev_arr[i]);
            }

            MenaiValue *r = menai_list_from_array_steal(rev_arr, n);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_CONCAT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_list(a, "list-concat")) {
                goto error;
            }

            if (!require_list(b, "list-concat")) {
                goto error;
            }

            MenaiList *lst_ca = (MenaiList *)a;
            MenaiList *lst_cb = (MenaiList *)b;
            ssize_t na = lst_ca->length, nb = lst_cb->length;
            ssize_t nc = na + nb;
            MenaiValue **cat_arr = nc > 0
                ? (MenaiValue **)menai_alloc((size_t)nc * sizeof(MenaiValue *)) : NULL;

            if (nc > 0 && !cat_arr) {
                PyErr_NoMemory();
                goto error;
            }

            for (ssize_t i = 0; i < na; i++) {
                cat_arr[i] = lst_ca->elements[i];
                menai_retain(cat_arr[i]);
            }

            for (ssize_t i = 0; i < nb; i++) {
                cat_arr[na + i] = lst_cb->elements[i];
                menai_retain(cat_arr[na + i]);
            }

            MenaiValue *r = menai_list_from_array_steal(cat_arr, nc);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_MEMBER_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *item = regs[base + src1];
            if (!require_list(a, "list-member?")) {
                goto error;
            }

            MenaiList *lst_mem = (MenaiList *)a;
            int mem_found = 0;
            for (ssize_t i = 0; i < lst_mem->length; i++) {
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
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *item = regs[base + src1];
            if (!require_list(a, "list-index")) {
                goto error;
            }

            MenaiList *lst_idx = (MenaiList *)a;
            ssize_t n = lst_idx->length;
            ssize_t found = -1;
            for (ssize_t i = 0; i < n; i++) {
                int eq = menai_value_equal(lst_idx->elements[i], item);
                if (eq) {
                    found = i;
                    break;
                }
            }

            if (found == -1) {
                menai_reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                MenaiValue *_r = make_integer_from_ssize_t(found);
                if (_r == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
            }

            break;
        }

        case OP_LIST_SLICE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            MenaiValue *c = regs[base + src2];
            if (!require_list(a, "list-slice")) {
                goto error;
            }

            if (!IS_MENAI_INTEGER(b) || !IS_MENAI_INTEGER(c)) {
                menai_raise_eval_error("list-slice: indices must be integers");
                goto error;
            }

            MenaiList *lst_sl = (MenaiList *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            MenaiInteger *ic = (MenaiInteger *)c;
            long start_l, end_l;
            if (!ib->is_big) {
                start_l = ib->small;
            } else {
                if (menai_bigint_to_long(&ib->big, &start_l) < 0) {
                    goto error;
                }
            }

            if (!ic->is_big) {
                end_l = ic->small;
            } else {
                if (menai_bigint_to_long(&ic->big, &end_l) < 0) {
                    goto error;
                }
            }

            ssize_t start = (ssize_t)start_l, end = (ssize_t)end_l;
            ssize_t n = lst_sl->length;
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

            MenaiValue *r = menai_list_slice(a, start, end);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_REMOVE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *item = regs[base + src1];
            if (!require_list(a, "list-remove")) {
                goto error;
            }

            MenaiList *lst_rm = (MenaiList *)a;
            ssize_t n = lst_rm->length;
            /* Count non-matching elements first */
            ssize_t keep = 0;
            for (ssize_t i = 0; i < n; i++) {
                int eq = menai_value_equal(lst_rm->elements[i], item);
                if (!eq) {
                    keep++;
                }
            }

            MenaiValue **rm_arr = keep > 0 ? (MenaiValue **)menai_alloc((size_t)keep * sizeof(MenaiValue *)) : NULL;
            if (keep > 0 && !rm_arr) {
                PyErr_NoMemory();
                goto error;
            }

            ssize_t j = 0;
            for (ssize_t i = 0; i < n; i++) {
                MenaiValue *e = lst_rm->elements[i];
                int eq = menai_value_equal(e, item);
                if (!eq) {
                    menai_retain(e);
                    rm_arr[j++] = e;
                }
            }

            MenaiValue *r = menai_list_from_array_steal(rm_arr, keep);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_list(a, "list->string")) {
                goto error;
            }

            if (!require_string(b, "list->string")) {
                goto error;
            }

            MenaiList *lst_ts = (MenaiList *)a;
            ssize_t n = lst_ts->length;
            /* Validate all elements are strings first. */
            for (ssize_t i = 0; i < n; i++) {
                if (!IS_MENAI_STRING(lst_ts->elements[i])) {
                    menai_raise_eval_error("list->string: all elements must be strings");
                    goto error;
                }
            }

            /* Compute total output length. */
            ssize_t sep_len = menai_string_length(b);
            const uint32_t *sep_data = menai_string_data(b);
            ssize_t total = (n > 0) ? (n - 1) * sep_len : 0;
            for (ssize_t i = 0; i < n; i++) {
                total += menai_string_length(lst_ts->elements[i]);
            }

            uint32_t *lts_buf = total > 0 ? (uint32_t *)malloc((size_t)total * sizeof(uint32_t)) : NULL;
            if (total > 0 && !lts_buf) {
                goto error;
            }

            uint32_t *dst = lts_buf;
            for (ssize_t i = 0; i < n; i++) {
                if (i > 0 && sep_len > 0) {
                    memcpy(dst, sep_data, (size_t)sep_len * sizeof(uint32_t));
                    dst += sep_len;
                }

                ssize_t elen = menai_string_length(lst_ts->elements[i]);
                if (elen > 0) {
                    memcpy(dst, menai_string_data(lst_ts->elements[i]),
                           (size_t)elen * sizeof(uint32_t));
                    dst += elen;
                }
            }

            MenaiValue *r = menai_string_from_codepoints(lts_buf, total);
            free(lts_buf);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_TO_SET: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_list_singular(a, "list->set")) {
                goto error;
            }

            MenaiList *lst = (MenaiList *)a;
            ssize_t n = lst->length;
            MenaiValue **nelems = n > 0 ? (MenaiValue **)malloc(n * sizeof(MenaiValue *)) : NULL;
            Py_hash_t *nhashes = n > 0 ? (Py_hash_t *)malloc(n * sizeof(Py_hash_t)) : NULL;
            if (n > 0 && (!nelems || !nhashes)) {
                free(nelems);
                free(nhashes);
                PyErr_NoMemory();
                goto error;
            }

            MenaiHashTable lts_seen;
            int lts_err = 0;
            if (n > 0 && menai_ht_init(&lts_seen, n) < 0) {
                free(nelems);
                free(nhashes);
                goto error;
            }

            ssize_t out = 0;
            for (ssize_t i = 0; i < n && !lts_err; i++) {
                MenaiValue *elem = lst->elements[i];
                Py_hash_t h = menai_value_hash(elem);
                if (h == -1) {
                    lts_err = 1;
                    break;
                }

                ssize_t existing = menai_ht_lookup(&lts_seen, elem, h);
                if (existing == -2) {
                    lts_err = 1;
                    break;
                }

                if (existing < 0) {
                    menai_ht_insert(&lts_seen, elem, h, out);
                    menai_retain(elem);
                    nelems[out] = elem;
                    nhashes[out] = h;
                    out++;
                }
            }

            if (n > 0) {
                menai_ht_free(&lts_seen);
            }

            if (lts_err) {
                for (ssize_t k = 0; k < out; k++) {
                    menai_release(nelems[k]);
                }

                free(nelems);
                free(nhashes);
                goto error;
            }

            MenaiValue *r = menai_set_from_arrays_steal(nelems, nhashes, out);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_DICT(regs[base + src0]));
            break;
        }

        case OP_DICT_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_dict(a, "dict=?")) {
                goto error;
            }

            if (!require_dict(b, "dict=?")) {
                goto error;
            }

            MenaiDict *da = (MenaiDict *)a;
            MenaiDict *db = (MenaiDict *)b;
            int eq = (da->length == db->length);
            for (ssize_t i = 0; eq && i < da->length; i++) {
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
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_dict(a, "dict!=?")) {
                goto error;
            }

            if (!require_dict(b, "dict!=?")) {
                goto error;
            }

            MenaiDict *da = (MenaiDict *)a;
            MenaiDict *db = (MenaiDict *)b;
            int neq = (da->length != db->length);
            for (ssize_t i = 0; !neq && i < da->length; i++) {
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
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_dict(a, "dict-length")) {
                goto error;
            }

            MenaiValue *_r = make_integer_from_ssize_t(((MenaiDict *)a)->length);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_DICT_KEYS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_dict(a, "dict-keys")) {
                goto error;
            }

            MenaiDict *d = (MenaiDict *)a;
            ssize_t n = d->length;
            MenaiValue **dk_arr = n > 0 ? (MenaiValue **)menai_alloc((size_t)n * sizeof(MenaiValue *)) : NULL;

            if (n > 0 && !dk_arr) {
                PyErr_NoMemory();
                goto error;
            }

            for (ssize_t i = 0; i < n; i++) {
                menai_retain(d->keys[i]);
                dk_arr[i] = d->keys[i];
            }

            MenaiValue *r = menai_list_from_array_steal(dk_arr, n);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_VALUES: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_dict(a, "dict-values")) {
                goto error;
            }

            MenaiDict *d = (MenaiDict *)a;
            ssize_t n = d->length;
            MenaiValue **dv_arr = n > 0 ? (MenaiValue **)menai_alloc((size_t)n * sizeof(MenaiValue *)) : NULL;

            if (n > 0 && !dv_arr) {
                PyErr_NoMemory();
                goto error;
            }

            for (ssize_t i = 0; i < n; i++) {
                menai_retain(d->values[i]);
                dv_arr[i] = d->values[i];
            }

            MenaiValue *r = menai_list_from_array_steal(dv_arr, n);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_HAS_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *key = regs[base + src1];
            if (!require_dict(a, "dict-has?")) {
                goto error;
            }

            MenaiDict *d = (MenaiDict *)a;
            Py_hash_t h = menai_value_hash(key);
            if (h == -1) {
                goto error;
            }

            int has = (menai_ht_lookup(&d->ht, key, h) >= 0);
            bool_store(regs, base + dest, has);
            break;
        }

        case OP_DICT_GET: {
            /* src0=dict, src1=key, src2=default */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *key = regs[base + src1];
            MenaiValue *def = regs[base + src2];
            if (!require_dict(a, "dict-get")) {
                goto error;
            }

            MenaiDict *d = (MenaiDict *)a;
            Py_hash_t h = menai_value_hash(key);
            if (h == -1) {
                goto error;
            }

            ssize_t idx = menai_ht_lookup(&d->ht, key, h);
            if (idx == -2) {
                goto error;
            }

            if (idx >= 0) {
                menai_reg_set_borrow(regs, base + dest, d->values[idx]);
            } else {
                menai_reg_set_borrow(regs, base + dest, def);
            }

            break;
        }

        case OP_DICT_SET: {
            /* src0=dict, src1=key, src2=value */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *key = regs[base + src1];
            MenaiValue *val = regs[base + src2];
            if (!require_dict(a, "dict-set")) {
                goto error;
            }

            MenaiDict *d = (MenaiDict *)a;
            Py_hash_t h = menai_value_hash(key);
            if (h == -1) {
                goto error;
            }

            ssize_t replace_idx = menai_ht_lookup(&d->ht, key, h);
            if (replace_idx == -2) {
                goto error;
            }

            ssize_t n = d->length;
            ssize_t new_n = (replace_idx >= 0) ? n : n + 1;
            MenaiValue **nkeys = (MenaiValue **)malloc(new_n * sizeof(MenaiValue *));
            MenaiValue **nvals = (MenaiValue **)malloc(new_n * sizeof(MenaiValue *));
            Py_hash_t *nhashes = (Py_hash_t *)malloc(new_n * sizeof(Py_hash_t));
            if (!nkeys || !nvals || !nhashes) {
                free(nkeys);
                free(nvals);
                free(nhashes);
                PyErr_NoMemory();
                goto error;
            }

            if (replace_idx >= 0) {
                for (ssize_t i = 0; i < n; i++) {
                    if (i == replace_idx) {
                        menai_retain(key);
                        nkeys[i] = key;
                        menai_retain(val);
                        nvals[i] = val;
                        nhashes[i] = h;
                    } else {
                        menai_retain(d->keys[i]);
                        nkeys[i] = d->keys[i];
                        menai_retain(d->values[i]);
                        nvals[i] = d->values[i];
                        nhashes[i] = d->hashes[i];
                    }
                }
            } else {
                for (ssize_t i = 0; i < n; i++) {
                    menai_retain(d->keys[i]);
                    nkeys[i] = d->keys[i];
                    menai_retain(d->values[i]);
                    nvals[i] = d->values[i];
                    nhashes[i] = d->hashes[i];
                }

                menai_retain(key);
                nkeys[n] = key;
                menai_retain(val);
                nvals[n] = val;
                nhashes[n] = h;
            }

            MenaiValue *result = menai_dict_from_arrays_steal(nkeys, nvals, nhashes, new_n);
            if (result == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, result);
            break;
        }

        case OP_DICT_REMOVE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *key = regs[base + src1];
            if (!require_dict(a, "dict-remove")) {
                goto error;
            }

            MenaiDict *d = (MenaiDict *)a;
            Py_hash_t h = menai_value_hash(key);
            if (h == -1) {
                goto error;
            }

            ssize_t remove_idx = menai_ht_lookup(&d->ht, key, h);
            if (remove_idx == -2) {
                goto error;
            }

            if (remove_idx < 0) {
                menai_reg_set_borrow(regs, base + dest, a);
                break;
            }

            ssize_t n = d->length;
            ssize_t new_n = n - 1;
            MenaiValue **nkeys = new_n > 0 ? (MenaiValue **)malloc(new_n * sizeof(MenaiValue *)) : NULL;
            MenaiValue **nvals = new_n > 0 ? (MenaiValue **)malloc(new_n * sizeof(MenaiValue *)) : NULL;
            Py_hash_t *nhashes = new_n > 0 ? (Py_hash_t *)malloc(new_n * sizeof(Py_hash_t)) : NULL;
            if (new_n > 0 && (!nkeys || !nvals || !nhashes)) {
                free(nkeys);
                free(nvals);
                free(nhashes);
                PyErr_NoMemory();
                goto error;
            }

            for (ssize_t i = 0, j = 0; i < n; i++) {
                if (i == remove_idx) {
                    continue;
                }

                menai_retain(d->keys[i]);
                nkeys[j] = d->keys[i];
                menai_retain(d->values[i]);
                nvals[j] = d->values[i];
                nhashes[j] = d->hashes[i];
                j++;
            }

            MenaiValue *r = menai_dict_from_arrays_steal(nkeys, nvals, nhashes, new_n);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_MERGE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_dict(a, "dict-merge")) {
                goto error;
            }

            if (!require_dict(b, "dict-merge")) {
                goto error;
            }

            MenaiDict *da = (MenaiDict *)a;
            MenaiDict *db = (MenaiDict *)b;
            ssize_t na = da->length, nb = db->length;
            ssize_t cap = na + nb;
            MenaiValue **nkeys = cap > 0 ? (MenaiValue **)malloc(cap * sizeof(MenaiValue *)) : NULL;
            MenaiValue **nvals = cap > 0 ? (MenaiValue **)malloc(cap * sizeof(MenaiValue *)) : NULL;
            Py_hash_t *nhashes = cap > 0 ? (Py_hash_t *)malloc(cap * sizeof(Py_hash_t)) : NULL;
            if (cap > 0 && (!nkeys || !nvals || !nhashes)) {
                free(nkeys);
                free(nvals);
                free(nhashes);
                PyErr_NoMemory();
                goto error;
            }

            ssize_t out = 0;
            /* Add a's entries, using b's value where b overrides */
            for (ssize_t i = 0; i < na; i++) {
                ssize_t bi = menai_ht_lookup(&db->ht, da->keys[i], da->hashes[i]);
                if (bi == -2) {
                    for (ssize_t k = 0; k < out; k++) {
                        menai_release(nkeys[k]);
                        menai_release(nvals[k]);
                    }

                    free(nkeys);
                    free(nvals);
                    free(nhashes);
                    goto error;
                }

                menai_retain(da->keys[i]);
                nkeys[out] = da->keys[i];
                nhashes[out] = da->hashes[i];
                if (bi >= 0) {
                    menai_retain(db->values[bi]);
                    nvals[out] = db->values[bi];
                } else {
                    menai_retain(da->values[i]);
                    nvals[out] = da->values[i];
                }

                out++;
            }

            /* Add b's entries not in a */
            for (ssize_t i = 0; i < nb; i++) {
                ssize_t ai = menai_ht_lookup(&da->ht, db->keys[i], db->hashes[i]);
                if (ai == -2) {
                    for (ssize_t k = 0; k < out; k++) {
                        menai_release(nkeys[k]);
                        menai_release(nvals[k]);
                    }

                    free(nkeys);
                    free(nvals);
                    free(nhashes);
                    goto error;
                }

                if (ai < 0) {
                    menai_retain(db->keys[i]);
                    nkeys[out] = db->keys[i];
                    menai_retain(db->values[i]);
                    nvals[out] = db->values[i];
                    nhashes[out] = db->hashes[i];
                    out++;
                }
            }

            MenaiValue *r = menai_dict_from_arrays_steal(nkeys, nvals, nhashes, out);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_SET(regs[base + src0]));
            break;
        }

        case OP_SET_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_set(a, "set=?")) {
                goto error;
            }

            if (!require_set(b, "set=?")) {
                goto error;
            }

            MenaiSet *sa = (MenaiSet *)a;
            MenaiSet *sb = (MenaiSet *)b;
            int eq = (sa->length == sb->length);
            for (ssize_t i = 0; eq && i < sa->length; i++) {
                ssize_t idx = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (idx == -2) {
                    goto error;
                }

                if (idx < 0) {
                    eq = 0;
                    break;
                }
            }

            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_SET_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_set(a, "set!=?")) {
                goto error;
            }

            if (!require_set(b, "set!=?")) {
                goto error;
            }

            MenaiSet *sa = (MenaiSet *)a;
            MenaiSet *sb = (MenaiSet *)b;
            int neq = (sa->length != sb->length);
            for (ssize_t i = 0; !neq && i < sa->length; i++) {
                ssize_t idx = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (idx == -2) {
                    goto error;
                }

                if (idx < 0) {
                    neq = 1;
                    break;
                }
            }

            bool_store(regs, base + dest, neq);
            break;
        }

        case OP_SET_LENGTH: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_set_singular(a, "set-length")) {
                goto error;
            }

            MenaiValue *_r = make_integer_from_ssize_t(((MenaiSet *)a)->length);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_SET_MEMBER_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *item = regs[base + src1];
            if (!require_set_singular(a, "set-member?")) {
                goto error;
            }

            MenaiSet *s = (MenaiSet *)a;
            Py_hash_t h = menai_value_hash(item);
            if (h == -1) {
                goto error;
            }

            ssize_t idx = menai_ht_lookup(&s->ht, item, h);
            if (idx == -2) {
                goto error;
            }

            bool_store(regs, base + dest, idx >= 0);
            break;
        }

        case OP_SET_ADD: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *item = regs[base + src1];
            if (!require_set_singular(a, "set-add")) {
                goto error;
            }

            MenaiSet *s = (MenaiSet *)a;
            Py_hash_t h = menai_value_hash(item);
            if (h == -1) {
                goto error;
            }

            ssize_t existing = menai_ht_lookup(&s->ht, item, h);
            if (existing == -2) {
                goto error;
            }

            if (existing >= 0) {
                menai_reg_set_borrow(regs, base + dest, a);
            } else {
                ssize_t n = s->length;
                MenaiValue **nelems = (MenaiValue **)malloc((n + 1) * sizeof(MenaiValue *));
                Py_hash_t *nhashes = (Py_hash_t *)malloc((n + 1) * sizeof(Py_hash_t));
                if (!nelems || !nhashes) {
                    free(nelems);
                    free(nhashes);
                    PyErr_NoMemory();
                    goto error;
                }

                for (ssize_t i = 0; i < n; i++) {
                    menai_retain(s->elements[i]);
                    nelems[i] = s->elements[i];
                    nhashes[i] = s->hashes[i];
                }

                menai_retain(item);
                nelems[n] = item;
                nhashes[n] = h;
                MenaiValue *r = menai_set_from_arrays_steal(nelems, nhashes, n + 1);
                if (r == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, r);
            }

            break;
        }

        case OP_SET_REMOVE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *item = regs[base + src1];
            if (!require_set_singular(a, "set-remove")) {
                goto error;
            }

            MenaiSet *s = (MenaiSet *)a;
            Py_hash_t h = menai_value_hash(item);
            if (h == -1) {
                goto error;
            }

            ssize_t remove_idx = menai_ht_lookup(&s->ht, item, h);
            if (remove_idx == -2) {
                goto error;
            }

            if (remove_idx < 0) {
                menai_reg_set_borrow(regs, base + dest, a);
                break;
            }

            ssize_t n = s->length;
            ssize_t new_n = n - 1;
            MenaiValue **nelems = new_n > 0 ? (MenaiValue **)malloc(new_n * sizeof(MenaiValue *)) : NULL;
            Py_hash_t *nhashes = new_n > 0 ? (Py_hash_t *)malloc(new_n * sizeof(Py_hash_t)) : NULL;
            if (new_n > 0 && (!nelems || !nhashes)) {
                free(nelems);
                free(nhashes);
                PyErr_NoMemory();
                goto error;
            }

            for (ssize_t i = 0, j = 0; i < n; i++) {
                if (i == remove_idx) {
                    continue;
                }

                menai_retain(s->elements[i]);
                nelems[j] = s->elements[i];
                nhashes[j] = s->hashes[i];
                j++;
            }

            MenaiValue *r = menai_set_from_arrays_steal(nelems, nhashes, new_n);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_UNION: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_set(a, "set-union")) {
                goto error;
            }

            if (!require_set(b, "set-union")) {
                goto error;
            }

            MenaiSet *sa = (MenaiSet *)a;
            MenaiSet *sb = (MenaiSet *)b;
            ssize_t na = sa->length, nb = sb->length;
            ssize_t cap = na + nb;
            MenaiValue **nelems = cap > 0 ? (MenaiValue **)malloc(cap * sizeof(MenaiValue *)) : NULL;
            Py_hash_t *nhashes = cap > 0 ? (Py_hash_t *)malloc(cap * sizeof(Py_hash_t)) : NULL;
            if (cap > 0 && (!nelems || !nhashes)) {
                free(nelems);
                free(nhashes);
                PyErr_NoMemory();
                goto error;
            }

            ssize_t out = 0;
            for (ssize_t i = 0; i < na; i++) {
                menai_retain(sa->elements[i]);
                nelems[out] = sa->elements[i];
                nhashes[out] = sa->hashes[i];
                out++;
            }

            for (ssize_t i = 0; i < nb; i++) {
                ssize_t in_a = menai_ht_lookup(&sa->ht, sb->elements[i], sb->hashes[i]);
                if (in_a == -2) {
                    for (ssize_t k = 0; k < out; k++) {
                        menai_release(nelems[k]);
                    }

                    free(nelems);
                    free(nhashes);
                    goto error;
                }

                if (in_a < 0) {
                    menai_retain(sb->elements[i]);
                    nelems[out] = sb->elements[i];
                    nhashes[out] = sb->hashes[i];
                    out++;
                }
            }

            MenaiValue *r = menai_set_from_arrays_steal(nelems, nhashes, out);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_INTERSECTION: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_set(a, "set-intersection")) {
                goto error;
            }

            if (!require_set(b, "set-intersection")) {
                goto error;
            }

            MenaiSet *sa = (MenaiSet *)a;
            MenaiSet *sb = (MenaiSet *)b;
            ssize_t na = sa->length;
            MenaiValue **nelems = na > 0 ? (MenaiValue **)malloc(na * sizeof(MenaiValue *)) : NULL;
            Py_hash_t *nhashes = na > 0 ? (Py_hash_t *)malloc(na * sizeof(Py_hash_t)) : NULL;
            if (na > 0 && (!nelems || !nhashes)) {
                free(nelems);
                free(nhashes);
                PyErr_NoMemory();
                goto error;
            }

            ssize_t out = 0;
            for (ssize_t i = 0; i < na; i++) {
                ssize_t in_b = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (in_b == -2) {
                    for (ssize_t k = 0; k < out; k++) {
                        menai_release(nelems[k]);
                    }

                    free(nelems);
                    free(nhashes);
                    goto error;
                }

                if (in_b >= 0) {
                    menai_retain(sa->elements[i]);
                    nelems[out] = sa->elements[i];
                    nhashes[out] = sa->hashes[i];
                    out++;
                }
            }

            MenaiValue *r = menai_set_from_arrays_steal(nelems, nhashes, out);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_DIFFERENCE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_set(a, "set-difference")) {
                goto error;
            }

            if (!require_set(b, "set-difference")) {
                goto error;
            }

            MenaiSet *sa = (MenaiSet *)a;
            MenaiSet *sb = (MenaiSet *)b;
            ssize_t na = sa->length;
            MenaiValue **nelems = na > 0 ? (MenaiValue **)malloc(na * sizeof(MenaiValue *)) : NULL;
            Py_hash_t *nhashes = na > 0 ? (Py_hash_t *)malloc(na * sizeof(Py_hash_t)) : NULL;
            if (na > 0 && (!nelems || !nhashes)) {
                free(nelems);
                free(nhashes);
                PyErr_NoMemory();
                goto error;
            }

            ssize_t out = 0;
            for (ssize_t i = 0; i < na; i++) {
                ssize_t in_b = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (in_b == -2) {
                    for (ssize_t k = 0; k < out; k++) {
                        menai_release(nelems[k]);
                    }

                    free(nelems);
                    free(nhashes);
                    goto error;
                }

                if (in_b < 0) {
                    menai_retain(sa->elements[i]); nelems[out] = sa->elements[i];
                    nhashes[out] = sa->hashes[i];
                    out++;
                }
            }

            MenaiValue *r = menai_set_from_arrays_steal(nelems, nhashes, out);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_SUBSET_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_set(a, "set-subset?")) {
                goto error;
            }

            if (!require_set(b, "set-subset?")) {
                goto error;
            }

            MenaiSet *sa = (MenaiSet *)a;
            MenaiSet *sb = (MenaiSet *)b;
            if (sa->length > sb->length) {
                bool_store(regs, base + dest, 0);
                break;
            }

            int is_subset = 1;
            for (ssize_t i = 0; i < sa->length; i++) {
                ssize_t idx = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (idx == -2) {
                    goto error;
                }

                if (idx < 0) {
                    is_subset = 0;
                    break;
                }
            }

            bool_store(regs, base + dest, is_subset);
            break;
        }

        case OP_SET_TO_LIST: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (!require_set_singular(a, "set->list")) {
                goto error;
            }

            MenaiSet *s = (MenaiSet *)a;
            ssize_t set_n = s->length;
            MenaiValue **stl_arr = set_n > 0 ? (MenaiValue **)menai_alloc((size_t)set_n * sizeof(MenaiValue *)) : NULL;

            if (set_n > 0 && !stl_arr) {
                PyErr_NoMemory();
                goto error;
            }

            for (ssize_t i = 0; i < set_n; i++) {
                menai_retain(s->elements[i]);
                stl_arr[i] = s->elements[i];
            }

            MenaiValue *r = menai_list_from_array_steal(stl_arr, set_n);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_RANGE: {
            /* src0=start, src1=end, src2=step — all integers */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *ra = regs[base + src0];
            MenaiValue *rb = regs[base + src1];
            MenaiValue *rc = regs[base + src2];
            if (!IS_MENAI_INTEGER(ra) || !IS_MENAI_INTEGER(rb) || !IS_MENAI_INTEGER(rc)) {
                menai_raise_eval_error("range requires integer arguments");
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)ra;
            MenaiInteger *ib = (MenaiInteger *)rb;
            MenaiInteger *ic = (MenaiInteger *)rc;
            long start, end, step;
            if (!ia->is_big) {
                start = ia->small;
            } else {
                if (menai_bigint_to_long(&ia->big, &start) < 0) {
                    goto error;
                }
            }

            if (!ib->is_big) {
                end = ib->small;
            } else {
                if (menai_bigint_to_long(&ib->big, &end) < 0) {
                    goto error;
                }
            }

            if (!ic->is_big) {
                step = ic->small;
            } else {
                if (menai_bigint_to_long(&ic->big, &step) < 0) {
                    goto error;
                }
            }

            if (step == 0) {
                menai_raise_eval_error("range: step cannot be zero");
                goto error;
            }

            /* Compute length */
            ssize_t n = 0;
            if (step > 0 && end > start) {
                n = (end - start + step - 1) / step;
            } else if (step < 0 && end < start) {
                n = (start - end - step - 1) / (-step);
            }

            MenaiValue **rng_arr = n > 0
                ? (MenaiValue **)menai_alloc((size_t)n * sizeof(MenaiValue *)) : NULL;

            if (n > 0 && !rng_arr) {
                PyErr_NoMemory();
                goto error;
            }

            long val = start;
            for (ssize_t i = 0; i < n; i++) {
                MenaiValue *mi = make_integer_from_long(val);
                if (mi == NULL) {
                    for (ssize_t k = 0; k < i; k++) {
                        menai_release(rng_arr[k]);
                    }

                    menai_free(rng_arr, (size_t)n * sizeof(MenaiValue *));
                    goto error;
                }

                rng_arr[i] = mi;
                val += step;
            }

            MenaiValue *r = menai_list_from_array_steal(rng_arr, n);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_MAKE_STRUCT: {
            /*
             * MAKE_STRUCT src0, src1:
             * src0 = absolute slot of MenaiStructType descriptor in outgoing zone.
             * src1 = field count. Fields are in slots src0+1..src0+n_fields.
             */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *struct_type = regs[base + src0];
            if (!IS_MENAI_STRUCTTYPE(struct_type)) {
                menai_raise_eval_error("struct constructor: first argument must be a struct type");
                goto error;
            }

            int n_fields = src1;
            MenaiValue *instance = menai_struct_alloc(struct_type, &regs[base + src0 + 1], n_fields);
            if (instance == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, instance);
            break;
        }

        case OP_STRUCT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_STRUCT(regs[base + src0]));
            break;
        }

        case OP_STRUCT_TYPE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *stype = regs[base + src0];
            MenaiValue *val = regs[base + src1];
            if (!require_structtype(stype, "struct-type?")) {
                goto error;
            }

            if (!IS_MENAI_STRUCT(val)) {
                bool_store(regs, base + dest, 0);
                break;
            }

            int tag_a = ((MenaiStructType *)stype)->tag;
            int tag_b = ((MenaiStructType *)((MenaiStruct *)val)->struct_type)->tag;
            bool_store(regs, base + dest, tag_a == tag_b);
            break;
        }

        case OP_STRUCT_GET: {
            /* src1 holds a MenaiSymbol field name */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *val = regs[base + src0];
            MenaiValue *field_sym = regs[base + src1];
            if (!require_struct(val, "struct-get")) {
                goto error;
            }

            if (!require_symbol(field_sym, "struct-get")) {
                goto error;
            }

            MenaiValue *stype = ((MenaiStruct *)val)->struct_type;
            MenaiValue *field_name = menai_symbol_name(field_sym);
            int fi = menai_struct_field_index((MenaiStructType *)stype, field_name);
            if (fi < 0) {
                PyObject *stype_name = menai_string_to_pyunicode(((MenaiStructType *)stype)->name);
                PyObject *fname_py = menai_string_to_pyunicode(field_name);
                menai_raise_eval_errorf(
                    "'struct-get': struct '%s' has no field '%s'",
                    stype_name ? PyUnicode_AsUTF8(stype_name) : "?",
                    fname_py ? PyUnicode_AsUTF8(fname_py) : "?");
                Py_XDECREF(stype_name);
                Py_XDECREF(fname_py);
                goto error;
            }

            MenaiValue *fv = ((MenaiStruct *)val)->items[fi];
            menai_reg_set_borrow(regs, base + dest, fv);
            break;
        }

        case OP_STRUCT_GET_IMM: {
            /* src1 holds a MenaiInteger field index */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *val = regs[base + src0];
            MenaiValue *fidx = regs[base + src1];
            if (!require_struct(val, "struct-get-imm")) {
                goto error;
            }

            if (!require_integer(fidx, "struct-get-imm")) {
                goto error;
            }

            MenaiInteger *fi_io = (MenaiInteger *)fidx;
            long fi_l;
            if (!fi_io->is_big) {
                fi_l = fi_io->small;
            } else {
                if (menai_bigint_to_long(&fi_io->big, &fi_l) < 0) {
                    goto error;
                }
            }

            ssize_t fi = (ssize_t)fi_l;
            MenaiValue *fv = ((MenaiStruct *)val)->items[fi];
            menai_reg_set_borrow(regs, base + dest, fv);
            break;
        }

        case OP_STRUCT_SET: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *val = regs[base + src0];
            MenaiValue *field_sym = regs[base + src1];
            MenaiValue *new_val = regs[base + src2];
            if (!require_struct(val, "struct-set")) {
                goto error;
            }

            if (!require_symbol(field_sym, "struct-set")) {
                goto error;
            }

            MenaiValue *stype = ((MenaiStruct *)val)->struct_type;
            MenaiValue *field_name = menai_symbol_name(field_sym);
            int fi = menai_struct_field_index((MenaiStructType *)stype, field_name);
            if (fi < 0) {
                PyObject *stype_name = menai_string_to_pyunicode(((MenaiStructType *)stype)->name);
                PyObject *fname_py = menai_string_to_pyunicode(field_name);
                menai_raise_eval_errorf(
                    "'struct-set': struct '%s' has no field '%s'",
                    stype_name ? PyUnicode_AsUTF8(stype_name) : "?",
                    fname_py ? PyUnicode_AsUTF8(fname_py) : "?");
                Py_XDECREF(stype_name);
                Py_XDECREF(fname_py);
                goto error;
            }

            ssize_t nf = ((MenaiStruct *)val)->nfields;
            MenaiValue **tmp = (MenaiValue **)malloc(nf * sizeof(MenaiValue *));
            if (!tmp) {
                PyErr_NoMemory();
                goto error;
            }

            for (ssize_t i = 0; i < nf; i++) {
                tmp[i] = (i == fi) ? new_val : ((MenaiStruct *)val)->items[i];
            }

            MenaiValue *r = menai_struct_alloc(stype, tmp, nf);
            free(tmp);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRUCT_SET_IMM: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *val = regs[base + src0];
            MenaiValue *fidx = regs[base + src1];
            MenaiValue *new_val = regs[base + src2];
            if (!require_struct(val, "struct-set-imm")) {
                goto error;
            }

            if (!require_integer(fidx, "struct-set-imm")) {
                goto error;
            }

            MenaiInteger *fi_io = (MenaiInteger *)fidx;
            long fi_l;
            if (!fi_io->is_big) {
                fi_l = fi_io->small;
            } else {
                if (menai_bigint_to_long(&fi_io->big, &fi_l) < 0) {
                    goto error;
                }
            }

            ssize_t fi = (ssize_t)fi_l;
            MenaiValue *stype = ((MenaiStruct *)val)->struct_type;
            ssize_t nf = ((MenaiStruct *)val)->nfields;
            MenaiValue **tmp = (MenaiValue **)malloc(nf * sizeof(MenaiValue *));
            if (!tmp) {
                PyErr_NoMemory();
                goto error;
            }

            for (ssize_t i = 0; i < nf; i++) {
                tmp[i] = (i == fi) ? new_val : ((MenaiStruct *)val)->items[i];
            }

            MenaiValue *r = menai_struct_alloc(stype, tmp, nf);
            free(tmp);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_STRUCT_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_struct(a, "struct=?")) {
                goto error;
            }

            if (!require_struct(b, "struct=?")) {
                goto error;
            }

            MenaiStruct *sa = (MenaiStruct *)a;
            MenaiStruct *sb = (MenaiStruct *)b;
            int eq = (((MenaiStructType *)sa->struct_type)->tag ==
                      ((MenaiStructType *)sb->struct_type)->tag);
            ssize_t nf = sa->nfields;
            for (ssize_t i = 0; eq && i < nf; i++) {
                eq = menai_value_equal(sa->items[i], sb->items[i]);
            }

            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_STRUCT_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            MenaiValue *b = regs[base + src1];
            if (!require_struct(a, "struct!=?")) {
                goto error;
            }

            if (!require_struct(b, "struct!=?")) {
                goto error;
            }

            MenaiStruct *sa = (MenaiStruct *)a;
            MenaiStruct *sb = (MenaiStruct *)b;
            int neq = (((MenaiStructType *)sa->struct_type)->tag !=
                       ((MenaiStructType *)sb->struct_type)->tag);
            if (!neq) {
                ssize_t nf = sa->nfields;
                for (ssize_t i = 0; i < nf; i++) {
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
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *val = regs[base + src0];
            if (!require_struct(val, "struct-type")) {
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, ((MenaiStruct *)val)->struct_type);
            break;
        }

        case OP_STRUCT_TYPE_NAME: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *val = regs[base + src0];
            if (!require_structtype(val, "struct-type-name")) {
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest,
                ((MenaiStructType *)val)->name);
            break;
        }

        case OP_STRUCT_FIELDS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *val = regs[base + src0];
            if (!require_structtype(val, "struct-fields")) {
                goto error;
            }

            MenaiStructType *st = (MenaiStructType *)val;
            int n = st->nfields;
            MenaiValue **sf_arr = n > 0 ? (MenaiValue **)menai_alloc((size_t)n * sizeof(MenaiValue *)) : NULL;

            if (n > 0 && !sf_arr) {
                PyErr_NoMemory();
                goto error;
            }

            for (int i = 0; i < n; i++) {
                MenaiValue *sym = menai_symbol_alloc(st->fields[i].name);
                if (sym == NULL) {
                    for (int k = 0; k < i; k++) {
                        menai_release(sf_arr[k]);
                    }

                    menai_free(sf_arr, (size_t)n * sizeof(MenaiValue *));
                    goto error;
                }

                sf_arr[i] = sym;
            }

            MenaiValue *r = menai_list_from_array_steal(sf_arr, (ssize_t)n);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        default:
            menai_raise_eval_errorf("Unimplemented opcode: %d", opcode);
            goto error;
        }

        continue;

error:
        /* Release all live frames above the sentinel. */
        for (int d = frame_depth; d >= 1; d--) {
            if (frames[d].code_obj) {
                menai_code_object_release(frames[d].code_obj);
            }
        }

        return NULL;
    }
}

/*
 * menai_vm_c_execute — the Python-callable entry point
 */
static PyObject *
menai_vm_c_execute(PyObject *self, PyObject *args)
{
    PyObject *code;
    PyObject *globals_dict;

    if (!PyArg_ParseTuple(args, "OO", &code, &globals_dict)) {
        return NULL;
    }

    /* Convert the Python CodeObject tree to a native MenaiCodeObject tree.
     * All constants are converted to fast MenaiValue *s during this pass. */
    MenaiCodeObject *native_code = menai_code_object_from_python(code);
    if (!native_code) {
        return NULL;
    }

    /* Get (or build) the cached GlobalsTable. */
    const GlobalsTable *globals_gt = NULL;
    if (globals_dict && globals_dict != Py_None) {
        globals_gt = globals_get(globals_dict);
        if (!globals_gt) {
            menai_code_object_release(native_code);
            return NULL;
        }
    }

    /* Build the per-call globals table from the cached entries. */
    GlobalsTable globals;
    if (globals_build(&globals, globals_gt) < 0) {
        menai_code_object_release(native_code);
        return NULL;
    }

    /* Compute the register window size. */
    int max_locals = menai_code_object_max_locals(native_code);
    for (ssize_t i = 0; i < globals.count; i++) {
        MenaiValue *val = globals.entries[i].value;
        if (IS_MENAI_FUNCTION(val)) {
            int n = menai_code_object_max_locals(((MenaiFunction *)val)->bytecode);
            if (n > max_locals) {
                max_locals = n;
            }
        }
    }

    /* Allocate the register array. */
    MenaiValue **regs = menai_regs_alloc((size_t)(MAX_FRAME_DEPTH + 1) * max_locals, Menai_NONE);
    if (regs == NULL) {
        globals_free(&globals);
        menai_code_object_release(native_code);
        return NULL;
    }

    /* Run the VM. */
    MenaiValue *result = execute_loop(native_code, &globals, regs, max_locals);

    /* Clean up. */
    menai_regs_free(regs, (size_t)(MAX_FRAME_DEPTH + 1) * max_locals);
    globals_free(&globals);
    menai_code_object_release(native_code);

    if (result == NULL) {
        return NULL;
    }

    /* Convert to a slow Python MenaiValue before returning to Python callers. */
    PyObject *slow = menai_value_to_slow_value(result);
    menai_release((MenaiValue *)result);
    return slow;
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

static int
menai_vm_shim_init(void)
{
    if (!menai_vm_bridge_init()) {
        return -1;
    }

    PyObject *bytecode_mod = PyImport_ImportModule("menai.menai_bytecode");
    if (!bytecode_mod) {
        return -1;
    }

    PyObject *co_type = PyObject_GetAttrString(bytecode_mod, "CodeObject");
    Py_DECREF(bytecode_mod);
    if (!co_type) {
        return -1;
    }

    _py_code_object_type = (PyTypeObject *)co_type;

    Menai_NONE = menai_none_singleton();
    Menai_TRUE = menai_boolean_true();
    Menai_FALSE = menai_boolean_false();
    Menai_EMPTY_LIST = menai_list_new_empty();
    Menai_EMPTY_DICT = menai_dict_new_empty();
    Menai_EMPTY_SET = menai_set_new_empty();

    PyObject *err_mod = PyImport_ImportModule("menai.menai_error");
    if (err_mod == NULL) {
        return -1;
    }

    MenaiEvalError_type = PyObject_GetAttrString(err_mod, "MenaiEvalError");
    MenaiCancelledException_type = PyObject_GetAttrString(err_mod, "MenaiCancelledException");
    Py_DECREF(err_mod);
    if (MenaiEvalError_type == NULL || MenaiCancelledException_type == NULL) {
        Py_XDECREF(MenaiEvalError_type);
        Py_XDECREF(MenaiCancelledException_type);
        return -1;
    }

    return 0;
}

PyMODINIT_FUNC
PyInit_menai_vm_c(void)
{
    PyObject *module = PyModule_Create(&menai_vm_c_module);
    if (module == NULL) {
        return NULL;
    }

    if (menai_vm_shim_init() < 0) {
        Py_DECREF(module);
        return NULL;
    }

    return module;
}
