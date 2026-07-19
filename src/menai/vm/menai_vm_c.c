/*
 * menai_vm_c.c — C implementation of the Menai VM execute loop.
 *
 * Exposes:
 *   menai_vm_c.execute(code, globals_dict) -> MenaiValue *   (in menai_vm_bridge.c)
 *   menai_vm_c.cancel() -> None   (request cancellation of the running execute)
 *
 * The execute entry point and all Python-boundary logic live in
 * menai_vm_bridge.c.  This file contains the native execute loop,
 * globals table management, and the cancel method.
 */
#define _POSIX_C_SOURCE 200809L
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "menai_vm_c.h"
#include "menai_vm_atomic.h"

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
#define CANCEL_CHECK_INTERVAL (1 << 20)

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
#define OP_MAKE_DICT 280
#define OP_DICT_P 281
#define OP_DICT_EQ_P 282
#define OP_DICT_NEQ_P 283
#define OP_DICT_KEYS 284
#define OP_DICT_VALUES 285
#define OP_DICT_LENGTH 286
#define OP_DICT_HAS_P 287
#define OP_DICT_REMOVE 288
#define OP_DICT_MERGE 289
#define OP_DICT_SET 290
#define OP_DICT_GET 291
#define OP_MAKE_LIST 300
#define OP_LIST_P 301
#define OP_LIST_EQ_P 302
#define OP_LIST_NEQ_P 303
#define OP_LIST_PREPEND 304
#define OP_LIST_APPEND 305
#define OP_LIST_REVERSE 306
#define OP_LIST_FIRST 307
#define OP_LIST_REST 308
#define OP_LIST_LAST 309
#define OP_LIST_LENGTH 310
#define OP_LIST_REF 311
#define OP_LIST_NULL_P 312
#define OP_LIST_MEMBER_P 313
#define OP_LIST_INDEX 314
#define OP_LIST_SLICE 315
#define OP_LIST_REMOVE 316
#define OP_LIST_CONCAT 317
#define OP_LIST_TO_STRING 318
#define OP_LIST_TO_SET 319
#define OP_MAKE_SET 340
#define OP_SET_P 341
#define OP_SET_EQ_P 342
#define OP_SET_NEQ_P 343
#define OP_SET_MEMBER_P 344
#define OP_SET_ADD 345
#define OP_SET_REMOVE 346
#define OP_SET_LENGTH 347
#define OP_SET_UNION 348
#define OP_SET_INTERSECTION 349
#define OP_SET_DIFFERENCE 350
#define OP_SET_SUBSET_P 351
#define OP_SET_TO_LIST 352
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
#define OP_BYTES_P 400
#define OP_BYTES_EQ_P 401
#define OP_BYTES_NEQ_P 402
#define OP_BYTES_LENGTH 403
#define OP_BYTES_REF 410
#define OP_BYTES_APPEND_U8 411
#define OP_LIST_TO_BYTES 415
#define OP_BYTES_SLICE 420
#define OP_STRING_TO_BYTES 425
#define OP_BYTES_TO_STRING 426
#define OP_BYTES_TO_LIST 427
#define OP_BYTES_TO_STRING_HEX 428
#define OP_STRING_HEX_TO_BYTES 429
#define OP_BYTES_CONCAT 435
#define OP_BYTES_INDEX 440
#define OP_BYTES_INDEX_INT 441
#define OP_BYTES_LT_P 445
#define OP_BYTES_GT_P 446
#define OP_BYTES_LTE_P 447
#define OP_BYTES_GTE_P 448
#define OP_BYTES_READ_U8 460
#define OP_BYTES_READ_U16_LE 461
#define OP_BYTES_READ_U24_LE 462
#define OP_BYTES_READ_U32_LE 463
#define OP_BYTES_READ_U64_LE 464
#define OP_BYTES_READ_U16_BE 465
#define OP_BYTES_READ_U24_BE 466
#define OP_BYTES_READ_U32_BE 467
#define OP_BYTES_READ_U64_BE 468
#define OP_BYTES_READ_I8 469
#define OP_BYTES_READ_I16_LE 470
#define OP_BYTES_READ_I24_LE 471
#define OP_BYTES_READ_I32_LE 472
#define OP_BYTES_READ_I64_LE 473
#define OP_BYTES_READ_I16_BE 474
#define OP_BYTES_READ_I24_BE 475
#define OP_BYTES_READ_I32_BE 476
#define OP_BYTES_READ_I64_BE 477
#define OP_BYTES_APPEND_U16_LE 481
#define OP_BYTES_APPEND_U16_BE 482
#define OP_BYTES_APPEND_U24_LE 483
#define OP_BYTES_APPEND_U24_BE 484
#define OP_BYTES_APPEND_U32_LE 485
#define OP_BYTES_APPEND_U32_BE 486
#define OP_BYTES_APPEND_U64_LE 487
#define OP_BYTES_APPEND_U64_BE 488
#define OP_BYTES_APPEND_I8 489
#define OP_BYTES_APPEND_I16_LE 490
#define OP_BYTES_APPEND_I16_BE 491
#define OP_BYTES_APPEND_I24_LE 492
#define OP_BYTES_APPEND_I24_BE 493
#define OP_BYTES_APPEND_I32_LE 494
#define OP_BYTES_APPEND_I32_BE 495
#define OP_BYTES_APPEND_I64_LE 496
#define OP_BYTES_APPEND_I64_BE 497
#define OP_BYTES_WRITE_U8 500
#define OP_BYTES_WRITE_U16_LE 501
#define OP_BYTES_WRITE_U16_BE 502
#define OP_BYTES_WRITE_U24_LE 503
#define OP_BYTES_WRITE_U24_BE 504
#define OP_BYTES_WRITE_U32_LE 505
#define OP_BYTES_WRITE_U32_BE 506
#define OP_BYTES_WRITE_U64_LE 507
#define OP_BYTES_WRITE_U64_BE 508
#define OP_BYTES_WRITE_I8 509
#define OP_BYTES_WRITE_I16_LE 510
#define OP_BYTES_WRITE_I16_BE 511
#define OP_BYTES_WRITE_I24_LE 512
#define OP_BYTES_WRITE_I24_BE 513
#define OP_BYTES_WRITE_I32_LE 514
#define OP_BYTES_WRITE_I32_BE 515
#define OP_BYTES_WRITE_I64_LE 516
#define OP_BYTES_WRITE_I64_BE 517
#define OP_BYTES_READ_ULEB128 520
#define OP_BYTES_APPEND_ULEB128 521
#define OP_BYTES_READ_SLEB128 522
#define OP_BYTES_APPEND_SLEB128 523

/*
 * Singleton values fetched from menai_vm_bridge at init time.
 */
extern MenaiValue *Menai_NONE;
extern MenaiValue *Menai_TRUE;
extern MenaiValue *Menai_FALSE;
extern MenaiValue *Menai_EMPTY_LIST;
extern MenaiValue *Menai_EMPTY_DICT;
extern MenaiValue *Menai_EMPTY_SET;

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
 * menai_integer_to_menai_bigint — promote a MenaiInteger to an owned MenaiBigInt.
 * Caller must ensure val is a MenaiInteger and must free *out after use.
 * *out must be initialised (menai_bigint_init) before calling.
 * Returns 0 on success, negative MENAI_ERR_* on failure.
 */
static inline int
menai_integer_to_menai_bigint(MenaiInteger *val, MenaiBigInt *out)
{
    if (!val->is_big) {
        return menai_bigint_from_long(val->small, out);
    }

    return menai_bigint_copy(&val->big, out);
}

static inline MenaiValue *long_to_menai_integer(long n)
{
    return menai_integer_from_long(n);
}

/*
 * menai_integer_to_long — extract a C long from a MenaiInteger.
 * Returns 0 on success, -1 on error (value too large for a C long).
 * Caller must ensure val is a MenaiInteger.
 */
static inline int
menai_integer_to_long(MenaiValue *val, long *out)
{
    MenaiInteger *ib = (MenaiInteger *)val;
    if (!ib->is_big) {
        *out = ib->small;
        return 0;
    }

    if (menai_bigint_to_long(&ib->big, out) < 0) {
        return -1;
    }

    return 0;
}

/*
 * menai_integer_to_unsigned_long_long — extract an unsigned long long from a MenaiInteger.
 * Returns 0 on success, -1 on error (no exception set — caller handles).
 * Caller must ensure val is a MenaiInteger.
 */
static inline int
menai_integer_to_unsigned_long_long(MenaiValue *val, unsigned long long *out)
{
    MenaiInteger *ib = (MenaiInteger *)val;
    if (!ib->is_big) {
        if (ib->small < 0) {
            return -1;
        }
        *out = (unsigned long long)ib->small;
        return 0;
    }

    return menai_bigint_to_unsigned_long_long(&ib->big, out);
}

/*
 * ssize_t_to_menai_integer — create a MenaiInteger from a ssize_t.
 *
 * ssize_t fits in a long on all supported platforms, so this is a direct
 * delegation to menai_integer_from_long.
 */
static inline MenaiValue *
ssize_t_to_menai_integer(ssize_t n)
{
    return menai_integer_from_long((long)n);
}

/*
 * menai_integer_to_ssize_t — extract a ssize_t from a MenaiInteger.
 * Returns 0 on success, -1 on error (value too large for a ssize_t).
 * Caller must ensure val is a MenaiInteger.
 */
static inline int
menai_integer_to_ssize_t(MenaiValue *val, ssize_t *out)
{
    long tmp;
    if (menai_integer_to_long(val, &tmp) < 0) {
        return -1;
    }

    *out = (ssize_t)tmp;
    return 0;
}

static inline MenaiValue *double_to_menai_float(double v)
{
    return menai_float_alloc(v);
}

static inline MenaiValue *doubles_to_menai_complex(double real, double imag)
{
    return menai_complex_alloc(real, imag);
}

static inline void bool_store(MenaiValue **regs, int slot, int cond)
{
    menai_reg_set_borrow(regs, slot, cond ? Menai_TRUE : Menai_FALSE);
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

/*
 * Register array helpers
 *
 * The register array is a flat MenaiValue * array:
 *   regs[depth * max_locals + slot]
 * All slots are initialised to Menai_NONE (borrowed — the singleton is
 * kept alive by the module).  menai_reg_set_own/menai_reg_set_borrow manage reference counts correctly.
 */

/*
 * globals_free — free a GlobalsTable and all its owned resources.
 */
void
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
 * globals_slot_insert — insert a (name, hash, value) triple into the
 * GlobalsTable's open-addressing slot array.  The slot array must already
 * be allocated with slot_count > 0.  Does NOT touch the entries array —
 * the caller is responsible for that.
 */
static void
globals_slot_insert(GlobalsTable *gt, const char *name, hash_t h, MenaiValue *value)
{
    ssize_t mask = gt->slot_count - 1;
    uhash_t perturb = (uhash_t)h;
    ssize_t slot = (ssize_t)(perturb & (uhash_t)mask);
    for (;;) {
        if (gt->slots[slot].name == NULL) {
            gt->slots[slot].name = name;
            gt->slots[slot].hash = h;
            gt->slots[slot].value = value;
            break;
        }

        perturb >>= 5;
        slot = (ssize_t)((5 * (uhash_t)slot + 1 + perturb) & (uhash_t)mask);
    }
}

/*
 * globals_alloc_slots — allocate the entries and slots arrays for a
 * GlobalsTable that will hold n entries.  Returns 0 on success, -1 on
 * error (no Python exception set — caller handles).
 */
static int
globals_alloc_slots(GlobalsTable *gt, ssize_t n)
{
    gt->slots = NULL;
    gt->entries = NULL;
    gt->slot_count = 0;
    gt->count = 0;
    gt->owns_names = 0;

    if (n > 0) {
        gt->entries = (GlobalsEntry *)malloc(n * sizeof(GlobalsEntry));
        if (gt->entries == NULL) {
            return MENAI_ERR_NOMEM;
        }

        ssize_t min_slots = (n * 3 + 1) / 2;
        ssize_t sc = 4;
        while (sc < min_slots) {
            sc <<= 1;
        }

        gt->slots = (GlobalsSlot *)calloc(sc, sizeof(GlobalsSlot));
        if (gt->slots == NULL) {
            free(gt->entries);
            gt->entries = NULL;
            return MENAI_ERR_NOMEM;
        }

        gt->slot_count = sc;
    }

    return 0;
}

/*
 * globals_build — build a GlobalsTable from the cached globals GlobalsTable.
 *
 * All entries are already fast MenaiValue * objects retained and copied
 * directly from the cached table.  Returns 0 on success, MENAI_ERR_* on error.
 */
static int
globals_build(GlobalsTable *gt, const GlobalsTable *globals_gt)
{
    ssize_t total = globals_gt ? globals_gt->count : 0;

    int err = globals_alloc_slots(gt, total);
    if (err < 0) {
        return err;
    }

    for (ssize_t i = 0; i < total; i++) {
        menai_retain(globals_gt->entries[i].value);
        gt->entries[gt->count].name = globals_gt->entries[i].name;
        gt->entries[gt->count].value = globals_gt->entries[i].value;
        gt->count++;
    }

    for (ssize_t i = 0; i < gt->count; i++) {
        hash_t h = menai_name_str_hash(gt->entries[i].name);
        globals_slot_insert(gt, gt->entries[i].name, h, gt->entries[i].value);
    }

    return 0;
}

/*
 * globals_build_from_dict — build a GlobalsTable from a native MenaiDict.
 *
 * Fills the entries array only (no hash slots).  Names are strdup'd from
 * the MenaiString keys via menai_string_to_utf8.  Sets owns_names = 1.
 * Returns 0 on success, MENAI_ERR_* on error.
 */
int
globals_build_from_dict(GlobalsTable *gt, MenaiValue *dict_val)
{
    MenaiDict *d = (MenaiDict *)dict_val;
    ssize_t n = d->length;

    gt->slots = NULL;
    gt->entries = NULL;
    gt->slot_count = 0;
    gt->count = 0;
    gt->owns_names = 1;

    if (n > 0) {
        gt->entries = (GlobalsEntry *)malloc(n * sizeof(GlobalsEntry));
        if (gt->entries == NULL) {
            return MENAI_ERR_NOMEM;
        }

        for (ssize_t i = 0; i < n; i++) {
            MenaiValue *k = d->keys[i];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(k))) {
                globals_free(gt);
                return MENAI_ERR_TYPE;
            }

            char *name_copy = menai_string_to_utf8(k, NULL);
            if (name_copy == NULL) {
                globals_free(gt);
                return MENAI_ERR_NOMEM;
            }

            menai_retain(d->values[i]);
            gt->entries[gt->count].name = name_copy;
            gt->entries[gt->count].value = d->values[i];
            gt->count++;
        }
    }

    return 0;
}

/*
 * globals_build_from_arrays — build a GlobalsTable from arrays of names
 * and values.
 *
 * Fills the entries array only (no hash slots).  Names are strdup'd from
 * the input strings.  Values are retained.  Sets owns_names = 1.
 * Returns 0 on success, MENAI_ERR_* on error.
 */
int
globals_build_from_arrays(GlobalsTable *gt, const char **names, MenaiValue **values, ssize_t n)
{
    gt->slots = NULL;
    gt->entries = NULL;
    gt->slot_count = 0;
    gt->count = 0;
    gt->owns_names = 1;

    if (n > 0) {
        gt->entries = (GlobalsEntry *)malloc(n * sizeof(GlobalsEntry));
        if (gt->entries == NULL) {
            return MENAI_ERR_NOMEM;
        }

        for (ssize_t i = 0; i < n; i++) {
            char *name_copy = strdup(names[i]);
            if (name_copy == NULL) {
                globals_free(gt);
                return MENAI_ERR_NOMEM;
            }

            menai_retain(values[i]);
            gt->entries[gt->count].name = name_copy;
            gt->entries[gt->count].value = values[i];
            gt->count++;
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
 * globals_merge_extra_native — merge a native MenaiDict of extra bindings
 * into a per-call GlobalsTable.  Extra bindings shadow prelude entries
 * with the same name.
 *
 * If owns_names is 0, all existing borrowed names are strdup'd and
 * owns_names is set to 1 so that globals_free will free all names.
 *
 * Returns 0 on success, MENAI_ERR_* on error.
 */
static int
globals_merge_extra_native(GlobalsTable *gt, MenaiValue *extra_dict_val)
{
    MenaiDict *extra = (MenaiDict *)extra_dict_val;
    ssize_t nextra = extra->length;
    if (nextra == 0) {
        return 0;
    }

    /*
     * If names are currently borrowed (owns_names == 0), strdup them all
     * so that globals_free will correctly free every name including the
     * new ones we are about to add.
     */
    if (!gt->owns_names) {
        for (ssize_t i = 0; i < gt->count; i++) {
            char *name_copy = strdup(gt->entries[i].name);
            if (name_copy == NULL) {
                return MENAI_ERR_NOMEM;
            }

            gt->entries[i].name = name_copy;
        }

        gt->owns_names = 1;
    }

    ssize_t new_count = gt->count + nextra;
    GlobalsEntry *new_entries = (GlobalsEntry *)realloc(gt->entries, new_count * sizeof(GlobalsEntry));
    if (new_entries == NULL) {
        return MENAI_ERR_NOMEM;
    }
    gt->entries = new_entries;

    free(gt->slots);
    gt->slots = NULL;
    gt->slot_count = 0;

    /*
     * Rebuild the hash slots for the new total size.
     * We cannot use globals_alloc_slots here because it would zero
     * gt->count and overwrite gt->entries (which we just realloc'd).
     * Instead, allocate the slots array directly.
     */
    ssize_t min_slots = (new_count * 3 + 1) / 2;
    ssize_t sc = 4;
    while (sc < min_slots) {
        sc <<= 1;
    }

    gt->slots = (GlobalsSlot *)calloc(sc, sizeof(GlobalsSlot));
    if (gt->slots == NULL) {
        return MENAI_ERR_NOMEM;
    }

    gt->slot_count = sc;

    for (ssize_t i = 0; i < gt->count; i++) {
        hash_t h = menai_name_str_hash(gt->entries[i].name);
        globals_slot_insert(gt, gt->entries[i].name, h, gt->entries[i].value);
    }

    for (ssize_t i = 0; i < nextra; i++) {
        MenaiValue *k = extra->keys[i];
        if (MENAI_UNLIKELY(!IS_MENAI_STRING(k))) {
            return MENAI_ERR_TYPE;
        }

        char *name_copy = menai_string_to_utf8(k, NULL);
        if (name_copy == NULL) {
            return MENAI_ERR_NOMEM;
        }

        MenaiValue *fast_val = extra->values[i];
        menai_retain(fast_val);

        hash_t h = menai_name_str_hash(name_copy);
        MenaiValue *existing = globals_lookup_h(gt, name_copy, h);
        if (existing != NULL) {
            for (ssize_t j = 0; j < gt->count; j++) {
                if (strcmp(gt->entries[j].name, name_copy) == 0) {
                    menai_release(gt->entries[j].value);
                    gt->entries[j].value = fast_val;
                    break;
                }
            }

            ssize_t mask = gt->slot_count - 1;
            uhash_t perturb = (uhash_t)h;
            ssize_t slot = (ssize_t)(perturb & (uhash_t)mask);
            for (;;) {
                if (gt->slots[slot].name != NULL &&
                    gt->slots[slot].hash == h &&
                    strcmp(gt->slots[slot].name, name_copy) == 0) {
                    gt->slots[slot].value = fast_val;
                    break;
                }

                perturb >>= 5;
                slot = (ssize_t)((5 * (uhash_t)slot + 1 + perturb) & (uhash_t)mask);
            }

            free(name_copy);
            menai_release(fast_val);
        } else {
            gt->entries[gt->count].name = name_copy;
            gt->entries[gt->count].value = fast_val;
            gt->count++;
            globals_slot_insert(gt, name_copy, h, fast_val);
        }
    }

    return 0;
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
 * Returns MENAI_OK on success, or a MENAI_ERR_* code on error.
 */
static int
call_setup(Frame *new_frame, MenaiValue *func_obj, MenaiValue **regs, int callee_base, int arity, int return_dest)
{
    MenaiFunction *func = (MenaiFunction *)func_obj;
    MenaiCodeObject *co = func->bytecode;
    int param_count = co->param_count;
    int is_variadic = co->is_variadic;

    if (MENAI_UNLIKELY(is_variadic)) {
        int min_arity = param_count - 1;
        if (arity < min_arity) {
            return MENAI_ERR_ARITY_MISMATCH;
        }

        /* Pack excess args into a MenaiList for the rest parameter. */
        int rest_count = arity - min_arity;
        MenaiValue *rest_list = menai_list_alloc(rest_count);
        if (!rest_list) {
            return MENAI_ERR_NOMEM;
        }

        for (int k = 0; k < rest_count; k++) {
            menai_list_elements(rest_list)[k] = regs[callee_base + min_arity + k];
            menai_retain(menai_list_elements(rest_list)[k]);
        }

        menai_reg_set_own(regs, callee_base + min_arity, rest_list);
    } else if (MENAI_UNLIKELY(arity != param_count)) {
        return MENAI_ERR_ARITY_MISMATCH;
    }

    /* Populate capture slots: regs[callee_base + param_count + i] */
    ssize_t ncap = func->ncap;
    MenaiValue **captures = func->captures;
    for (ssize_t i = 0; i < ncap; i++) {
        MenaiValue *cv = *captures++;
        menai_reg_set_borrow(regs, callee_base + param_count + (int)i, cv);
    }

    frame_setup(new_frame, co, callee_base, return_dest);
    return MENAI_OK;
}

/*
 * Internal execute — called by menai_vm_c_execute after setup.
 * Returns the result value (new reference) or NULL on error.
 */
static MenaiValue *
execute_loop(MenaiCodeObject *code, const GlobalsTable *globals,
             MenaiValue **regs, int max_locals, MenaiVMError *out_error, int *cancel_flag)
{
    int vm_err = MENAI_OK;
    const char *vm_user_message = NULL;

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
    int cur_opcode = 0;
    int cur_ip = 0;

    while (1) {
        /* Cancellation check */
        if ((++instr_count & (CANCEL_CHECK_INTERVAL - 1)) == 0) {
            instr_count = 0;

            if (cancel_flag && _menai_atomic_load((_menai_atomic_int *)cancel_flag)) {
                vm_err = MENAI_ERR_CANCELLED;
                goto error;
            }
        }

        if (frame->ip >= frame->code_len) {
            vm_err = MENAI_ERR_MISSING_RETURN;
            goto error;
        }

        /* Fetch and decode instruction */
        uint64_t word = frame->instrs[frame->ip++];
        int opcode = (int)((word >> OPCODE_SHIFT) & OPCODE_MASK);
        int dest = (int)((word >> DEST_SHIFT) & FIELD_MASK);
        int base = frame->base;

        cur_opcode = opcode;
        cur_ip = frame->ip - 1;

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
                vm_err = MENAI_ERR_UNDEFINED_VARIABLE;
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, val);
            break;
        }

        case OP_MOVE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            menai_reg_set_borrow(regs, base + dest, regs[base + src0]);
            break;
        }

        case OP_JUMP: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            frame->ip = src0;
            break;
        }

        case OP_JUMP_IF_FALSE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *cond = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BOOLEAN(cond))) {
                vm_err = MENAI_ERR_IF_NOT_BOOLEAN;
                goto error;
            }

            if (!menai_boolean_value(cond)) {
                int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
                frame->ip = src1;
            }

            break;
        }

        case OP_JUMP_IF_TRUE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *cond = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BOOLEAN(cond))) {
                vm_err = MENAI_ERR_IF_NOT_BOOLEAN;
                goto error;
            }

            if (menai_boolean_value(cond)) {
                int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
                frame->ip = src1;
            }

            break;
        }

        case OP_RAISE_ERROR: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *msg = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(msg))) {
                vm_err = MENAI_ERR_ERROR_MSG_NOT_STRING;
                goto error;
            }

            char *cstr = menai_string_to_utf8(msg, NULL);
            if (cstr == NULL) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            vm_err = MENAI_ERR_USER_ERROR;
            vm_user_message = cstr;
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
            MenaiValue *raw = regs[base + src0];
            int arity = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int callee_base = base + frame->local_count;

            if (IS_MENAI_FUNCTION(raw)) {
                if (frame_depth >= MAX_FRAME_DEPTH) {
                    vm_err = MENAI_ERR_CALL_DEPTH_EXCEEDED;
                    goto error;
                }

                frame_depth++;
                Frame *new_frame = &frames[frame_depth];
                new_frame->code_obj = NULL;
                new_frame->constants_items = NULL;
                new_frame->instrs = NULL;

                vm_err = call_setup(new_frame, raw, regs, callee_base, arity, dest);
                if (MENAI_UNLIKELY(vm_err < 0)) {
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
                    vm_err = MENAI_ERR_STRUCT_ARITY_MISMATCH;
                    goto error;
                }

                MenaiValue *instance = menai_struct_alloc(raw, &regs[callee_base], n_fields);
                if (instance == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, instance);
                break;
            }

            vm_err = MENAI_ERR_NOT_CALLABLE;
            goto error;
        }

        case OP_TAIL_CALL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *raw = regs[base + src0];
            int n_args = (int)((word >> SRC1_SHIFT) & FIELD_MASK);

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
                vm_err = call_setup(frame, raw, regs, base, n_args, saved_return_dest);
                if (MENAI_UNLIKELY(vm_err < 0)) {
                    menai_release(raw);
                    goto error;
                }

                menai_release(raw);
                break;
            }

            if (IS_MENAI_STRUCTTYPE(raw)) {
                int n_fields = ((MenaiStructType *)raw)->nfields;
                if (n_args != (int)n_fields) {
                    vm_err = MENAI_ERR_STRUCT_ARITY_MISMATCH;
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
            vm_err = MENAI_ERR_NOT_CALLABLE;
            goto error;
        }

        case OP_APPLY: {
            /*
             * APPLY dest, src0, src1:
             * src0 = function register, src1 = arg_list register.
             * Scatters the list into the callee's register window and pushes a frame.
             */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *raw_func = regs[base + src0];
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *raw_args = regs[base + src1];

            if (MENAI_UNLIKELY(!IS_MENAI_LIST(raw_args))) {
                vm_err = MENAI_ERR_APPLY_SECOND_NOT_LIST;
                goto error;
            }

            MenaiList *list = (MenaiList *)raw_args;
            MenaiValue **elements = list->elements;
            int arity = (int)list->length;

            if (IS_MENAI_FUNCTION(raw_func)) {
                if (frame_depth >= MAX_FRAME_DEPTH) {
                    vm_err = MENAI_ERR_CALL_DEPTH_EXCEEDED;
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

                vm_err = call_setup(new_frame, raw_func, regs, callee_base, arity, dest);
                if (MENAI_UNLIKELY(vm_err < 0)) {
                    frame_depth--;
                    goto error;
                }

                frame = new_frame;
                break;
            }

            if (IS_MENAI_STRUCTTYPE(raw_func)) {
                int n_fields = ((MenaiStructType *)raw_func)->nfields;
                if (arity != (int)n_fields) {
                    vm_err = MENAI_ERR_STRUCT_ARITY_MISMATCH;
                    goto error;
                }

                MenaiValue *instance = menai_struct_alloc(raw_func, elements, n_fields);
                if (instance == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, instance);
                break;
            }

            vm_err = MENAI_ERR_APPLY_FIRST_NOT_FUNCTION;
            goto error;
        }

        case OP_TAIL_APPLY: {
            /*
             * TAIL_APPLY src0, src1:
             * src0 = function register, src1 = arg_list register.
             * Reuses current frame (tail position).
             */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *raw_func = regs[base + src0];
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *raw_args = regs[base + src1];

            /* Own raw_func before the scatter loop which may overwrite its slot. */
            /* Own raw_args for the same reason — src1 may be < arity. */
            menai_retain(raw_func);
            menai_retain(raw_args);

            if (MENAI_UNLIKELY(!IS_MENAI_LIST(raw_args))) {
                menai_release(raw_func);
                menai_release(raw_args);
                vm_err = MENAI_ERR_APPLY_SECOND_NOT_LIST;
                goto error;
            }

            MenaiList *list = (MenaiList *)raw_args;
            MenaiValue **elements = list->elements;
            int arity = (int)list->length;

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
                vm_err = call_setup(frame, raw_func, regs, base, arity, saved_return_dest);
                if (MENAI_UNLIKELY(vm_err < 0)) {
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
                    vm_err = MENAI_ERR_STRUCT_ARITY_MISMATCH;
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
            vm_err = MENAI_ERR_APPLY_FIRST_NOT_FUNCTION;
            goto error;
        }

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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BOOLEAN(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_BOOLEAN(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_boolean_value(a) == menai_boolean_value(b));
            break;
        }

        case OP_BOOLEAN_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BOOLEAN(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_BOOLEAN(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_boolean_value(a) != menai_boolean_value(b));
            break;
        }

        case OP_BOOLEAN_NOT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BOOLEAN(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SYMBOL(a))) {
                vm_err = MENAI_ERR_NOT_SYMBOL_PAIR;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_SYMBOL(b))) {
                vm_err = MENAI_ERR_NOT_SYMBOL_PAIR;
                goto error;
            }

            bool_store(regs, base + dest, menai_string_equal(menai_symbol_name(a), menai_symbol_name(b)));
            break;
        }

        case OP_SYMBOL_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SYMBOL(a))) {
                vm_err = MENAI_ERR_NOT_SYMBOL_PAIR;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_SYMBOL(b))) {
                vm_err = MENAI_ERR_NOT_SYMBOL_PAIR;
                goto error;
            }

            bool_store(regs, base + dest, !menai_string_equal(menai_symbol_name(a), menai_symbol_name(b)));
            break;
        }

        case OP_SYMBOL_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SYMBOL(a))) {
                vm_err = MENAI_ERR_NOT_SYMBOL;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FUNCTION(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FUNCTION(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, a == b);
            break;
        }

        case OP_FUNCTION_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FUNCTION(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FUNCTION(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, a != b);
            break;
        }

        case OP_FUNCTION_MIN_ARITY: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *f = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FUNCTION(f))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiFunction *fn = (MenaiFunction *)f;
            int min_a = fn->bytecode->is_variadic ? fn->bytecode->param_count - 1 : fn->bytecode->param_count;
            MenaiValue *_r = long_to_menai_integer(min_a);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FUNCTION_VARIADIC_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *f = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FUNCTION(f))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, ((MenaiFunction *)f)->bytecode->is_variadic);
            break;
        }

        case OP_FUNCTION_ACCEPTS_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *f = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FUNCTION(f))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *n_obj = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(n_obj))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
                vm_err = menai_bigint_to_long(&n_io->big, &n);
                if (vm_err < 0) {
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            if (MENAI_LIKELY(!ia->is_big && !ib->is_big)) {
                bool_store(regs, base + dest, ia->small == ib->small);
                break;
            }

            const MenaiBigInt *ma = ia->is_big ? &ia->big : NULL;
            const MenaiBigInt *mb = ib->is_big ? &ib->big : NULL;
            MenaiBigInt tmp_a, tmp_b;
            menai_bigint_init(&tmp_a);
            menai_bigint_init(&tmp_b);

            vm_err = menai_integer_to_menai_bigint(ia, &tmp_a);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &tmp_b);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                menai_bigint_free(&tmp_a);
                goto error;
            }

            const MenaiBigInt *pa = ia->is_big ? ma : &tmp_a;
            const MenaiBigInt *pb = ib->is_big ? mb : &tmp_b;
            bool_store(regs, base + dest, menai_bigint_eq(pa, pb));

            menai_bigint_free(&tmp_a);
            menai_bigint_free(&tmp_b);
            break;
        }

        case OP_INTEGER_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            if (MENAI_LIKELY(!ia->is_big && !ib->is_big)) {
                bool_store(regs, base + dest, ia->small != ib->small);
                break;
            }

            const MenaiBigInt *ma = ia->is_big ? &ia->big : NULL;
            const MenaiBigInt *mb = ib->is_big ? &ib->big : NULL;
            MenaiBigInt tmp_a, tmp_b;
            menai_bigint_init(&tmp_a);
            menai_bigint_init(&tmp_b);

            vm_err = menai_integer_to_menai_bigint(ia, &tmp_a);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &tmp_b);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                menai_bigint_free(&tmp_a);
                goto error;
            }

            const MenaiBigInt *pa = ia->is_big ? ma : &tmp_a;
            const MenaiBigInt *pb = ib->is_big ? mb : &tmp_b;
            bool_store(regs, base + dest, menai_bigint_ne(pa, pb));

            menai_bigint_free(&tmp_a);
            menai_bigint_free(&tmp_b);
            break;
        }

        case OP_INTEGER_LT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            if (MENAI_LIKELY(!ia->is_big && !ib->is_big)) {
                bool_store(regs, base + dest, ia->small < ib->small);
                break;
            }

            const MenaiBigInt *ma = ia->is_big ? &ia->big : NULL;
            const MenaiBigInt *mb = ib->is_big ? &ib->big : NULL;
            MenaiBigInt tmp_a, tmp_b;
            menai_bigint_init(&tmp_a);
            menai_bigint_init(&tmp_b);

            vm_err = menai_integer_to_menai_bigint(ia, &tmp_a);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &tmp_b);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                menai_bigint_free(&tmp_a);
                goto error;
            }

            const MenaiBigInt *pa = ia->is_big ? ma : &tmp_a;
            const MenaiBigInt *pb = ib->is_big ? mb : &tmp_b;
            bool_store(regs, base + dest, menai_bigint_lt(pa, pb));

            menai_bigint_free(&tmp_a);
            menai_bigint_free(&tmp_b);
            break;
        }

        case OP_INTEGER_GT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            if (MENAI_LIKELY(!ia->is_big && !ib->is_big)) {
                bool_store(regs, base + dest, ia->small > ib->small);
                break;
            }

            const MenaiBigInt *ma = ia->is_big ? &ia->big : NULL;
            const MenaiBigInt *mb = ib->is_big ? &ib->big : NULL;
            MenaiBigInt tmp_a, tmp_b;
            menai_bigint_init(&tmp_a);
            menai_bigint_init(&tmp_b);

            vm_err = menai_integer_to_menai_bigint(ia, &tmp_a);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &tmp_b);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                menai_bigint_free(&tmp_a);
                goto error;
            }

            const MenaiBigInt *pa = ia->is_big ? ma : &tmp_a;
            const MenaiBigInt *pb = ib->is_big ? mb : &tmp_b;
            bool_store(regs, base + dest, menai_bigint_gt(pa, pb));

            menai_bigint_free(&tmp_a);
            menai_bigint_free(&tmp_b);
            break;
        }

        case OP_INTEGER_LTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            if (MENAI_LIKELY(!ia->is_big && !ib->is_big)) {
                bool_store(regs, base + dest, ia->small <= ib->small);
                break;
            }

            const MenaiBigInt *ma = ia->is_big ? &ia->big : NULL;
            const MenaiBigInt *mb = ib->is_big ? &ib->big : NULL;
            MenaiBigInt tmp_a, tmp_b;
            menai_bigint_init(&tmp_a);
            menai_bigint_init(&tmp_b);

            vm_err = menai_integer_to_menai_bigint(ia, &tmp_a);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &tmp_b);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                menai_bigint_free(&tmp_a);
                goto error;
            }

            const MenaiBigInt *pa = ia->is_big ? ma : &tmp_a;
            const MenaiBigInt *pb = ib->is_big ? mb : &tmp_b;
            bool_store(regs, base + dest, menai_bigint_le(pa, pb));

            menai_bigint_free(&tmp_a);
            menai_bigint_free(&tmp_b);
            break;
        }

        case OP_INTEGER_GTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            if (MENAI_LIKELY(!ia->is_big && !ib->is_big)) {
                bool_store(regs, base + dest, ia->small >= ib->small);
                break;
            }

            const MenaiBigInt *ma = ia->is_big ? &ia->big : NULL;
            const MenaiBigInt *mb = ib->is_big ? &ib->big : NULL;
            MenaiBigInt tmp_a, tmp_b;
            menai_bigint_init(&tmp_a);
            menai_bigint_init(&tmp_b);

            vm_err = menai_integer_to_menai_bigint(ia, &tmp_a);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &tmp_b);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                menai_bigint_free(&tmp_a);
                goto error;
            }

            const MenaiBigInt *pa = ia->is_big ? ma : &tmp_a;
            const MenaiBigInt *pb = ib->is_big ? mb : &tmp_b;
            bool_store(regs, base + dest, menai_bigint_ge(pa, pb));

            menai_bigint_free(&tmp_a);
            menai_bigint_free(&tmp_b);
            break;
        }

        case OP_INTEGER_ABS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
                    vm_err = menai_bigint_from_long(sv, &tmp);
                    if (vm_err < 0) {
                        goto error;
                    }

                    vm_err = menai_bigint_abs(&tmp, &res);
                    if (vm_err < 0) {
                        menai_bigint_free(&tmp);
                        goto error;
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
            vm_err = menai_bigint_abs(&ia->big, &res);
            if (vm_err < 0) {
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
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
                    vm_err = menai_bigint_from_long(sv, &tmp);
                    if (vm_err < 0) {
                        goto error;
                    }

                    vm_err = menai_bigint_neg(&tmp, &res);
                    if (vm_err < 0) {
                        menai_bigint_free(&tmp);
                        goto error;
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
            vm_err = menai_bigint_neg(&ia->big, &res);
            if (vm_err < 0) {
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
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiBigInt tmp, res;
            menai_bigint_init(&tmp);
            menai_bigint_init(&res);

            MenaiInteger *ia = (MenaiInteger *)a;
            vm_err = menai_integer_to_menai_bigint(ia, &tmp);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_bigint_not(&tmp, &res);
            if (vm_err < 0) {
                menai_bigint_free(&tmp);
                goto error;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            if (!ia->is_big && !ib->is_big) {
                long la = ia->small;
                long lb = ib->small;
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
            vm_err = menai_integer_to_menai_bigint(ia, &av);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &bv);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                goto error;
            }

            vm_err = menai_bigint_add(&av, &bv, &res);
            if (vm_err < 0) {
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            if (!ia->is_big && !ib->is_big) {
                long la = ia->small;
                long lb = ib->small;
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
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            vm_err = menai_integer_to_menai_bigint(ia, &av);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &bv);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                goto error;
            }

            vm_err = menai_bigint_sub(&av, &bv, &res);
            if (vm_err < 0) {
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

        case OP_INTEGER_MUL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            if (!ia->is_big && !ib->is_big) {
                long la = ia->small;
                long lb = ib->small;
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
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            vm_err = menai_integer_to_menai_bigint(ia, &av);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &bv);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                goto error;
            }

            vm_err = menai_bigint_mul(&av, &bv, &res);
            if (vm_err < 0) {
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

        case OP_INTEGER_DIV: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            int b_is_zero = (!ib->is_big && ib->small == 0) || (ib->is_big && ib->big.sign == 0);
            if (b_is_zero) {
                vm_err = MENAI_ERR_DIVISION_BY_ZERO;
                goto error;
            }

            if (!ia->is_big && !ib->is_big) {
                long la = ia->small;
                long lb = ib->small;

                /* Floor division: round toward negative infinity. */
                long lq = la / lb;
                long lr = la % lb;
                if (lr != 0 && ((lr < 0) != (lb < 0))) {
                    lq--;
                }

                MenaiValue *_r = menai_integer_from_long(lq);
                if (!_r) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
                break;
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            vm_err = menai_integer_to_menai_bigint(ia, &av);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &bv);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                goto error;
            }

            vm_err = menai_bigint_floordiv(&av, &bv, &res);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                menai_bigint_free(&bv);
                goto error;
            }

            menai_bigint_free(&av);
            menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_MOD: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            int b_is_zero = (!ib->is_big && ib->small == 0) || (ib->is_big && ib->big.sign == 0);
            if (b_is_zero) {
                vm_err = MENAI_ERR_MODULO_BY_ZERO;
                goto error;
            }

            if (!ia->is_big && !ib->is_big) {
                long la = ia->small;
                long lb = ib->small;

                /* Floor modulo: result takes sign of divisor. */
                long lr = la % lb;
                if (lr != 0 && ((lr < 0) != (lb < 0))) {
                    lr += lb;
                }

                MenaiValue *_r = menai_integer_from_long(lr);
                if (!_r) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
                break;
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            vm_err = menai_integer_to_menai_bigint(ia, &av);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &bv);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                goto error;
            }

            vm_err = menai_bigint_mod(&av, &bv, &res);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                menai_bigint_free(&bv);
                goto error;
            }

            menai_bigint_free(&av);
            menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_EXPN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            int b_is_neg = (!ib->is_big && ib->small < 0) || (ib->is_big && ib->big.sign == -1);
            if (b_is_neg) {
                vm_err = MENAI_ERR_NEGATIVE_EXPONENT;
                goto error;
            }

            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            vm_err = menai_integer_to_menai_bigint(ia, &av);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &bv);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                goto error;
            }

            vm_err = menai_bigint_pow(&av, &bv, &res);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                menai_bigint_free(&bv);
                goto error;
            }

            menai_bigint_free(&av);
            menai_bigint_free(&bv);
            MenaiValue *_r = menai_integer_from_bigint(res);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_BIT_OR: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            vm_err = menai_integer_to_menai_bigint(ia, &av);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &bv);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                goto error;
            }

            vm_err = menai_bigint_or(&av, &bv, &res);
            if (vm_err < 0) {
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

        case OP_INTEGER_BIT_AND: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            vm_err = menai_integer_to_menai_bigint(ia, &av);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &bv);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                goto error;
            }

            vm_err = menai_bigint_and(&av, &bv, &res);
            if (vm_err < 0) {
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

        case OP_INTEGER_BIT_XOR: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            MenaiBigInt av, bv, res;
            menai_bigint_init(&av);
            menai_bigint_init(&bv);
            menai_bigint_init(&res);
            vm_err = menai_integer_to_menai_bigint(ia, &av);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &bv);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                goto error;
            }

            vm_err = menai_bigint_xor(&av, &bv, &res);
            if (vm_err < 0) {
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

        case OP_INTEGER_BIT_SHIFT_LEFT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            long shift;
            if (!ib->is_big) {
                shift = ib->small;
            } else {
                if (!menai_bigint_fits_long(&ib->big)) {
                    vm_err = MENAI_ERR_SHIFT_TOO_LARGE;
                    goto error;
                }

                vm_err = menai_bigint_to_long(&ib->big, &shift);
                if (vm_err < 0) {
                    goto error;
                }
            }

            if (shift < 0) {
                vm_err = MENAI_ERR_NEGATIVE_SHIFT;
                goto error;
            }

            MenaiBigInt av, res;
            menai_bigint_init(&av);
            menai_bigint_init(&res);
            vm_err = menai_integer_to_menai_bigint(ia, &av);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_bigint_shift_left(&av, (ssize_t)shift, &res);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                goto error;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            long shift;
            if (!ib->is_big) {
                shift = ib->small;
            } else {
                if (!menai_bigint_fits_long(&ib->big)) {
                    vm_err = MENAI_ERR_SHIFT_TOO_LARGE;
                    goto error;
                }

                vm_err = menai_bigint_to_long(&ib->big, &shift);
                if (vm_err < 0) {
                    goto error;
                }
            }

            if (shift < 0) {
                vm_err = MENAI_ERR_NEGATIVE_SHIFT;
                goto error;
            }

            MenaiBigInt av, res;
            menai_bigint_init(&av);
            menai_bigint_init(&res);
            vm_err = menai_integer_to_menai_bigint(ia, &av);
            if (vm_err < 0) {
                goto error;
            }

            vm_err = menai_bigint_shift_right(&av, (ssize_t)shift, &res);
            if (vm_err < 0) {
                menai_bigint_free(&av);
                goto error;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            if (MENAI_LIKELY(!ia->is_big && !ib->is_big)) {
                menai_reg_set_borrow(regs, base + dest, ia->small <= ib->small ? a : b);
                break;
            }

            const MenaiBigInt *ma = ia->is_big ? &ia->big : NULL;
            const MenaiBigInt *mb = ib->is_big ? &ib->big : NULL;
            MenaiBigInt tmp_a, tmp_b;
            menai_bigint_init(&tmp_a);
            menai_bigint_init(&tmp_b);

            vm_err = menai_integer_to_menai_bigint(ia, &tmp_a);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &tmp_b);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                menai_bigint_free(&tmp_a);
                goto error;
            }

            const MenaiBigInt *pa = ia->is_big ? ma : &tmp_a;
            const MenaiBigInt *pb = ib->is_big ? mb : &tmp_b;
            bool_store(regs, base + dest, menai_bigint_ge(pa, pb));
            menai_reg_set_borrow(regs, base + dest, menai_bigint_le(pa, pb) ? a : b);

            menai_bigint_free(&tmp_a);
            menai_bigint_free(&tmp_b);
            break;
        }

        case OP_INTEGER_MAX: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            if (MENAI_LIKELY(!ia->is_big && !ib->is_big)) {
                menai_reg_set_borrow(regs, base + dest, ia->small >= ib->small ? a : b);
                break;
            }

            const MenaiBigInt *ma = ia->is_big ? &ia->big : NULL;
            const MenaiBigInt *mb = ib->is_big ? &ib->big : NULL;
            MenaiBigInt tmp_a, tmp_b;
            menai_bigint_init(&tmp_a);
            menai_bigint_init(&tmp_b);

            vm_err = menai_integer_to_menai_bigint(ia, &tmp_a);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                goto error;
            }

            vm_err = menai_integer_to_menai_bigint(ib, &tmp_b);
            if (MENAI_UNLIKELY(vm_err < 0)) {
                menai_bigint_free(&tmp_a);
                goto error;
            }

            const MenaiBigInt *pa = ia->is_big ? ma : &tmp_a;
            const MenaiBigInt *pb = ib->is_big ? mb : &tmp_b;
            bool_store(regs, base + dest, menai_bigint_ge(pa, pb));
            menai_reg_set_borrow(regs, base + dest, menai_bigint_ge(pa, pb) ? a : b);

            menai_bigint_free(&tmp_a);
            menai_bigint_free(&tmp_b);
            break;
        }

        case OP_INTEGER_TO_FLOAT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            double d;
            if (!ia->is_big) {
                d = (double)ia->small;
            } else {
                vm_err = menai_bigint_to_double(&ia->big, &d);
                if (vm_err < 0) {
                    goto error;
                }
            }

            MenaiValue *_r = double_to_menai_float(d);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_INTEGER_TO_COMPLEX: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            double re, im;
            if (!ia->is_big) {
                re = (double)ia->small;
            } else {
                vm_err = menai_bigint_to_double(&ia->big, &re);
                if (vm_err < 0) {
                    goto error;
                }
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            if (!ib->is_big) {
                im = (double)ib->small;
            } else {
                vm_err = menai_bigint_to_double(&ib->big, &im);
                if (vm_err < 0) {
                    goto error;
                }
            }

            MenaiValue *r = doubles_to_menai_complex(re, im);
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_INTEGER_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            long radix;
            if (!ib->is_big) {
                radix = ib->small;
            } else {
                vm_err = menai_bigint_to_long(&ib->big, &radix);
                if (vm_err < 0) {
                    goto error;
                }
            }

            if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
                vm_err = MENAI_ERR_INVALID_RADIX;
                goto error;
            }

            MenaiBigInt tmp;
            menai_bigint_init(&tmp);
            vm_err = menai_integer_to_menai_bigint(ia, &tmp);
            if (vm_err < 0) {
                goto error;
            }

            MenaiValue *r = menai_bigint_to_menai_string(&tmp, (int)radix);
            menai_bigint_free(&tmp);
            if (r == NULL) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_INTEGER_CODEPOINT_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)a;
            long cp;
            if (!ia->is_big) {
                cp = ia->small;
            } else {
                vm_err = menai_bigint_to_long(&ia->big, &cp);
                if (vm_err < 0) {
                    goto error;
                }
            }

            if (cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF)) {
                vm_err = MENAI_ERR_INVALID_CODEPOINT;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) == menai_float_value(b));
            break;
        }

        case OP_FLOAT_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) != menai_float_value(b));
            break;
        }

        case OP_FLOAT_LT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) < menai_float_value(b));
            break;
        }

        case OP_FLOAT_GT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) > menai_float_value(b));
            break;
        }

        case OP_FLOAT_LTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) <= menai_float_value(b));
            break;
        }

        case OP_FLOAT_GTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_float_value(a) >= menai_float_value(b));
            break;
        }

        case OP_FLOAT_NEG: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(-menai_float_value(a));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_ABS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double v = menai_float_value(a);
            {
                MenaiValue *_r = double_to_menai_float(fabs(v));
                if (_r == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
            }

            break;
        }

        case OP_FLOAT_ADD: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(menai_float_value(a) + menai_float_value(b));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_SUB: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(menai_float_value(a) - menai_float_value(b));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_MUL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(menai_float_value(a) * menai_float_value(b));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_DIV: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double bv = menai_float_value(b);
            if (bv == 0.0) {
                vm_err = MENAI_ERR_DIVISION_BY_ZERO;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(menai_float_value(a) / bv);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_FLOOR_DIV: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double bv = menai_float_value(b);
            if (bv == 0.0) {
                vm_err = MENAI_ERR_DIVISION_BY_ZERO;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(floor(menai_float_value(a) / bv));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_MOD: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double bv = menai_float_value(b);
            if (bv == 0.0) {
                vm_err = MENAI_ERR_MODULO_BY_ZERO;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(fmod(menai_float_value(a), bv));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_EXP: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(exp(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_EXPN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(pow(menai_float_value(a), menai_float_value(b)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_LOG: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double v = menai_float_value(a);
            if (v < 0.0) {
                vm_err = MENAI_ERR_NEGATIVE_ARGUMENT;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(v == 0.0 ? -INFINITY : log(v));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_LOG10: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double v = menai_float_value(a);
            if (v < 0.0) {
                vm_err = MENAI_ERR_NEGATIVE_ARGUMENT;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(v == 0.0 ? -INFINITY : log10(v));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_LOG2: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double v = menai_float_value(a);
            if (v < 0.0) {
                vm_err = MENAI_ERR_NEGATIVE_ARGUMENT;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(v == 0.0 ? -INFINITY : log2(v));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_LOGN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double av = menai_float_value(a), bv = menai_float_value(b);
            if (bv <= 0.0 || bv == 1.0) {
                vm_err = MENAI_ERR_INVALID_LOG_BASE;
                goto error;
            }

            if (av < 0.0) {
                vm_err = MENAI_ERR_NEGATIVE_ARGUMENT;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(av == 0.0 ? -INFINITY : log(av) / log(bv));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_SIN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(sin(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_COS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(cos(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_TAN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(tan(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_SQRT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double v = menai_float_value(a);
            if (v < 0.0) {
                vm_err = MENAI_ERR_NEGATIVE_ARGUMENT;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(sqrt(v));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_FLOOR: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(floor(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_CEIL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(ceil(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_ROUND: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = double_to_menai_float(round(menai_float_value(a)));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_MIN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double av = menai_float_value(a), bv = menai_float_value(b);
            MenaiValue *_r = double_to_menai_float(av <= bv ? av : bv);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_MAX: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double av = menai_float_value(a), bv = menai_float_value(b);
            MenaiValue *_r = double_to_menai_float(av >= bv ? av : bv);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_FLOAT_TO_INTEGER: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            double v = menai_float_value(a);
            MenaiBigInt res;
            menai_bigint_init(&res);
            vm_err = menai_bigint_from_double(trunc(v), &res);
            if (vm_err < 0) {
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *r = doubles_to_menai_complex(menai_float_value(a), menai_float_value(b));
            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_FLOAT_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FLOAT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
                vm_err = MENAI_ERR_CLOSURE_INDEX_OUT_OF_RANGE;
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
            MenaiValue *closure = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_FUNCTION(closure))) {
                vm_err = MENAI_ERR_PATCH_CLOSURE_NOT_FUNCTION;
                goto error;
            }

            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *val = regs[base + src2];
            MenaiFunction *fn = (MenaiFunction *)closure;

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            MenaiComplex *cb = (MenaiComplex *)b;
            bool_store(regs, base + dest, ca->real == cb->real && ca->imag == cb->imag);
            break;
        }

        case OP_COMPLEX_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            MenaiComplex *cb = (MenaiComplex *)b;
            bool_store(regs, base + dest, ca->real != cb->real || ca->imag != cb->imag);
            break;
        }

        case OP_COMPLEX_REAL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_fr = double_to_menai_float(((MenaiComplex *)a)->real);
            if (_fr == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_IMAG: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_fr = double_to_menai_float(((MenaiComplex *)a)->imag);
            if (_fr == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_ABS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            double re = ca->real;
            double im = ca->imag;
            MenaiValue *_fr = double_to_menai_float(sqrt(re * re + im * im));
            if (_fr == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _fr);
            break;
        }

        case OP_COMPLEX_NEG: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            MenaiValue *_r = doubles_to_menai_complex(-ca->real, -ca->imag);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_ADD: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            MenaiComplex *cb = (MenaiComplex *)b;
            MenaiValue *_r = doubles_to_menai_complex(ca->real + cb->real, ca->imag + cb->imag);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SUB: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            MenaiComplex *cb = (MenaiComplex *)b;
            MenaiValue *_r = doubles_to_menai_complex(ca->real - cb->real, ca->imag - cb->imag);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_MUL: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            MenaiComplex *cb = (MenaiComplex *)b;
            double ar = ca->real;
            double ai = ca->imag;
            double br = cb->real;
            double bi = cb->imag;
            MenaiValue *_r = doubles_to_menai_complex(ar * br - ai * bi, ar * bi + ai * br);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_DIV: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            MenaiComplex *cb = (MenaiComplex *)b;
            double ar = ca->real;
            double ai = ca->imag;
            double br = cb->real;
            double bi = cb->imag;
            if (br == 0.0 && bi == 0.0) {
                vm_err = MENAI_ERR_DIVISION_BY_ZERO;
                goto error;
            }

            double denom = br * br + bi * bi;
            MenaiValue *_r = doubles_to_menai_complex(
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            MenaiComplex *cb = (MenaiComplex *)b;
            mc_t za = mc(ca->real, ca->imag);
            mc_t zb = mc(cb->real, cb->imag);
            mc_t cr = mc_pow(za, zb);
            MenaiValue *_r = doubles_to_menai_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_EXP: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            mc_t z = mc(ca->real, ca->imag);
            mc_t cr = mc_exp(z);
            MenaiValue *_r = doubles_to_menai_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOG: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            mc_t z = mc(ca->real, ca->imag);
            mc_t cr = mc_log(z);
            MenaiValue *_r = doubles_to_menai_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOG10: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            mc_t z = mc(ca->real, ca->imag);
            mc_t cr = mc_log10(z);
            MenaiValue *_r = doubles_to_menai_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SIN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            mc_t z = mc(ca->real, ca->imag);
            mc_t cr = mc_sin(z);
            MenaiValue *_r = doubles_to_menai_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_COS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            mc_t z = mc(ca->real, ca->imag);
            mc_t cr = mc_cos(z);
            MenaiValue *_r = doubles_to_menai_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_TAN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            mc_t z = mc(ca->real, ca->imag);
            mc_t cr = mc_tan(z);
            MenaiValue *_r = doubles_to_menai_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_SQRT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            mc_t z = mc(ca->real, ca->imag);
            mc_t cr = mc_sqrt(z);
            MenaiValue *_r = doubles_to_menai_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_LOGN: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiComplex *ca = (MenaiComplex *)a;
            MenaiComplex *cb = (MenaiComplex *)b;
            mc_t za = mc(ca->real, ca->imag);
            mc_t zb = mc(cb->real, cb->imag);
            if (mc_zero(zb)) {
                vm_err = MENAI_ERR_INVALID_LOG_BASE;
                goto error;
            }

            mc_t cr = mc_logn(za, zb);
            MenaiValue *_r = doubles_to_menai_complex(cr.re, cr.im);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_COMPLEX_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_COMPLEX(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_string_equal(a, b));
            break;
        }

        case OP_STRING_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, !menai_string_equal(a, b));
            break;
        }

        case OP_STRING_LT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_string_compare(a, b) < 0);
            break;
        }

        case OP_STRING_GT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_string_compare(a, b) > 0);
            break;
        }

        case OP_STRING_LTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_string_compare(a, b) <= 0);
            break;
        }

        case OP_STRING_GTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_string_compare(a, b) >= 0);
            break;
        }

        case OP_STRING_LENGTH: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = ssize_t_to_menai_integer(menai_string_length(a));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_STRING_UPCASE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_string_has_prefix(a, b));
            break;
        }

        case OP_STRING_SUFFIX_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_string_has_suffix(a, b));
            break;
        }

        case OP_STRING_REF: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_INDEX_NOT_INTEGER;
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            long idx_l;
            if (!ib->is_big) {
                idx_l = ib->small;
            } else {
                vm_err = menai_bigint_to_long(&ib->big, &idx_l);
                if (vm_err < 0) {
                    goto error;
                }
            }

            ssize_t idx = (ssize_t)idx_l;
            ssize_t slen = menai_string_length(a);
            if (idx < 0 || idx >= slen) {
                vm_err = MENAI_ERR_INDEX_OUT_OF_RANGE;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_SLICE_INDICES_NOT_INTEGER;
                goto error;
            }

            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *c = regs[base + src2];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(c))) {
                vm_err = MENAI_ERR_SLICE_INDICES_NOT_INTEGER;
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            MenaiInteger *ic = (MenaiInteger *)c;
            long start_l, end_l;
            if (!ib->is_big) {
                start_l = ib->small;
            } else {
                vm_err = menai_bigint_to_long(&ib->big, &start_l);
                if (vm_err < 0) {
                    goto error;
                }
            }

            if (!ic->is_big) {
                end_l = ic->small;
            } else {
                vm_err = menai_bigint_to_long(&ic->big, &end_l);
                if (vm_err < 0) {
                    goto error;
                }
            }

            ssize_t start = (ssize_t)start_l, end = (ssize_t)end_l;
            ssize_t slen = menai_string_length(a);
            if (start < 0) {
                vm_err = MENAI_ERR_NEGATIVE_SLICE_INDEX;
                goto error;
            }

            if (end < 0) {
                vm_err = MENAI_ERR_NEGATIVE_SLICE_INDEX;
                goto error;
            }

            if (start > slen) {
                vm_err = MENAI_ERR_SLICE_START_OUT_OF_RANGE;
                goto error;
            }

            if (end > slen) {
                vm_err = MENAI_ERR_SLICE_END_OUT_OF_RANGE;
                goto error;
            }

            if (start > end) {
                vm_err = MENAI_ERR_SLICE_START_AFTER_END;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *c = regs[base + src2];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(c))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            ssize_t idx = menai_string_find(a, b);
            if (idx == -2) {
                goto error;
            }

            if (idx == -1) {
                menai_reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                MenaiValue *_r = ssize_t_to_menai_integer(idx);
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
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            ssize_t slen = menai_string_length(a);
            if (slen != 1) {
                vm_err = MENAI_ERR_NOT_SINGLE_CHAR_STRING;
                goto error;
            }

            MenaiValue *_r = long_to_menai_integer((long)menai_string_get(a, 0));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_STRING_TO_INTEGER: {
            /* src0=string, src1=radix(integer) */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_RADIX_NOT_INTEGER;
                goto error;
            }

            MenaiInteger *ib = (MenaiInteger *)b;
            long radix;
            if (!ib->is_big) {
                radix = ib->small;
            } else {
                vm_err = menai_bigint_to_long(&ib->big, &radix);
                if (vm_err < 0) {
                    goto error;
                }
            }

            if (radix != 2 && radix != 8 && radix != 10 && radix != 16) {
                vm_err = MENAI_ERR_INVALID_RADIX;
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
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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

            /* Try complex if the string contains 'j' or 'J'. */
            if (stn_has_j) {
                double stn_re, stn_im;
                if (parse_complex_string(stn_buf, &stn_re, &stn_im)) {
                    MenaiValue *r = doubles_to_menai_complex(stn_re, stn_im);
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
                MenaiValue *_r = double_to_menai_float(stn_dv);
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            ssize_t alen = menai_string_length(a);
            ssize_t blen = menai_string_length(b);
            const uint32_t *adata = menai_string_data(a);
            const uint32_t *bdata = menai_string_data(b);
            MenaiValue *r;
            if (blen == 0) {
                /* Split into individual codepoints */
                MenaiValue *r_stl = menai_list_alloc(alen);
                if (!r_stl) {
                    vm_err = MENAI_ERR_NOMEM;
                    goto error;
                }

                MenaiValue **stl_arr = menai_list_elements(r_stl);
                for (ssize_t i = 0; i < alen; i++) {
                    stl_arr[i] = menai_string_from_codepoint(adata[i]);
                    if (!stl_arr[i]) {
                        for (ssize_t k = 0; k < i; k++) {
                            menai_release(stl_arr[k]);
                        }

                        menai_release(r_stl);
                        goto error;
                    }
                }

                r = r_stl;
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
                MenaiValue *r_parts = menai_list_alloc(nparts);
                if (!r_parts) {
                    vm_err = MENAI_ERR_NOMEM;
                    goto error;
                }

                MenaiValue **parts2 = menai_list_elements(r_parts);
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

                            menai_release(r_parts);
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

                r = r_parts;
            }

            if (r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_BYTES_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_BYTES(regs[base + src0]));
            break;
        }

        case OP_BYTES_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_bytes_equal(a, b));
            break;
        }

        case OP_BYTES_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, !menai_bytes_equal(a, b));
            break;
        }

        case OP_BYTES_LENGTH: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = ssize_t_to_menai_integer(menai_bytes_length(a));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_BYTES_REF: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *idx_val = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(idx_val))) {
                vm_err = MENAI_ERR_OFFSET_NOT_INTEGER;
                goto error;
            }

            ssize_t offset;
            if (MENAI_UNLIKELY(menai_integer_to_ssize_t(idx_val, &offset) < 0)) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }

            ssize_t blen = menai_bytes_length(b);
            if (offset < 0 || offset >= blen) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }

            MenaiValue *_r = menai_bytes_ref(b, offset);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_BYTES_APPEND_U8: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *v = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(v))) {
                vm_err = MENAI_ERR_VALUE_NOT_INTEGER;
                goto error;
            }

            long val;
            if (MENAI_UNLIKELY(menai_integer_to_long(v, &val) < 0)) {
                vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE;
                goto error;
            }

            if (val < 0 || val > 255) {
                vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE;
                goto error;
            }

            MenaiValue *_r = menai_bytes_append_u8(b, (uint8_t)val);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_LIST_TO_BYTES: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *lst = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(lst))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            ssize_t n = menai_list_length(lst);
            MenaiValue *result = menai_bytes_alloc(n);
            if (result == NULL) {
                goto error;
            }

            MenaiBytes *mb = (MenaiBytes *)result;
            for (ssize_t i = 0; i < n; i++) {
                MenaiValue *elem = menai_list_get((MenaiList *)lst, i);
                if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(elem))) {
                    vm_err = MENAI_ERR_LIST_ELEMENTS_NOT_INTEGERS;
                    menai_release(result);
                    goto error;
                }

                long val;
                if (MENAI_UNLIKELY(menai_integer_to_long(elem, &val) < 0)) {
                    vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE;
                    menai_release(result);
                    goto error;
                }

                if (val < 0 || val > 255) {
                    vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE;
                    menai_release(result);
                    goto error;
                }

                mb->inline_data[i] = (uint8_t)val;
            }

            menai_reg_set_own(regs, base + dest, result);
            break;
        }

        case OP_BYTES_SLICE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *start_val = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(start_val))) {
                vm_err = MENAI_ERR_SLICE_START_NOT_INTEGER;
                goto error;
            }

            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *end_val = regs[base + src2];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(end_val))) {
                vm_err = MENAI_ERR_SLICE_END_NOT_INTEGER;
                goto error;
            }

            ssize_t blen = menai_bytes_length(b);
            ssize_t start;
            if (MENAI_UNLIKELY(menai_integer_to_ssize_t(start_val, &start) < 0)) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }

            ssize_t end;
            if (MENAI_UNLIKELY(menai_integer_to_ssize_t(end_val, &end) < 0)) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }

            if (start < 0) {
                start = 0;
            }

            if (end > blen) {
                end = blen;
            }

            if (start > end) {
                start = end;
            }

            MenaiValue *_r = menai_bytes_slice(b, start, end);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_STRING_TO_BYTES: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *s = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(s))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            ssize_t slen = menai_string_length(s);
            const uint32_t *cp = menai_string_data(s);

            /* Encode UTF-32 codepoints to UTF-8 bytes */
            ssize_t nbytes = 0;
            for (ssize_t i = 0; i < slen; i++) {
                uint32_t c = cp[i];
                if (c < 0x80) {
                    nbytes += 1;
                } else if (c < 0x800) {
                    nbytes += 2;
                } else if (c < 0x10000) {
                    nbytes += 3;
                } else {
                    nbytes += 4;
                }
            }

            MenaiValue *result = menai_bytes_alloc(nbytes);
            if (result == NULL) {
                goto error;
            }

            MenaiBytes *mb = (MenaiBytes *)result;
            ssize_t pos = 0;
            for (ssize_t i = 0; i < slen; i++) {
                uint32_t c = cp[i];
                if (c < 0x80) {
                    mb->inline_data[pos++] = (uint8_t)c;
                } else if (c < 0x800) {
                    mb->inline_data[pos++] = (uint8_t)(0xC0 | (c >> 6));
                    mb->inline_data[pos++] = (uint8_t)(0x80 | (c & 0x3F));
                } else if (c < 0x10000) {
                    mb->inline_data[pos++] = (uint8_t)(0xE0 | (c >> 12));
                    mb->inline_data[pos++] = (uint8_t)(0x80 | ((c >> 6) & 0x3F));
                    mb->inline_data[pos++] = (uint8_t)(0x80 | (c & 0x3F));
                } else {
                    mb->inline_data[pos++] = (uint8_t)(0xF0 | (c >> 18));
                    mb->inline_data[pos++] = (uint8_t)(0x80 | ((c >> 12) & 0x3F));
                    mb->inline_data[pos++] = (uint8_t)(0x80 | ((c >> 6) & 0x3F));
                    mb->inline_data[pos++] = (uint8_t)(0x80 | (c & 0x3F));
                }
            }

            menai_reg_set_own(regs, base + dest, result);
            break;
        }

        case OP_BYTES_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            ssize_t nbytes = menai_bytes_length(b);
            const uint8_t *data = menai_bytes_data(b);

            /* Decode UTF-8 to UTF-32 codepoints */
            ssize_t ncp = 0;
            ssize_t i = 0;
            while (i < nbytes) {
                uint8_t c = data[i];
                if (c < 0x80) {
                    i += 1;
                } else if ((c & 0xE0) == 0xC0) {
                    if (i + 2 > nbytes || (data[i+1] & 0xC0) != 0x80) {
                        vm_err = MENAI_ERR_INVALID_UTF8;
                        goto error;
                    }
                    i += 2;
                } else if ((c & 0xF0) == 0xE0) {
                    if (i + 3 > nbytes || (data[i+1] & 0xC0) != 0x80 || (data[i+2] & 0xC0) != 0x80) {
                        vm_err = MENAI_ERR_INVALID_UTF8;
                        goto error;
                    }
                    i += 3;
                } else if ((c & 0xF8) == 0xF0) {
                    if (i + 4 > nbytes || (data[i+1] & 0xC0) != 0x80 || (data[i+2] & 0xC0) != 0x80 || (data[i+3] & 0xC0) != 0x80) {
                        vm_err = MENAI_ERR_INVALID_UTF8;
                        goto error;
                    }
                    i += 4;
                } else {
                    vm_err = MENAI_ERR_INVALID_UTF8;
                    goto error;
                }
                ncp++;
            }

            uint32_t *cp_buf = (uint32_t *)malloc((size_t)ncp * sizeof(uint32_t));
            if (!cp_buf) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            i = 0;
            ssize_t cp_idx = 0;
            while (i < nbytes) {
                uint8_t c = data[i];
                if (c < 0x80) {
                    cp_buf[cp_idx++] = c;
                    i += 1;
                } else if ((c & 0xE0) == 0xC0) {
                    cp_buf[cp_idx++] = ((uint32_t)(c & 0x1F) << 6) | (data[i+1] & 0x3F);
                    i += 2;
                } else if ((c & 0xF0) == 0xE0) {
                    cp_buf[cp_idx++] = ((uint32_t)(c & 0x0F) << 12) | ((uint32_t)(data[i+1] & 0x3F) << 6) | (data[i+2] & 0x3F);
                    i += 3;
                } else {
                    cp_buf[cp_idx++] = ((uint32_t)(c & 0x07) << 18) | ((uint32_t)(data[i+1] & 0x3F) << 12) | ((uint32_t)(data[i+2] & 0x3F) << 6) | (data[i+3] & 0x3F);
                    i += 4;
                }
            }

            MenaiValue *result = menai_string_from_codepoints(cp_buf, ncp);
            free(cp_buf);
            if (result == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, result);
            break;
        }

        case OP_BYTES_TO_LIST: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            ssize_t nbytes = menai_bytes_length(b);
            MenaiValue *result = menai_list_alloc(nbytes);
            if (result == NULL) {
                goto error;
            }

            MenaiValue **arr = menai_list_elements(result);
            const uint8_t *data = menai_bytes_data(b);
            for (ssize_t i = 0; i < nbytes; i++) {
                arr[i] = menai_integer_from_long((long)data[i]);
                if (arr[i] == NULL) {
                    for (ssize_t j = 0; j < i; j++) {
                        menai_release(arr[j]);
                    }

                    menai_release(result);
                    goto error;
                }
            }

            menai_reg_set_own(regs, base + dest, result);
            break;
        }

        case OP_BYTES_TO_STRING_HEX: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            ssize_t nbytes = menai_bytes_length(b);
            const uint8_t *data = menai_bytes_data(b);

            uint32_t *cp_buf = (uint32_t *)malloc((size_t)(nbytes * 2) * sizeof(uint32_t));
            if (!cp_buf) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            static const uint32_t hex_chars[] = {
                '0', '1', '2', '3', '4', '5', '6', '7',
                '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
            };

            for (ssize_t i = 0; i < nbytes; i++) {
                cp_buf[i * 2] = hex_chars[(data[i] >> 4) & 0xF];
                cp_buf[i * 2 + 1] = hex_chars[data[i] & 0xF];
            }

            MenaiValue *result = menai_string_from_codepoints(cp_buf, nbytes * 2);
            free(cp_buf);
            if (result == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, result);
            break;
        }

        case OP_STRING_HEX_TO_BYTES: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *s = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(s))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            ssize_t slen = menai_string_length(s);
            const uint32_t *cp = menai_string_data(s);

            if (slen % 2 != 0) {
                vm_err = MENAI_ERR_HEX_EVEN_LENGTH;
                goto error;
            }

            ssize_t nbytes = slen / 2;
            MenaiValue *result = menai_bytes_alloc(nbytes);
            if (result == NULL) {
                goto error;
            }

            MenaiBytes *mb = (MenaiBytes *)result;
            for (ssize_t i = 0; i < nbytes; i++) {
                uint32_t hi = cp[i * 2];
                uint32_t lo = cp[i * 2 + 1];
                int hi_val = -1, lo_val = -1;

                if (hi >= '0' && hi <= '9') {
                    hi_val = hi - '0';
                } else if (hi >= 'a' && hi <= 'f') {
                    hi_val = hi - 'a' + 10;
                } else if (hi >= 'A' && hi <= 'F') {
                    hi_val = hi - 'A' + 10;
                }

                if (lo >= '0' && lo <= '9') {
                    lo_val = lo - '0';
                } else if (lo >= 'a' && lo <= 'f') {
                    lo_val = lo - 'a' + 10;
                } else if (lo >= 'A' && lo <= 'F') {
                    lo_val = lo - 'A' + 10;
                }

                if (hi_val < 0 || lo_val < 0) {
                    vm_err = MENAI_ERR_INVALID_HEX_CHAR;
                    menai_release(result);
                    goto error;
                }

                mb->inline_data[i] = (uint8_t)((hi_val << 4) | lo_val);
            }

            menai_reg_set_own(regs, base + dest, result);
            break;
        }

        case OP_BYTES_CONCAT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = menai_bytes_concat(a, b);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_BYTES_INDEX: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *haystack = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(haystack))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *needle = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(needle))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            ssize_t nlen = menai_bytes_length(needle);
            ssize_t hlen = menai_bytes_length(haystack);
            if (nlen == 0) {
                menai_reg_set_borrow(regs, base + dest, ssize_t_to_menai_integer(0));
                break;
            }

            if (nlen > hlen) {
                menai_reg_set_borrow(regs, base + dest, Menai_NONE);
                break;
            }

            const uint8_t *nd = menai_bytes_data(needle);
            const uint8_t *hd = menai_bytes_data(haystack);
            ssize_t limit = hlen - nlen;
            ssize_t found = -1;
            for (ssize_t i = 0; i <= limit; i++) {
                if (memcmp(hd + i, nd, (size_t)nlen) == 0) {
                    found = i;
                    break;
                }
            }

            if (found == -1) {
                menai_reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                MenaiValue *_r = ssize_t_to_menai_integer(found);
                if (_r == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
            }
            break;
        }

        case OP_BYTES_INDEX_INT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *byte_val = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(byte_val))) {
                vm_err = MENAI_ERR_BYTE_NOT_INTEGER;
                goto error;
            }

            long target;
            if (MENAI_UNLIKELY(menai_integer_to_long(byte_val, &target) < 0)) {
                vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE;
                goto error;
            }

            if (target < 0 || target > 255) {
                vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE;
                goto error;
            }

            ssize_t blen = menai_bytes_length(b);
            const uint8_t *data = menai_bytes_data(b);
            ssize_t found = -1;
            for (ssize_t i = 0; i < blen; i++) {
                if (data[i] == (uint8_t)target) {
                    found = i;
                    break;
                }
            }

            if (found == -1) {
                menai_reg_set_borrow(regs, base + dest, Menai_NONE);
            } else {
                MenaiValue *_r = ssize_t_to_menai_integer(found);
                if (_r == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
            }
            break;
        }

        case OP_BYTES_LT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_bytes_compare(a, b) < 0);
            break;
        }

        case OP_BYTES_GT_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_bytes_compare(a, b) > 0);
            break;
        }

        case OP_BYTES_LTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_bytes_compare(a, b) <= 0);
            break;
        }

        case OP_BYTES_GTE_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            bool_store(regs, base + dest, menai_bytes_compare(a, b) >= 0);
            break;
        }

        case OP_BYTES_READ_U8: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *off_val = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(off_val))) {
                vm_err = MENAI_ERR_OFFSET_NOT_INTEGER;
                goto error;
            }

            ssize_t offset;
            if (MENAI_UNLIKELY(menai_integer_to_ssize_t(off_val, &offset) < 0)) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }

            ssize_t blen = menai_bytes_length(b);
            if (offset < 0 || offset >= blen) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }

            MenaiValue *_r = menai_integer_from_long((long)menai_bytes_get(b, offset));
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_BYTES_READ_I8: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *off_val = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(off_val))) {
                vm_err = MENAI_ERR_OFFSET_NOT_INTEGER;
                goto error;
            }

            ssize_t offset;
            if (MENAI_UNLIKELY(menai_integer_to_ssize_t(off_val, &offset) < 0)) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }

            ssize_t blen = menai_bytes_length(b);
            if (offset < 0 || offset >= blen) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }

            int8_t val = (int8_t)menai_bytes_get(b, offset);
            MenaiValue *_r = menai_integer_from_long((long)val);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        /* Multi-byte read helpers using a shared pattern.  Each reads N bytes
         * at the given offset, assembles them in the specified endianness,
         * and returns an integer.  */
#define BYTES_READ_MULTI(opcode_name, width, is_signed, le) \
        case opcode_name: { \
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK); \
            MenaiValue *b = regs[base + src0]; \
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) { \
                vm_err = MENAI_ERR_TYPE_MISMATCH; \
                goto error; \
            } \
\
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK); \
            MenaiValue *off_val = regs[base + src1]; \
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(off_val))) { \
                vm_err = MENAI_ERR_OFFSET_NOT_INTEGER; \
                goto error; \
            } \
\
            ssize_t offset; \
            if (MENAI_UNLIKELY(menai_integer_to_ssize_t(off_val, &offset) < 0)) { \
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS; \
                goto error; \
            } \
\
            ssize_t blen = menai_bytes_length(b); \
            if (offset < 0 || offset + (width) > blen) { \
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS; \
                goto error; \
            } \
\
            const uint8_t *d = menai_bytes_data(b) + offset; \
            unsigned long uval = 0; \
            if (le) { \
                for (int _i = 0; _i < (width); _i++) { \
                    uval |= ((unsigned long)d[_i]) << (_i * 8); \
                } \
            } else { \
                for (int _i = 0; _i < (width); _i++) { \
                    uval = (uval << 8) | d[_i]; \
                } \
            } \
\
            if (is_signed) { \
                unsigned long sign_bit = 1UL << ((width) * 8 - 1); \
                long sval; \
                if (uval & sign_bit) { \
                    if ((width) == sizeof(unsigned long)) { \
                        sval = (long)uval; \
                    } else { \
                        sval = (long)(uval | ~((1UL << (width) * 8) - 1)); \
                    } \
                } else { \
                    sval = (long)uval; \
                } \
\
                MenaiValue *_r = menai_integer_from_long(sval); \
                if (_r == NULL) { \
                    goto error; \
                } \
\
                menai_reg_set_own(regs, base + dest, _r); \
            } else { \
                if ((width) == sizeof(unsigned long) && (long)uval < 0) { \
                    /* Unsigned 64-bit value exceeds LONG_MAX */ \
                    MenaiBigInt big; \
                    menai_bigint_init(&big); \
                    vm_err = menai_bigint_from_unsigned_long_long((unsigned long long)uval, &big); \
                    if (vm_err < 0) { \
                        goto error; \
                    } \
\
                    MenaiValue *_r = menai_integer_from_bigint(big); \
                    if (_r == NULL) { \
                        goto error; \
                    } \
\
                    menai_reg_set_own(regs, base + dest, _r); \
                } else { \
                    MenaiValue *_r = menai_integer_from_long((long)uval); \
                    if (_r == NULL) { \
                        goto error; \
                    } \
\
                    menai_reg_set_own(regs, base + dest, _r); \
                } \
            } \
            break; \
        }

        BYTES_READ_MULTI(OP_BYTES_READ_U16_LE, 2, 0, 1)
        BYTES_READ_MULTI(OP_BYTES_READ_U24_LE, 3, 0, 1)
        BYTES_READ_MULTI(OP_BYTES_READ_U32_LE, 4, 0, 1)
        BYTES_READ_MULTI(OP_BYTES_READ_U64_LE, 8, 0, 1)
        BYTES_READ_MULTI(OP_BYTES_READ_U16_BE, 2, 0, 0)
        BYTES_READ_MULTI(OP_BYTES_READ_U24_BE, 3, 0, 0)
        BYTES_READ_MULTI(OP_BYTES_READ_U32_BE, 4, 0, 0)
        BYTES_READ_MULTI(OP_BYTES_READ_U64_BE, 8, 0, 0)
        BYTES_READ_MULTI(OP_BYTES_READ_I16_LE, 2, 1, 1)
        BYTES_READ_MULTI(OP_BYTES_READ_I24_LE, 3, 1, 1)
        BYTES_READ_MULTI(OP_BYTES_READ_I32_LE, 4, 1, 1)
        BYTES_READ_MULTI(OP_BYTES_READ_I64_LE, 8, 1, 1)
        BYTES_READ_MULTI(OP_BYTES_READ_I16_BE, 2, 1, 0)
        BYTES_READ_MULTI(OP_BYTES_READ_I24_BE, 3, 1, 0)
        BYTES_READ_MULTI(OP_BYTES_READ_I32_BE, 4, 1, 0)
        BYTES_READ_MULTI(OP_BYTES_READ_I64_BE, 8, 1, 0)

#undef BYTES_READ_MULTI

        /*
         * Multi-byte append helpers using a shared pattern.  Each takes bytes
         * and an integer value, encodes the value into N bytes in the specified
         * endianness, appends them, and returns the new bytes.
         * For signed variants the value range check uses the signed range.
         */
#define BYTES_APPEND_MULTI(opcode_name, width, is_signed, le) \
        case opcode_name: { \
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK); \
            MenaiValue *b = regs[base + src0]; \
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) { \
                vm_err = MENAI_ERR_TYPE_MISMATCH; \
                goto error; \
            } \
\
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK); \
            MenaiValue *v = regs[base + src1]; \
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(v))) { \
                vm_err = MENAI_ERR_VALUE_NOT_INTEGER; \
                goto error; \
            } \
\
            long val; \
            if (is_signed) { \
                if (MENAI_UNLIKELY(menai_integer_to_long(v, &val) < 0)) { \
                    vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE; \
                    goto error; \
                } \
\
                if ((width) < (int)sizeof(long)) { \
                   long max_val = (long)((1UL << ((width) * 8 - 1)) - 1); \
                   if (val < -max_val - 1 || val > max_val) { \
                       vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE; \
                       goto error; \
                   } \
                } \
                unsigned long uval = (unsigned long)val; \
                MenaiValue *_r = menai_bytes_append_multi(b, uval, (width), le); \
                if (_r == NULL) { \
                    goto error; \
                } \
\
                menai_reg_set_own(regs, base + dest, _r); \
            } else { \
                unsigned long long uval_ull; \
                if (menai_integer_to_unsigned_long_long(v, &uval_ull) < 0) { \
                    vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE; \
                    goto error; \
                } \
\
                if ((width) < (int)sizeof(unsigned long long) && uval_ull > ((1ULL << ((width) * 8)) - 1)) { \
                    vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE; \
                    goto error; \
                } \
\
                MenaiValue *_r = menai_bytes_append_multi(b, (unsigned long)uval_ull, (width), le); \
                if (_r == NULL) { \
                    goto error; \
                } \
\
                menai_reg_set_own(regs, base + dest, _r); \
            } \
            break; \
        }

        BYTES_APPEND_MULTI(OP_BYTES_APPEND_U16_LE, 2, 0, 1)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_U16_BE, 2, 0, 0)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_U24_LE, 3, 0, 1)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_U24_BE, 3, 0, 0)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_U32_LE, 4, 0, 1)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_U32_BE, 4, 0, 0)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_U64_LE, 8, 0, 1)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_U64_BE, 8, 0, 0)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_I8, 1, 1, 1)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_I16_LE, 2, 1, 1)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_I16_BE, 2, 1, 0)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_I24_LE, 3, 1, 1)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_I24_BE, 3, 1, 0)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_I32_LE, 4, 1, 1)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_I32_BE, 4, 1, 0)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_I64_LE, 8, 1, 1)
        BYTES_APPEND_MULTI(OP_BYTES_APPEND_I64_BE, 8, 1, 0)

#undef BYTES_APPEND_MULTI

        /*
         * Multi-byte write helpers.  Each takes bytes, offset, and integer value,
         * writes the encoded value at the offset, and returns the new bytes.
         * For signed variants the value range check uses the signed range.
         */
#define BYTES_WRITE_MULTI(opcode_name, width, is_signed, le) \
        case opcode_name: { \
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK); \
            MenaiValue *b = regs[base + src0]; \
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) { \
                vm_err = MENAI_ERR_TYPE_MISMATCH; \
                goto error; \
            } \
\
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK); \
            MenaiValue *off_val = regs[base + src1]; \
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(off_val))) { \
                vm_err = MENAI_ERR_OFFSET_NOT_INTEGER; \
                goto error; \
            } \
\
            int src2 = (int)(word & FIELD_MASK); \
            MenaiValue *v = regs[base + src2]; \
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(v))) { \
                vm_err = MENAI_ERR_VALUE_NOT_INTEGER; \
                goto error; \
            } \
\
            ssize_t offset; \
            if (MENAI_UNLIKELY(menai_integer_to_ssize_t(off_val, &offset) < 0)) { \
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS; \
                goto error; \
            } \
\
            ssize_t blen = menai_bytes_length(b); \
            if (offset < 0 || offset + (width) > blen) { \
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS; \
                goto error; \
            } \
\
            unsigned long long uval_ull; \
            if (is_signed) { \
                long val; \
                if (menai_integer_to_long(v, &val) < 0) { \
                    vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE; \
                    goto error; \
                } \
\
                if ((width) < (int)sizeof(long)) { \
                    long max_val = (long)((1UL << ((width) * 8 - 1)) - 1); \
                    if (val < -max_val - 1 || val > max_val) { \
                        vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE; \
                        goto error; \
                    } \
                } \
                uval_ull = (unsigned long long)(unsigned long)val; \
            } else { \
                if (menai_integer_to_unsigned_long_long(v, &uval_ull) < 0) { \
                    vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE; \
                    goto error; \
                } \
\
                if ((width) < (int)sizeof(unsigned long long) && \
                        uval_ull > ((1ULL << ((width) * 8)) - 1)) { \
                    vm_err = MENAI_ERR_VALUE_OUT_OF_RANGE; \
                    goto error; \
                } \
            } \
            MenaiValue *_r = menai_bytes_write_multi(b, offset, (unsigned long)uval_ull, (width), le); \
            if (_r == NULL) { \
                goto error; \
            } \
\
            menai_reg_set_own(regs, base + dest, _r); \
            break; \
        }

        BYTES_WRITE_MULTI(OP_BYTES_WRITE_U8, 1, 0, 1)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_U16_LE, 2, 0, 1)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_U16_BE, 2, 0, 0)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_U24_LE, 3, 0, 1)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_U24_BE, 3, 0, 0)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_U32_LE, 4, 0, 1)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_U32_BE, 4, 0, 0)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_U64_LE, 8, 0, 1)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_U64_BE, 8, 0, 0)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_I8, 1, 1, 1)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_I16_LE, 2, 1, 1)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_I16_BE, 2, 1, 0)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_I24_LE, 3, 1, 1)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_I24_BE, 3, 1, 0)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_I32_LE, 4, 1, 1)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_I32_BE, 4, 1, 0)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_I64_LE, 8, 1, 1)
        BYTES_WRITE_MULTI(OP_BYTES_WRITE_I64_BE, 8, 1, 0)

#undef BYTES_WRITE_MULTI

        /*
         * LEB128 read (unsigned).  Returns a two-element list (value next-offset).
         */
        case OP_BYTES_READ_ULEB128: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *off_val = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(off_val))) {
                vm_err = MENAI_ERR_OFFSET_NOT_INTEGER;
                goto error;
            }

            ssize_t offset;
            if (MENAI_UNLIKELY(menai_integer_to_ssize_t(off_val, &offset) < 0)) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }
            ssize_t blen = menai_bytes_length(b);
            if (offset < 0 || offset >= blen) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }

            const uint8_t *d = menai_bytes_data(b);
            unsigned long long result = 0;
            int shift = 0;
            ssize_t pos = offset;
            int byte;
            do {
                if (pos >= blen) {
                    vm_err = MENAI_ERR_TRUNCATED_LEB128;
                    goto error;
                }
                byte = d[pos];
                result |= ((unsigned long long)(byte & 0x7F)) << shift;
                shift += 7;
                pos++;
            } while (byte & 0x80);

            MenaiValue *val_result;
            if ((long long)result < 0) {
                /* Value exceeds LONG_MAX — use bigint path */
                MenaiBigInt big;
                menai_bigint_init(&big);
                vm_err = menai_bigint_from_unsigned_long_long(result, &big);
                if (vm_err < 0) {
                    goto error;
                }
                val_result = menai_integer_from_bigint(big);
                if (val_result == NULL) {
                    goto error;
                }
            } else {
                val_result = menai_integer_from_long((long)result);
                if (val_result == NULL) {
                    goto error;
                }
            }

            MenaiValue *next_off = menai_integer_from_long((long)pos);
            if (next_off == NULL) {
                menai_release(val_result);
                goto error;
            }

            MenaiValue *lst = menai_list_alloc(2);
            if (lst == NULL) {
                menai_release(val_result);
                menai_release(next_off);
                goto error;
            }

            MenaiValue **elems = menai_list_elements(lst);
            elems[0] = val_result;
            elems[1] = next_off;
            menai_reg_set_own(regs, base + dest, lst);
            break;
        }

        /*
         * LEB128 append (unsigned).  Encodes value as unsigned LEB128 and appends.
         */
        case OP_BYTES_APPEND_ULEB128: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *v = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(v))) {
                vm_err = MENAI_ERR_VALUE_NOT_INTEGER;
                goto error;
            }

            unsigned long long uval;
            if (MENAI_UNLIKELY(menai_integer_to_unsigned_long_long(v, &uval) < 0)) {
                vm_err = MENAI_ERR_NEGATIVE_ARGUMENT;
                goto error;
            }
            uint8_t buf[10];
            int nbytes = 0;
            do {
                buf[nbytes++] = (uint8_t)(uval & 0x7F);
                uval >>= 7;
            } while (uval != 0);

            for (int i = 0; i < nbytes - 1; i++) {
                buf[i] |= 0x80;
            }

            MenaiValue *result = b;
            menai_retain(result);
            for (int i = 0; i < nbytes; i++) {
                MenaiValue *next = menai_bytes_append_u8(result, buf[i]);
                menai_release(result);
                if (next == NULL) {
                    goto error;
                }
                result = next;
            }
            menai_reg_set_own(regs, base + dest, result);
            break;
        }

        /*
         * LEB128 read (signed).  Returns a two-element list (value next-offset).
         */
        case OP_BYTES_READ_SLEB128: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *off_val = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(off_val))) {
                vm_err = MENAI_ERR_OFFSET_NOT_INTEGER;
                goto error;
            }

            ssize_t offset;
            if (MENAI_UNLIKELY(menai_integer_to_ssize_t(off_val, &offset) < 0)) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }

            ssize_t blen = menai_bytes_length(b);
            if (offset < 0 || offset >= blen) {
                vm_err = MENAI_ERR_OFFSET_OUT_OF_BOUNDS;
                goto error;
            }

            const uint8_t *d = menai_bytes_data(b);
            long result = 0;
            int shift = 0;
            ssize_t pos = offset;
            int byte;
            do {
                if (pos >= blen) {
                    vm_err = MENAI_ERR_TRUNCATED_LEB128;
                    goto error;
                }
                byte = d[pos];
                result |= (byte & 0x7F) << shift;
                shift += 7;
                pos++;
            } while (byte & 0x80);

            /* Sign extend if the sign bit is set */
            if (shift < 64 && (byte & 0x40)) {
                result |= -1L << shift;
            }

            MenaiValue *val_result = menai_integer_from_long(result);
            if (val_result == NULL) {
                goto error;
            }

            MenaiValue *next_off = menai_integer_from_long((long)pos);
            if (next_off == NULL) {
                menai_release(val_result);
                goto error;
            }

            MenaiValue *lst = menai_list_alloc(2);
            if (lst == NULL) {
                menai_release(val_result);
                menai_release(next_off);
                goto error;
            }

            MenaiValue **elems = menai_list_elements(lst);
            elems[0] = val_result;
            elems[1] = next_off;
            menai_reg_set_own(regs, base + dest, lst);
            break;
        }

        /*
         * LEB128 append (signed).  Encodes value as signed LEB128 and appends.
         */
        case OP_BYTES_APPEND_SLEB128: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_BYTES(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *v = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(v))) {
                vm_err = MENAI_ERR_VALUE_NOT_INTEGER;
                goto error;
            }

            long val;
            if (MENAI_UNLIKELY(menai_integer_to_long(v, &val) < 0)) {
                vm_err = MENAI_ERR_OVERFLOW;
                goto error;
            }
            uint8_t buf[10];
            int nbytes = 0;
            int more = 1;
            while (more) {
                uint8_t byte = (uint8_t)(val & 0x7F);
                val >>= 7;
                if ((val == 0 && !(byte & 0x40)) || (val == -1 && (byte & 0x40))) {
                    more = 0;
                } else {
                    byte |= 0x80;
                }
                buf[nbytes++] = byte;
            }

            MenaiValue *result = b;
            menai_retain(result);
            for (int i = 0; i < nbytes; i++) {
                MenaiValue *next = menai_bytes_append_u8(result, buf[i]);
                menai_release(result);
                if (next == NULL) {
                    goto error;
                }
                result = next;
            }
            menai_reg_set_own(regs, base + dest, result);
            break;
        }

        case OP_LIST_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            bool_store(regs, base + dest, IS_MENAI_LIST(regs[base + src0]));
            break;
        }

        case OP_LIST_EQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int eq = menai_value_equal(a, b);
            bool_store(regs, base + dest, eq);
            break;
        }

        case OP_LIST_NEQ_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int eq = menai_value_equal(a, b);
            bool_store(regs, base + dest, !eq);
            break;
        }

        case OP_LIST_NULL_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int is_null = (((MenaiList *)a)->length == 0);
            bool_store(regs, base + dest, is_null);
            break;
        }

        case OP_LIST_LENGTH: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            ssize_t n = ((MenaiList *)a)->length;
            MenaiValue *_r = ssize_t_to_menai_integer(n);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_LIST_FIRST: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiList *lst_f = (MenaiList *)a;
            if (lst_f->length == 0) {
                vm_err = MENAI_ERR_EMPTY_LIST;
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, lst_f->elements[0]);
            break;
        }

        case OP_LIST_REST: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            if (((MenaiList *)a)->length == 0) {
                vm_err = MENAI_ERR_EMPTY_LIST;
                goto error;
            }

            MenaiValue *r = menai_list_rest(a);
            if (r == NULL) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_LAST: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiList *lst_l = (MenaiList *)a;
            ssize_t n = lst_l->length;
            if (n == 0) {
                vm_err = MENAI_ERR_EMPTY_LIST;
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, lst_l->elements[n - 1]);
            break;
        }

        case OP_LIST_REF: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_INDEX_NOT_INTEGER;
                goto error;
            }

            MenaiList *lst_ref = (MenaiList *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            long idx_l;
            if (!ib->is_big) {
                idx_l = ib->small;
            } else {
                vm_err = menai_bigint_to_long(&ib->big, &idx_l);
                if (vm_err < 0) {
                    goto error;
                }
            }

            ssize_t idx = (ssize_t)idx_l;
            ssize_t n = lst_ref->length;
            if (idx < 0 || idx >= n) {
                vm_err = MENAI_ERR_INDEX_OUT_OF_RANGE;
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, lst_ref->elements[idx]);
            break;
        }

        case OP_LIST_PREPEND: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiList *lst_pre = (MenaiList *)a;
            ssize_t n = lst_pre->length;
            MenaiValue *r = menai_list_alloc(n + 1);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *item = regs[base + src1];
            MenaiValue **pre_arr = menai_list_elements(r);
            pre_arr[0] = item;
            menai_retain(item);
            for (ssize_t i = 0; i < n; i++) {
                pre_arr[i + 1] = lst_pre->elements[i];
                menai_retain(pre_arr[i + 1]);
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_APPEND: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiList *lst_app = (MenaiList *)a;
            ssize_t n = lst_app->length;
            MenaiValue *r = menai_list_alloc(n + 1);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **app_arr = menai_list_elements(r);
            for (ssize_t i = 0; i < n; i++) {
                app_arr[i] = lst_app->elements[i];
                menai_retain(app_arr[i]);
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *item = regs[base + src1];
            app_arr[n] = item;
            menai_retain(item);
            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_REVERSE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiList *lst_rev = (MenaiList *)a;
            ssize_t n = lst_rev->length;
            MenaiValue *r = menai_list_alloc(n);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **rev_arr = menai_list_elements(r);
            for (ssize_t i = 0; i < n; i++) {
                rev_arr[i] = lst_rev->elements[n - 1 - i];
                menai_retain(rev_arr[i]);
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_CONCAT: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiList *lst_ca = (MenaiList *)a;
            MenaiList *lst_cb = (MenaiList *)b;
            ssize_t na = lst_ca->length, nb = lst_cb->length;
            ssize_t nc = na + nb;
            MenaiValue *r = menai_list_alloc(nc);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **cat_arr = menai_list_elements(r);
            for (ssize_t i = 0; i < na; i++) {
                cat_arr[i] = lst_ca->elements[i];
                menai_retain(cat_arr[i]);
            }

            for (ssize_t i = 0; i < nb; i++) {
                cat_arr[na + i] = lst_cb->elements[i];
                menai_retain(cat_arr[na + i]);
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_MEMBER_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *item = regs[base + src1];
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *item = regs[base + src1];
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
                MenaiValue *_r = ssize_t_to_menai_integer(found);
                if (_r == NULL) {
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, _r);
            }

            break;
        }

        case OP_LIST_SLICE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(b))) {
                vm_err = MENAI_ERR_SLICE_INDICES_NOT_INTEGER;
                goto error;
            }

            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *c = regs[base + src2];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(c))) {
                vm_err = MENAI_ERR_SLICE_INDICES_NOT_INTEGER;
                goto error;
            }

            MenaiList *lst_sl = (MenaiList *)a;
            MenaiInteger *ib = (MenaiInteger *)b;
            MenaiInteger *ic = (MenaiInteger *)c;
            long start_l, end_l;
            if (!ib->is_big) {
                start_l = ib->small;
            } else {
                vm_err = menai_bigint_to_long(&ib->big, &start_l);
                if (vm_err < 0) {
                    goto error;
                }
            }

            if (!ic->is_big) {
                end_l = ic->small;
            } else {
                vm_err = menai_bigint_to_long(&ic->big, &end_l);
                if (vm_err < 0) {
                    goto error;
                }
            }

            ssize_t start = (ssize_t)start_l, end = (ssize_t)end_l;
            ssize_t n = lst_sl->length;
            if (start < 0) {
                vm_err = MENAI_ERR_NEGATIVE_SLICE_INDEX;
                goto error;
            }

            if (end < 0) {
                vm_err = MENAI_ERR_NEGATIVE_SLICE_INDEX;
                goto error;
            }

            if (start > n) {
                vm_err = MENAI_ERR_SLICE_START_OUT_OF_RANGE;
                goto error;
            }

            if (end > n) {
                vm_err = MENAI_ERR_SLICE_END_OUT_OF_RANGE;
                goto error;
            }

            if (start > end) {
                vm_err = MENAI_ERR_SLICE_START_AFTER_END;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiList *lst_rm = (MenaiList *)a;
            ssize_t n = lst_rm->length;

            MenaiValue *r = menai_list_alloc(n);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *item = regs[base + src1];
            MenaiValue **rm_arr = menai_list_elements(r);
            ssize_t j = 0;
            for (ssize_t i = 0; i < n; i++) {
                MenaiValue *e = lst_rm->elements[i];
                int eq = menai_value_equal(e, item);
                if (!eq) {
                    menai_retain(e);
                    rm_arr[j++] = e;
                }
            }

            ((MenaiList *)r)->length = j;
            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_LIST_TO_STRING: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRING(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiList *lst_ts = (MenaiList *)a;
            ssize_t n = lst_ts->length;
            /* Validate all elements are strings first. */
            for (ssize_t i = 0; i < n; i++) {
                if (MENAI_UNLIKELY(!IS_MENAI_STRING(lst_ts->elements[i]))) {
                    vm_err = MENAI_ERR_LIST_TO_STRING_NOT_STRINGS;
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
            if (MENAI_UNLIKELY(!IS_MENAI_LIST(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiList *lst = (MenaiList *)a;
            ssize_t n = lst->length;
            MenaiValue *r = menai_set_alloc(n);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **nelems = ((MenaiSet *)r)->elements;
            hash_t *nhashes = ((MenaiSet *)r)->hashes;
            MenaiHashTable lts_seen;
            int lts_err = 0;
            if (n > 0 && (vm_err = menai_ht_init(&lts_seen, n)) < 0) {
                menai_release(r);
                goto error;
            }

            ssize_t out = 0;
            for (ssize_t i = 0; i < n && !lts_err; i++) {
                MenaiValue *elem = lst->elements[i];
                hash_t h = menai_value_hash(elem);
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

                menai_release(r);
                vm_err = MENAI_ERR_UNHASHABLE_KEY;
                goto error;
            }

            ((MenaiSet *)r)->length = out;
            vm_err = menai_ht_build(&((MenaiSet *)r)->ht, nelems, nhashes, out);
            if (vm_err < 0) {
                vm_err = MENAI_ERR_NOMEM;
                menai_release(r);
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = ssize_t_to_menai_integer(((MenaiDict *)a)->length);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_DICT_KEYS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiDict *d = (MenaiDict *)a;
            ssize_t n = d->length;
            MenaiValue *r = menai_list_alloc(n);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **dk_arr = menai_list_elements(r);
            for (ssize_t i = 0; i < n; i++) {
                menai_retain(d->keys[i]);
                dk_arr[i] = d->keys[i];
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_VALUES: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiDict *d = (MenaiDict *)a;
            ssize_t n = d->length;
            MenaiValue *r = menai_list_alloc(n);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **dv_arr = menai_list_elements(r);
            for (ssize_t i = 0; i < n; i++) {
                menai_retain(d->values[i]);
                dv_arr[i] = d->values[i];
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_DICT_HAS_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *key = regs[base + src1];
            MenaiDict *d = (MenaiDict *)a;
            hash_t h = menai_value_hash(key);
            if (h == -1) {
                vm_err = MENAI_ERR_UNHASHABLE_KEY;
                goto error;
            }

            int has = (menai_ht_lookup(&d->ht, key, h) >= 0);
            bool_store(regs, base + dest, has);
            break;
        }

        case OP_DICT_GET: {
            /* src0=dict, src1=key, src2=default */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *key = regs[base + src1];
            MenaiDict *d = (MenaiDict *)a;
            hash_t h = menai_value_hash(key);
            if (h == -1) {
                vm_err = MENAI_ERR_UNHASHABLE_KEY;
                goto error;
            }

            ssize_t idx = menai_ht_lookup(&d->ht, key, h);
            if (idx == -2) {
                goto error;
            }

            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *def = regs[base + src2];
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *key = regs[base + src1];
            MenaiDict *d = (MenaiDict *)a;
            hash_t h = menai_value_hash(key);
            if (h == -1) {
                vm_err = MENAI_ERR_UNHASHABLE_KEY;
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
            hash_t *nhashes = (hash_t *)malloc(new_n * sizeof(hash_t));
            if (!nkeys || !nvals || !nhashes) {
                free(nkeys);
                free(nvals);
                free(nhashes);
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *val = regs[base + src2];
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *key = regs[base + src1];
            MenaiDict *d = (MenaiDict *)a;
            hash_t h = menai_value_hash(key);
            if (h == -1) {
                vm_err = MENAI_ERR_UNHASHABLE_KEY;
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
            hash_t *nhashes = new_n > 0 ? (hash_t *)malloc(new_n * sizeof(hash_t)) : NULL;
            if (new_n > 0 && (!nkeys || !nvals || !nhashes)) {
                free(nkeys);
                free(nvals);
                free(nhashes);
                vm_err = MENAI_ERR_NOMEM;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_DICT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiDict *da = (MenaiDict *)a;
            MenaiDict *db = (MenaiDict *)b;
            ssize_t na = da->length, nb = db->length;
            ssize_t cap = na + nb;
            MenaiValue **nkeys = cap > 0 ? (MenaiValue **)malloc(cap * sizeof(MenaiValue *)) : NULL;
            MenaiValue **nvals = cap > 0 ? (MenaiValue **)malloc(cap * sizeof(MenaiValue *)) : NULL;
            hash_t *nhashes = cap > 0 ? (hash_t *)malloc(cap * sizeof(hash_t)) : NULL;
            if (cap > 0 && (!nkeys || !nvals || !nhashes)) {
                free(nkeys);
                free(nvals);
                free(nhashes);
                vm_err = MENAI_ERR_NOMEM;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            if (MENAI_UNLIKELY(!IS_MENAI_SET(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiValue *_r = ssize_t_to_menai_integer(((MenaiSet *)a)->length);
            if (_r == NULL) {
                goto error;
            }

            menai_reg_set_own(regs, base + dest, _r);
            break;
        }

        case OP_SET_MEMBER_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *item = regs[base + src1];
            MenaiSet *s = (MenaiSet *)a;
            hash_t h = menai_value_hash(item);
            if (h == -1) {
                vm_err = MENAI_ERR_UNHASHABLE_KEY;
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
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *item = regs[base + src1];
            MenaiSet *s = (MenaiSet *)a;
            hash_t h = menai_value_hash(item);
            if (h == -1) {
                vm_err = MENAI_ERR_UNHASHABLE_KEY;
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
                MenaiValue *r = menai_set_alloc(n + 1);
                if (!r) {
                    vm_err = MENAI_ERR_NOMEM;
                    goto error;
                }

                MenaiValue **nelems = ((MenaiSet *)r)->elements;
                hash_t *nhashes = ((MenaiSet *)r)->hashes;
                for (ssize_t i = 0; i < n; i++) {
                    menai_retain(s->elements[i]);
                    nelems[i] = s->elements[i];
                    nhashes[i] = s->hashes[i];
                }

                menai_retain(item);
                nelems[n] = item;
                nhashes[n] = h;
                ((MenaiSet *)r)->length = n + 1;
                vm_err = menai_ht_build(&((MenaiSet *)r)->ht, nelems, nhashes, n + 1);
                if (vm_err < 0) {
                    vm_err = MENAI_ERR_NOMEM;
                    menai_release(r);
                    goto error;
                }

                menai_reg_set_own(regs, base + dest, r);
            }

            break;
        }

        case OP_SET_REMOVE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *item = regs[base + src1];
            MenaiSet *s = (MenaiSet *)a;
            hash_t h = menai_value_hash(item);
            if (h == -1) {
                vm_err = MENAI_ERR_UNHASHABLE_KEY;
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
            MenaiValue *r = menai_set_alloc(new_n);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **nelems = ((MenaiSet *)r)->elements;
            hash_t *nhashes = ((MenaiSet *)r)->hashes;
            for (ssize_t i = 0, j = 0; i < n; i++) {
                if (i == remove_idx) {
                    continue;
                }

                menai_retain(s->elements[i]);
                nelems[j] = s->elements[i];
                nhashes[j] = s->hashes[i];
                j++;
            }

            ((MenaiSet *)r)->length = new_n;
            vm_err = menai_ht_build(&((MenaiSet *)r)->ht, nelems, nhashes, new_n);
            if (vm_err < 0) {
                vm_err = MENAI_ERR_NOMEM;
                menai_release(r);
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_UNION: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiSet *sa = (MenaiSet *)a;
            MenaiSet *sb = (MenaiSet *)b;
            ssize_t na = sa->length, nb = sb->length;
            ssize_t cap = na + nb;
            MenaiValue *r = menai_set_alloc(cap);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **nelems = ((MenaiSet *)r)->elements;
            hash_t *nhashes = ((MenaiSet *)r)->hashes;
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

                    menai_release(r);
                    goto error;
                }

                if (in_a < 0) {
                    menai_retain(sb->elements[i]);
                    nelems[out] = sb->elements[i];
                    nhashes[out] = sb->hashes[i];
                    out++;
                }
            }

            ((MenaiSet *)r)->length = out;
            vm_err = menai_ht_build(&((MenaiSet *)r)->ht, nelems, nhashes, out);
            if (vm_err < 0) {
                vm_err = MENAI_ERR_NOMEM;
                menai_release(r);
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_INTERSECTION: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiSet *sa = (MenaiSet *)a;
            MenaiSet *sb = (MenaiSet *)b;
            ssize_t na = sa->length;
            MenaiValue *r = menai_set_alloc(na);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **nelems = ((MenaiSet *)r)->elements;
            hash_t *nhashes = ((MenaiSet *)r)->hashes;
            ssize_t out = 0;
            for (ssize_t i = 0; i < na; i++) {
                ssize_t in_b = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (in_b == -2) {
                    for (ssize_t k = 0; k < out; k++) {
                        menai_release(nelems[k]);
                    }

                    menai_release(r);
                    goto error;
                }

                if (in_b >= 0) {
                    menai_retain(sa->elements[i]);
                    nelems[out] = sa->elements[i];
                    nhashes[out] = sa->hashes[i];
                    out++;
                }
            }

            ((MenaiSet *)r)->length = out;
            vm_err = menai_ht_build(&((MenaiSet *)r)->ht, nelems, nhashes, out);
            if (vm_err < 0) {
                vm_err = MENAI_ERR_NOMEM;
                menai_release(r);
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_DIFFERENCE: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiSet *sa = (MenaiSet *)a;
            MenaiSet *sb = (MenaiSet *)b;
            ssize_t na = sa->length;
            MenaiValue *r = menai_set_alloc(na);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **nelems = ((MenaiSet *)r)->elements;
            hash_t *nhashes = ((MenaiSet *)r)->hashes;
            ssize_t out = 0;
            for (ssize_t i = 0; i < na; i++) {
                ssize_t in_b = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
                if (in_b == -2) {
                    for (ssize_t k = 0; k < out; k++) {
                        menai_release(nelems[k]);
                    }

                    menai_release(r);
                    goto error;
                }

                if (in_b < 0) {
                    menai_retain(sa->elements[i]); nelems[out] = sa->elements[i];
                    nhashes[out] = sa->hashes[i];
                    out++;
                }
            }

            ((MenaiSet *)r)->length = out;
            vm_err = menai_ht_build(&((MenaiSet *)r)->ht, nelems, nhashes, out);
            if (vm_err < 0) {
                vm_err = MENAI_ERR_NOMEM;
                menai_release(r);
                goto error;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_SET_SUBSET_P: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *a = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *b = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_SET(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            if (MENAI_UNLIKELY(!IS_MENAI_SET(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiSet *s = (MenaiSet *)a;
            ssize_t set_n = s->length;
            MenaiValue *r = menai_list_alloc(set_n);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **stl_arr = menai_list_elements(r);
            for (ssize_t i = 0; i < set_n; i++) {
                menai_retain(s->elements[i]);
                stl_arr[i] = s->elements[i];
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_RANGE: {
            /* src0=start, src1=end, src2=step — all integers */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *ra = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(ra))) {
                vm_err = MENAI_ERR_RANGE_NOT_INTEGER;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *rb = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(rb))) {
                vm_err = MENAI_ERR_RANGE_NOT_INTEGER;
                goto error;
            }

            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *rc = regs[base + src2];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(rc))) {
                vm_err = MENAI_ERR_RANGE_NOT_INTEGER;
                goto error;
            }

            MenaiInteger *ia = (MenaiInteger *)ra;
            MenaiInteger *ib = (MenaiInteger *)rb;
            MenaiInteger *ic = (MenaiInteger *)rc;
            long start, end, step;
            if (!ia->is_big) {
                start = ia->small;
            } else {
                vm_err = menai_bigint_to_long(&ia->big, &start);
                if (vm_err < 0) {
                    goto error;
                }
            }

            if (!ib->is_big) {
                end = ib->small;
            } else {
                vm_err = menai_bigint_to_long(&ib->big, &end);
                if (vm_err < 0) {
                    goto error;
                }
            }

            if (!ic->is_big) {
                step = ic->small;
            } else {
                vm_err = menai_bigint_to_long(&ic->big, &step);
                if (vm_err < 0) {
                    goto error;
                }
            }

            if (step == 0) {
                vm_err = MENAI_ERR_RANGE_ZERO_STEP;
                goto error;
            }

            /* Compute length */
            ssize_t n = 0;
            if (step > 0 && end > start) {
                n = (end - start + step - 1) / step;
            } else if (step < 0 && end < start) {
                n = (start - end - step - 1) / (-step);
            }

            MenaiValue *r = menai_list_alloc(n);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **rng_arr = menai_list_elements(r);
            long val = start;
            for (ssize_t i = 0; i < n; i++) {
                MenaiValue *mi = long_to_menai_integer(val);
                if (mi == NULL) {
                    for (ssize_t k = 0; k < i; k++) {
                        menai_release(rng_arr[k]);
                    }

                    menai_release(r);
                    goto error;
                }

                rng_arr[i] = mi;
                val += step;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_MAKE_LIST: {
            /*
             * MAKE_LIST src0, src1:
             * src0 = base slot of outgoing zone (absolute slot index).
             * src1 = element count.
             * Elements are in slots src0..src0+n-1.
             */
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int n = src1;
            MenaiValue *r = menai_list_alloc(n);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue **lst_arr = menai_list_elements(r);
            for (int i = 0; i < n; i++) {
                lst_arr[i] = regs[base + src0 + i];
                menai_retain(lst_arr[i]);
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_MAKE_SET: {
            /*
             * MAKE_SET src0, src1:
             * src0 = base slot of outgoing zone (absolute slot index).
             * src1 = element count.
             * Elements are in slots src0..src0+n-1.
             */
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int n = src1;
            MenaiValue *r = menai_set_alloc(n);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiSet *s = (MenaiSet *)r;
            for (int i = 0; i < n; i++) {
                MenaiValue *elem = regs[base + src0 + i];
                hash_t h = menai_value_hash(elem);
                if (h == -1) {
                    vm_err = MENAI_ERR_UNHASHABLE_KEY;
                    menai_release(r);
                    goto error;
                }

                menai_retain(elem);
                s->elements[i] = elem;
                s->hashes[i] = h;
            }

            s->length = n;
            if (n > 0) {
                vm_err = menai_ht_build(&s->ht, s->elements, s->hashes, n);
                if (vm_err < 0) {
                    vm_err = MENAI_ERR_NOMEM;
                    menai_release(r);
                    goto error;
                }
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        case OP_MAKE_DICT: {
            /*
             * MAKE_DICT src0, src1:
             * src0 = base slot of outgoing zone (absolute slot index).
             * src1 = pair count.
             * Pairs are interleaved as k0, v0, k1, v1, ... in slots src0..src0+n*2-1.
             */
            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            int n = src1;
            MenaiValue **keys = (MenaiValue **)malloc((size_t)n * sizeof(MenaiValue *));
            MenaiValue **values = (MenaiValue **)malloc((size_t)n * sizeof(MenaiValue *));
            hash_t *hashes = (hash_t *)malloc((size_t)n * sizeof(hash_t));
            if (!keys || !values || !hashes) {
                free(keys);
                free(values);
                free(hashes);
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            for (int i = 0; i < n; i++) {
                MenaiValue *k = regs[base + src0 + i * 2];
                MenaiValue *v = regs[base + src0 + i * 2 + 1];
                hash_t h = menai_value_hash(k);
                if (h == -1) {
                    vm_err = MENAI_ERR_UNHASHABLE_KEY;
                    free(keys);
                    free(values);
                    free(hashes);
                    goto error;
                }

                menai_retain(k);
                menai_retain(v);
                keys[i] = k;
                values[i] = v;
                hashes[i] = h;
            }

            MenaiValue *r = menai_dict_from_arrays_steal(keys, values, hashes, n);
            if (!r) {
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
            MenaiValue *struct_type = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCTTYPE(struct_type))) {
                vm_err = MENAI_ERR_STRUCT_FIRST_NOT_TYPE;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
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
            MenaiValue *stype = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCTTYPE(stype))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *val = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCT(val))) {
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
            MenaiValue *val = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCT(val))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *field_sym = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_SYMBOL(field_sym))) {
                vm_err = MENAI_ERR_NOT_SYMBOL;
                goto error;
            }

            MenaiValue *stype = ((MenaiStruct *)val)->struct_type;
            MenaiValue *field_name = menai_symbol_name(field_sym);
            int fi = menai_struct_field_index((MenaiStructType *)stype, field_name);
            if (fi < 0) {
                vm_err = MENAI_ERR_STRUCT_FIELD_NOT_FOUND;
                goto error;
            }

            MenaiValue *fv = ((MenaiStruct *)val)->items[fi];
            menai_reg_set_borrow(regs, base + dest, fv);
            break;
        }

        case OP_STRUCT_GET_IMM: {
            /* src1 holds a MenaiInteger field index */
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *val = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCT(val))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *fidx = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(fidx))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *fi_io = (MenaiInteger *)fidx;
            long fi_l;
            if (!fi_io->is_big) {
                fi_l = fi_io->small;
            } else {
                vm_err = menai_bigint_to_long(&fi_io->big, &fi_l);
                if (vm_err < 0) {
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
            MenaiValue *val = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCT(val))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *field_sym = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_SYMBOL(field_sym))) {
                vm_err = MENAI_ERR_NOT_SYMBOL;
                goto error;
            }

            MenaiValue *stype = ((MenaiStruct *)val)->struct_type;
            MenaiValue *field_name = menai_symbol_name(field_sym);
            int fi = menai_struct_field_index((MenaiStructType *)stype, field_name);
            if (fi < 0) {
                vm_err = MENAI_ERR_STRUCT_FIELD_NOT_FOUND;
                goto error;
            }

            ssize_t nf = ((MenaiStruct *)val)->nfields;
            MenaiValue **tmp = (MenaiValue **)malloc(nf * sizeof(MenaiValue *));
            if (!tmp) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *new_val = regs[base + src2];
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
            MenaiValue *val = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCT(val))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            int src1 = (int)((word >> SRC1_SHIFT) & FIELD_MASK);
            MenaiValue *fidx = regs[base + src1];
            if (MENAI_UNLIKELY(!IS_MENAI_INTEGER(fidx))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiInteger *fi_io = (MenaiInteger *)fidx;
            long fi_l;
            if (!fi_io->is_big) {
                fi_l = fi_io->small;
            } else {
                vm_err = menai_bigint_to_long(&fi_io->big, &fi_l);
                if (vm_err < 0) {
                    goto error;
                }
            }

            ssize_t fi = (ssize_t)fi_l;
            MenaiValue *stype = ((MenaiStruct *)val)->struct_type;
            ssize_t nf = ((MenaiStruct *)val)->nfields;
            MenaiValue **tmp = (MenaiValue **)malloc(nf * sizeof(MenaiValue *));
            if (!tmp) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            int src2 = (int)(word & FIELD_MASK);
            MenaiValue *new_val = regs[base + src2];
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
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            if (MENAI_UNLIKELY(!IS_MENAI_STRUCT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCT(a))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            if (MENAI_UNLIKELY(!IS_MENAI_STRUCT(b))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
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
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCT(val))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, ((MenaiStruct *)val)->struct_type);
            break;
        }

        case OP_STRUCT_TYPE_NAME: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *val = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCTTYPE(val))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            menai_reg_set_borrow(regs, base + dest, ((MenaiStructType *)val)->name);
            break;
        }

        case OP_STRUCT_FIELDS: {
            int src0 = (int)((word >> SRC0_SHIFT) & FIELD_MASK);
            MenaiValue *val = regs[base + src0];
            if (MENAI_UNLIKELY(!IS_MENAI_STRUCTTYPE(val))) {
                vm_err = MENAI_ERR_TYPE_MISMATCH;
                goto error;
            }

            MenaiStructType *st = (MenaiStructType *)val;
            int n = st->nfields;
            MenaiValue *r = menai_list_alloc(n);
            if (!r) {
                vm_err = MENAI_ERR_NOMEM;
                goto error;
            }

            MenaiValue **sf_arr = menai_list_elements(r);
            for (int i = 0; i < n; i++) {
                MenaiValue *sym = menai_symbol_alloc(st->fields[i].name);
                if (sym == NULL) {
                    for (int k = 0; k < i; k++) {
                        menai_release(sf_arr[k]);
                    }

                    menai_release(r);
                    goto error;
                }

                sf_arr[i] = sym;
            }

            menai_reg_set_own(regs, base + dest, r);
            break;
        }

        default:
            vm_err = MENAI_ERR_UNIMPLEMENTED_OPCODE;
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

        out_error->code = vm_err;
        out_error->opcode = cur_opcode;
        out_error->ip = cur_ip;
        out_error->call_depth = frame_depth;
        out_error->user_message = vm_user_message;
        return NULL;
    }
}

/*
 * menai_vm_cancel_flag_alloc / _free / _set — per-instance cancellation flag
 * lifecycle.  Each MenaiVM instance allocates its own flag so that
 * cancelling one evaluation does not affect another.
 */
int *
menai_vm_cancel_flag_alloc(void)
{
    int *flag = (int *)menai_alloc(sizeof(int));
    if (flag) {
        *flag = 0;
    }
    return flag;
}

void
menai_vm_cancel_flag_free(int *flag)
{
    if (flag) {
        menai_free(flag);
    }
}

void
menai_vm_cancel_flag_set(int *flag)
{
    if (flag) {
        _menai_atomic_store((_menai_atomic_int *)flag, 1);
    }
}

/*
 * menai_vm_execute_native — native VM entry point.
 *
 * Executes code with the given cached globals table and optional extra
 * bindings (a native MenaiDict, or NULL).  Returns a new reference to
 * the result, or NULL on error.  On error, *out_error is filled in.
 */
MenaiValue *
menai_vm_execute_native(MenaiCodeObject *code, const GlobalsTable *globals_gt, MenaiValue *extra_bindings, MenaiVMError *out_error, int *cancel_flag)
{
    if (out_error) {
        out_error->code = MENAI_OK;
        out_error->opcode = 0;
        out_error->ip = 0;
        out_error->call_depth = 0;
        out_error->user_message = NULL;
    }

    GlobalsTable globals;
    int gerr = globals_build(&globals, globals_gt);
    if (gerr < 0) {
        if (out_error) {
            out_error->code = gerr;
        }
        return NULL;
    }

    if (extra_bindings != NULL) {
        int merr = globals_merge_extra_native(&globals, extra_bindings);
        if (merr < 0) {
            if (out_error) {
                out_error->code = merr;
            }
            globals_free(&globals);
            return NULL;
        }
    }

    int max_locals = menai_code_object_max_locals(code);
    for (ssize_t i = 0; i < globals.count; i++) {
        MenaiValue *val = globals.entries[i].value;
        if (IS_MENAI_FUNCTION(val)) {
            int n = menai_code_object_max_locals(((MenaiFunction *)val)->bytecode);
            if (n > max_locals) {
                max_locals = n;
            }
        }
    }

    MenaiValue **regs = menai_regs_alloc(
        (size_t)(MAX_FRAME_DEPTH + 1) * max_locals, Menai_NONE);
    if (regs == NULL) {
        if (out_error) {
            out_error->code = MENAI_ERR_NOMEM;
        }
        globals_free(&globals);
        return NULL;
    }

    MenaiValue *result = execute_loop(code, &globals, regs, max_locals, out_error, cancel_flag);

    menai_regs_free(regs, (size_t)(MAX_FRAME_DEPTH + 1) * max_locals);
    globals_free(&globals);

    return result;
}
