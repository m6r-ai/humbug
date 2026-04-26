/*
 * menai_vm_bigint.c — arbitrary-precision integer library for the Menai VM.
 *
 * Representation: sign-magnitude, base 2^32, little-endian digits.
 * Zero: sign=0, length=0, digits=NULL.
 *
 * Only menai_bigint_from_pylong and menai_bigint_to_pylong may call the Python C API.
 */
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <limits.h>

#include "menai_vm_c.h"

/* Internal forward declarations. */
static int _menai_bigint_cmp_mag(const MenaiBigInt *a, const MenaiBigInt *b);
static int _menai_bigint_add_mag(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
static int _menai_bigint_sub_mag(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
static void _menai_bigint_normalize(MenaiBigInt *a);
static int _menai_bigint_divmod_1(const MenaiBigInt *a, uint32_t b, MenaiBigInt *quotient, uint32_t *remainder);
static int _menai_bigint_divmod_mag(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *quotient, MenaiBigInt *remainder);

/*
 * Multiply two 32-bit values and return the 64-bit product split into
 * low and high 32-bit halves.  Avoids __uint128_t for MSVC portability.
 */
static uint64_t
_mul32(uint32_t a, uint32_t b)
{
    return (uint64_t)a * (uint64_t)b;
}

/* Strip leading zero digits and fix sign when length reaches 0. */
static void
_menai_bigint_normalize(MenaiBigInt *a)
{
    while (a->length > 0 && a->digits[a->length - 1] == 0) {
        a->length--;
    }

    if (a->length == 0) {
        if (a->digits != NULL) {
            free(a->digits);
            a->digits = NULL;
        }

        a->sign = 0;
    }
}

/*
 * Compare magnitudes of a and b.
 * Returns -1 if |a| < |b|, 0 if |a| == |b|, 1 if |a| > |b|.
 */
static int
_menai_bigint_cmp_mag(const MenaiBigInt *a, const MenaiBigInt *b)
{
    if (a->length != b->length) {
        return (a->length < b->length) ? -1 : 1;
    }

    for (ssize_t i = a->length - 1; i >= 0; i--) {
        if (a->digits[i] != b->digits[i]) {
            return (a->digits[i] < b->digits[i]) ? -1 : 1;
        }
    }

    return 0;
}

/*
 * result = |a| + |b|.  result may alias a or b.
 */
static int
_menai_bigint_add_mag(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    ssize_t max_len = (a->length > b->length) ? a->length : b->length;
    ssize_t out_len = max_len + 1;
    uint32_t *digits = (uint32_t *)malloc((size_t)out_len * sizeof(uint32_t));
    if (digits == NULL) {
        PyErr_NoMemory();
        return -1;
    }

    uint64_t carry = 0;
    for (ssize_t i = 0; i < out_len; i++) {
        uint64_t da = (i < a->length) ? a->digits[i] : 0;
        uint64_t db = (i < b->length) ? b->digits[i] : 0;
        uint64_t sum = da + db + carry;
        digits[i] = (uint32_t)(sum & 0xFFFFFFFFULL);
        carry = sum >> 32;
    }

    menai_bigint_free(result);
    result->digits = digits;
    result->length = out_len;
    result->sign = 1;
    _menai_bigint_normalize(result);
    return 0;
}

/*
 * result = |a| - |b|, assuming |a| >= |b|.  result may alias a or b.
 */
static int
_menai_bigint_sub_mag(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    /* |a| >= |b| is a precondition. */
    ssize_t out_len = a->length;
    uint32_t *digits = (uint32_t *)malloc((size_t)out_len * sizeof(uint32_t));
    if (digits == NULL) {
        PyErr_NoMemory();
        return -1;
    }

    int64_t borrow = 0;
    for (ssize_t i = 0; i < out_len; i++) {
        int64_t da = (int64_t)a->digits[i];
        int64_t db = (i < b->length) ? (int64_t)b->digits[i] : 0;
        int64_t diff = da - db - borrow;
        if (diff < 0) {
            diff += (int64_t)0x100000000LL;
            borrow = 1;
        } else {
            borrow = 0;
        }

        digits[i] = (uint32_t)diff;
    }

    menai_bigint_free(result);
    result->digits = digits;
    result->length = out_len;
    result->sign = 1;
    _menai_bigint_normalize(result);
    return 0;
}

/*
 * Divide magnitude of a by single digit b, storing quotient in *quotient
 * and remainder digit in *remainder.  b must be non-zero.
 */
static int
_menai_bigint_divmod_1(
    const MenaiBigInt *a, uint32_t b, MenaiBigInt *quotient, uint32_t *remainder)
{
    if (a->length == 0) {
        menai_bigint_free(quotient);
        *remainder = 0;
        return 0;
    }

    uint32_t *qdigits = (uint32_t *)malloc((size_t)a->length * sizeof(uint32_t));
    if (qdigits == NULL) {
        PyErr_NoMemory();
        return -1;
    }

    uint64_t rem = 0;
    for (ssize_t i = a->length - 1; i >= 0; i--) {
        uint64_t cur = (rem << 32) | a->digits[i];
        qdigits[i] = (uint32_t)(cur / b);
        rem = cur % b;
    }

    menai_bigint_free(quotient);
    quotient->digits = qdigits;
    quotient->length = a->length;
    quotient->sign = 1;
    _menai_bigint_normalize(quotient);
    *remainder = (uint32_t)rem;
    return 0;
}

/*
 * Divide magnitude of a by magnitude of b (b->length >= 2) using Knuth
 * Algorithm D.  Stores quotient in *quotient and remainder in *remainder.
 * Neither quotient nor remainder may alias a or b.
 */
static int
_menai_bigint_divmod_mag(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *quotient, MenaiBigInt *remainder)
{
    ssize_t m = a->length;
    ssize_t n = b->length;

    /* If |a| < |b|, quotient = 0, remainder = |a|. */
    if (m < n) {
        menai_bigint_init(quotient);
        if (menai_bigint_copy(a, remainder) < 0) {
            return -1;
        }

        return 0;
    }

    /*
     * Normalize: find shift d such that the leading digit of b is >= 2^31.
     * We shift both a and b left by d bits.
     */
    uint32_t lead = b->digits[n - 1];
    int d = 0;
    while ((lead & 0x80000000U) == 0) {
        lead <<= 1;
        d++;
    }

    /* Allocate shifted copies: un has m+1 digits, vn has n digits. */
    ssize_t un_len = m + 1;
    uint32_t *un = (uint32_t *)malloc((size_t)un_len * sizeof(uint32_t));
    if (un == NULL) {
        PyErr_NoMemory();
        return -1;
    }

    uint32_t *vn = (uint32_t *)calloc((size_t)n, sizeof(uint32_t));
    if (vn == NULL) {
        free(un);
        PyErr_NoMemory();
        return -1;
    }

    /* Shift a left by d bits into un. */
    if (d == 0) {
        for (ssize_t i = 0; i < m; i++) {
            un[i] = a->digits[i];
        }

        un[m] = 0;
    } else {
        uint32_t carry = 0;
        for (ssize_t i = 0; i < m; i++) {
            uint64_t v = ((uint64_t)a->digits[i] << d) | carry;
            un[i] = (uint32_t)(v & 0xFFFFFFFFULL);
            carry = (uint32_t)(v >> 32);
        }

        un[m] = carry;
    }

    /* Shift b left by d bits into vn. */
    if (d == 0) {
        for (ssize_t i = 0; i < n; i++) {
            vn[i] = b->digits[i];
        }
    } else {
        uint32_t carry = 0;
        for (ssize_t i = 0; i < n; i++) {
            uint64_t v = ((uint64_t)b->digits[i] << d) | carry;
            vn[i] = (uint32_t)(v & 0xFFFFFFFFULL);
            carry = (uint32_t)(v >> 32);
        }

        /* carry must be 0 here because we chose d so leading bit is set */
    }

    ssize_t q_len = m - n + 1;
    uint32_t *qdigits = (uint32_t *)malloc((size_t)q_len * sizeof(uint32_t));
    if (qdigits == NULL) {
        free(un);
        free(vn);
        PyErr_NoMemory();
        return -1;
    }

    uint64_t vn1 = vn[n - 1];
    uint64_t vn2 = (n >= 2) ? vn[n - 2] : 0;

    for (ssize_t j = m - n; j >= 0; j--) {
        /* Estimate q_hat = (un[j+n]*B + un[j+n-1]) / vn[n-1]. */
        uint64_t num_hi = un[j + n];
        uint64_t num_lo = un[j + n - 1];
        uint64_t num = (num_hi << 32) | num_lo;
        uint64_t q_hat, r_hat;

        if (num_hi >= vn1) {
            /* Would overflow — clamp to max digit. */
            q_hat = 0xFFFFFFFFULL;
            r_hat = num_lo + vn1;
        } else {
            q_hat = num / vn1;
            r_hat = num % vn1;
        }

        /* Refine: while q_hat*vn[n-2] > B*r_hat + un[j+n-2], decrement. */
        while (r_hat <= 0xFFFFFFFFULL) {
            /*
             * q_hat <= 0xFFFFFFFF and vn2 <= 0xFFFFFFFF so the product
             * fits in 64 bits — no overflow check needed.
             */
            uint64_t rhs = (r_hat << 32) | ((j + n - 2 >= 0) ? un[j + n - 2] : 0);
            uint64_t lhs = q_hat * vn2;
            if (lhs <= rhs) {
                break;
            }

            q_hat--;
            r_hat += vn1;
        }

        /* Multiply and subtract: un[j..j+n] -= q_hat * vn[0..n-1]. */
        int64_t borrow = 0;
        for (ssize_t i = 0; i < n; i++) {
            uint64_t prod = q_hat * (uint64_t)vn[i];
            int64_t sub = (int64_t)un[j + i] - (int64_t)(prod & 0xFFFFFFFFULL) - borrow;
            un[j + i] = (uint32_t)(sub & 0xFFFFFFFFLL);
            borrow = (int64_t)(prod >> 32) - (sub >> 32);
        }

        int64_t sub = (int64_t)un[j + n] - borrow;
        un[j + n] = (uint32_t)(sub & 0xFFFFFFFFLL);

        qdigits[j] = (uint32_t)q_hat;

        /* If we subtracted too much, add back. */
        if (sub < 0) {
            qdigits[j]--;
            uint64_t carry = 0;
            for (ssize_t i = 0; i < n; i++) {
                uint64_t s = (uint64_t)un[j + i] + (uint64_t)vn[i] + carry;
                un[j + i] = (uint32_t)(s & 0xFFFFFFFFULL);
                carry = s >> 32;
            }

            un[j + n] = (uint32_t)((uint64_t)un[j + n] + carry);
        }
    }

    /* Build quotient. */
    quotient->digits = qdigits;
    quotient->length = q_len;
    quotient->sign = 1;
    _menai_bigint_normalize(quotient);

    /* Unnormalize remainder: shift un right by d bits. */
    uint32_t *rdigits = (uint32_t *)malloc((size_t)n * sizeof(uint32_t));
    if (rdigits == NULL) {
        free(un);
        free(vn);
        PyErr_NoMemory();
        return -1;
    }

    if (d == 0) {
        for (ssize_t i = 0; i < n; i++) {
            rdigits[i] = un[i];
        }
    } else {
        uint32_t carry = 0;
        for (ssize_t i = n - 1; i >= 0; i--) {
            uint64_t v = ((uint64_t)carry << 32) | un[i];
            rdigits[i] = (uint32_t)(v >> d);
            carry = un[i] & ((1U << d) - 1U);
        }
    }

    remainder->digits = rdigits;
    remainder->length = n;
    remainder->sign = 1;
    _menai_bigint_normalize(remainder);

    free(un);
    free(vn);
    return 0;
}

/* Free the digit array and reset to zero. */
void
menai_bigint_free(MenaiBigInt *a)
{
    if (a->digits != NULL) {
        free(a->digits);
        a->digits = NULL;
    }

    a->length = 0;
    a->sign = 0;
}

/* Copy src into dst. */
int
menai_bigint_copy(const MenaiBigInt *src, MenaiBigInt *dst)
{
    if (src->length == 0) {
        menai_bigint_free(dst);
        return 0;
    }

    uint32_t *digits = (uint32_t *)malloc((size_t)src->length * sizeof(uint32_t));
    if (digits == NULL) {
        PyErr_NoMemory();
        return -1;
    }

    memcpy(digits, src->digits, (size_t)src->length * sizeof(uint32_t));
    menai_bigint_free(dst);
    dst->digits = digits;
    dst->length = src->length;
    dst->sign = src->sign;
    return 0;
}

/* Set a to the value of v. */
int
menai_bigint_from_long(long v, MenaiBigInt *a)
{
    menai_bigint_free(a);
    if (v == 0) {
        return 0;
    }

    int sign;
    unsigned long mag;
    if (v < 0) {
        sign = -1;
        /* Avoid UB for LONG_MIN: cast to unsigned before negating. */
        mag = (unsigned long)(-(v + 1)) + 1UL;
    } else {
        sign = 1;
        mag = (unsigned long)v;
    }

    /* Determine how many 32-bit digits we need. */
    ssize_t len;
    if (sizeof(unsigned long) <= 4 || mag <= 0xFFFFFFFFUL) {
        len = 1;
    } else {
        len = 2;
    }

    uint32_t *digits = (uint32_t *)malloc((size_t)len * sizeof(uint32_t));
    if (digits == NULL) {
        PyErr_NoMemory();
        return -1;
    }

    digits[0] = (uint32_t)(mag & 0xFFFFFFFFUL);
    if (len == 2) {
        digits[1] = (uint32_t)((uint64_t)mag >> 32);
    }

    a->digits = digits;
    a->length = len;
    a->sign = sign;
    _menai_bigint_normalize(a);
    return 0;
}

/* Set a to the value of the CPython integer object obj. */
int
menai_bigint_from_pylong(PyObject *obj, MenaiBigInt *a)
{
    if (!PyLong_Check(obj)) {
        PyErr_SetString(PyExc_TypeError, "expected int");
        return -1;
    }

    /* Try small value first. */
    int overflow = 0;
    long v = PyLong_AsLongAndOverflow(obj, &overflow);
    if (!overflow) {
        if (v == -1 && PyErr_Occurred()) {
            return -1;
        }

        return menai_bigint_from_long(v, a);
    }

    /* Large value: use _PyLong_AsByteArray. */
    int sign = 0;
#if PY_VERSION_HEX >= 0x030E00A1
    PyLong_GetSign(obj, &sign);
#else
    sign = _PyLong_Sign(obj);
#endif

    int is_neg = (sign < 0);

    /* Get number of bits. */
    size_t nbits = (size_t)_PyLong_NumBits(obj);
    if (nbits == (size_t)-1 && PyErr_Occurred()) {
        return -1;
    }

    /*
     * For negative numbers, the two's complement representation may need one
     * extra bit (sign bit), so we add 1 to nbits before computing nbytes.
     * This ensures _PyLong_AsByteArray never fails due to insufficient space.
     */
    size_t nbytes = (nbits + (is_neg ? 8 : 7)) / 8;
    if (nbytes == 0) {
        nbytes = 1;
    }

    unsigned char *buf = (unsigned char *)malloc(nbytes);
    if (buf == NULL) {
        PyErr_NoMemory();
        return -1;
    }

    /* Extract as little-endian two's complement bytes. */
#if PY_VERSION_HEX >= 0x030E00A1
    int bytearray_ret = _PyLong_AsByteArray((PyLongObject *)obj, buf, nbytes, 1, 1, 1);
#else
    int bytearray_ret = _PyLong_AsByteArray((PyLongObject *)obj, buf, nbytes, 1, 1);
#endif
    if (bytearray_ret < 0) {
        free(buf);
        return -1;
    }

    /* If negative, negate the two's complement to get magnitude. */
    if (is_neg) {
        /* Flip bits and add 1. */
        int carry = 1;
        for (size_t i = 0; i < nbytes; i++) {
            int val = (~buf[i] & 0xFF) + carry;
            buf[i] = (unsigned char)(val & 0xFF);
            carry = val >> 8;
        }
    }

    /* Pack bytes into 32-bit digits (little-endian). */
    ssize_t ndigits = (ssize_t)((nbytes + 3) / 4);
    uint32_t *digits = (uint32_t *)malloc((size_t)ndigits * sizeof(uint32_t));
    if (digits == NULL) {
        free(buf);
        PyErr_NoMemory();
        return -1;
    }

    for (ssize_t i = 0; i < ndigits; i++) {
        uint32_t d = 0;
        for (int b = 0; b < 4; b++) {
            size_t byte_idx = (size_t)(i * 4 + b);
            if (byte_idx < nbytes) {
                d |= ((uint32_t)buf[byte_idx]) << (b * 8);
            }
        }

        digits[i] = d;
    }

    free(buf);
    menai_bigint_free(a);
    a->digits = digits;
    a->length = ndigits;
    a->sign = is_neg ? -1 : 1;
    _menai_bigint_normalize(a);
    return 0;
}

/* Parse a NUL-terminated string in the given base and store the result in a. */
int
menai_bigint_from_string(const char *s, int base, MenaiBigInt *a)
{
    if (s == NULL || (base != 2 && base != 8 && base != 10 && base != 16)) {
        PyErr_SetString(PyExc_ValueError, "invalid base");
        return -1;
    }

    /* Skip leading whitespace. */
    while (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') {
        s++;
    }

    int sign = 1;
    if (*s == '-') {
        sign = -1;
        s++;
    } else if (*s == '+') {
        s++;
    }

    if (*s == '\0') {
        PyErr_SetString(PyExc_ValueError, "empty integer string");
        return -1;
    }

    /* Validate and convert characters. */
    const char *p = s;
    while (*p != '\0') {
        int digit;
        char c = *p;
        if (c >= '0' && c <= '9') {
            digit = c - '0';
        } else if (c >= 'a' && c <= 'f') {
            digit = c - 'a' + 10;
        } else if (c >= 'A' && c <= 'F') {
            digit = c - 'A' + 10;
        } else {
            PyErr_Format(PyExc_ValueError, "invalid character in integer string: '%c'", c);
            return -1;
        }

        if (digit >= base) {
            PyErr_Format(PyExc_ValueError, "invalid character for base %d: '%c'", base, c);
            return -1;
        }

        p++;
    }

    /* Build the value by Horner's method: acc = acc * base + digit. */
    MenaiBigInt acc;
    menai_bigint_init(&acc);

    MenaiBigInt base_int;
    menai_bigint_init(&base_int);
    if (menai_bigint_from_long((long)base, &base_int) < 0) {
        return -1;
    }

    MenaiBigInt digit_int;
    menai_bigint_init(&digit_int);

    MenaiBigInt tmp;
    menai_bigint_init(&tmp);

    for (; *s != '\0'; s++) {
        int digit;
        char c = *s;
        if (c >= '0' && c <= '9') {
            digit = c - '0';
        } else if (c >= 'a' && c <= 'f') {
            digit = c - 'a' + 10;
        } else {
            digit = c - 'A' + 10;
        }

        /* acc = acc * base */
        if (menai_bigint_mul(&acc, &base_int, &tmp) < 0) {
            goto fail;
        }

        if (menai_bigint_copy(&tmp, &acc) < 0) {
            goto fail;
        }

        /* acc = acc + digit */
        if (menai_bigint_from_long((long)digit, &digit_int) < 0) {
            goto fail;
        }

        if (menai_bigint_add(&acc, &digit_int, &tmp) < 0) {
            goto fail;
        }

        if (menai_bigint_copy(&tmp, &acc) < 0) {
            goto fail;
        }
    }

    menai_bigint_free(&base_int);
    menai_bigint_free(&digit_int);
    menai_bigint_free(&tmp);

    menai_bigint_free(a);
    *a = acc;
    if (a->sign != 0) {
        a->sign = sign;
    }

    return 0;

fail:
    menai_bigint_free(&acc);
    menai_bigint_free(&base_int);
    menai_bigint_free(&digit_int);
    menai_bigint_free(&tmp);
    return -1;
}

/*
 * Parse a UTF-32 codepoint array as an integer in the given base.
 * Since all valid digit characters are ASCII, each codepoint is validated
 * against 0x7F before being cast to char and forwarded to menai_bigint_from_string
 * via a temporary UTF-8 buffer.
 */
int
menai_bigint_from_codepoints(const uint32_t *data, ssize_t len, int base, MenaiBigInt *a)
{
    if (base != 2 && base != 8 && base != 10 && base != 16) {
        PyErr_SetString(PyExc_ValueError, "invalid base");
        return -1;
    }

    /* Allow a leading sign plus all digit characters — all ASCII. */
    char *buf = (char *)malloc((size_t)(len + 1));
    if (!buf) {
        PyErr_NoMemory();
        return -1;
    }

    for (ssize_t i = 0; i < len; i++) {
        if (data[i] > 0x7F) {
            free(buf);
            PyErr_SetString(PyExc_ValueError, "non-ASCII character in integer string");
            return -1;
        }

        buf[i] = (char)data[i];
    }

    buf[len] = '\0';

    int result = menai_bigint_from_string(buf, base, a);
    free(buf);
    return result;
}

/*
 * Convert trunc(v) to a MenaiInt.  v must be finite.
 *
 * Fast path: if trunc(v) fits in a long, delegate to menai_bigint_from_long.
 * Slow path: decompose the magnitude via frexp into 32-bit limbs, then
 * set the sign.  This avoids any Python C API call.
 */
int
menai_bigint_from_double(double v, MenaiBigInt *a)
{
    /* Work with the magnitude; v is already trunc()'d by the caller. */
    double t = v < 0.0 ? -v : v;
    if (!isfinite(t)) {
        PyErr_SetString(PyExc_ValueError, "cannot convert non-finite float to integer");
        return -1;
    }

    /* Fast path: magnitude fits in a non-negative long. */
    if (t <= (double)LONG_MAX) {
        long lv = (long)v;
        return menai_bigint_from_long(lv, a);
    }

    /* Slow path: decompose into 32-bit base-2^32 limbs using ldexp/frexp. */
    int exp;
    double frac = frexp(t, &exp);  /* t == frac * 2^exp, 0.5 <= frac < 1.0 */
    /* Number of 32-bit limbs needed: ceil(exp / 32) */
    ssize_t nlimbs = (exp + 31) / 32;
    uint32_t *digits = (uint32_t *)malloc((size_t)nlimbs * sizeof(uint32_t));
    if (!digits) {
        PyErr_NoMemory();
        return -1;
    }

    for (ssize_t i = nlimbs - 1; i >= 0; i--) {
        frac *= 4294967296.0;  /* frac * 2^32 */
        uint32_t limb = (uint32_t)frac;
        digits[i] = limb;
        frac -= (double)limb;
    }

    menai_bigint_free(a);
    a->digits = digits;
    a->length = nlimbs;
    a->sign = (v < 0.0) ? -1 : 1;
    _menai_bigint_normalize(a);
    return 0;
}

/* Return 1 if the value of a fits in a C long, 0 otherwise. */
int
menai_bigint_fits_long(const MenaiBigInt *a)
{
    if (a->length == 0) {
        return 1;
    }

    if (sizeof(long) == 4) {
        if (a->length > 1) {
            return 0;
        }

        if (a->sign == 1 && a->digits[0] > (uint32_t)0x7FFFFFFFUL) {
            return 0;
        }

        if (a->sign == -1 && a->digits[0] > (uint32_t)0x80000000UL) {
            return 0;
        }

        return 1;
    }

    /* sizeof(long) == 8 */
    if (a->length > 2) {
        return 0;
    }

    uint64_t mag = 0;
    if (a->length >= 1) {
        mag = a->digits[0];
    }

    if (a->length == 2) {
        mag |= ((uint64_t)a->digits[1] << 32);
    }

    if (a->sign == 1 && mag > (uint64_t)0x7FFFFFFFFFFFFFFFULL) {
        return 0;
    }

    if (a->sign == -1 && mag > (uint64_t)0x8000000000000000ULL) {
        return 0;
    }

    return 1;
}

/* Store the value of a in *out as a C long. */
int
menai_bigint_to_long(const MenaiBigInt *a, long *out)
{
    if (!menai_bigint_fits_long(a)) {
        PyErr_SetString(PyExc_OverflowError, "integer too large to convert to C long");
        return -1;
    }

    if (a->length == 0) {
        *out = 0;
        return 0;
    }

    unsigned long mag = a->digits[0];
    if (sizeof(long) == 8 && a->length == 2) {
        mag |= ((uint64_t)a->digits[1] << 32);
    }

    if (a->sign == -1) {
        /*
         * Use unsigned negation to avoid UB for LONG_MIN.
         * 0UL - mag wraps correctly in unsigned arithmetic.
         */
        *out = (long)(0UL - mag);
    } else {
        *out = (long)mag;
    }

    return 0;
}

/* Store the value of a as a double in *out. */
int
menai_bigint_to_double(const MenaiBigInt *a, double *out)
{
    if (a->length == 0) {
        *out = 0.0;
        return 0;
    }

    double result = 0.0;
    double base = 4294967296.0; /* 2^32 */
    double scale = 1.0;

    for (ssize_t i = 0; i < a->length; i++) {
        result += (double)a->digits[i] * scale;
        scale *= base;
    }

    if (!isfinite(result)) {
        PyErr_SetString(PyExc_OverflowError, "integer too large to convert to float");
        return -1;
    }

    *out = (a->sign == -1) ? -result : result;
    return 0;
}

/* Return a new CPython integer object representing the value of a. */
PyObject *
menai_bigint_to_pylong(const MenaiBigInt *a)
{
    if (a->length == 0) {
        return PyLong_FromLong(0);
    }

    /* Pack digits into a byte array (little-endian). */
    size_t nbytes = (size_t)a->length * 4;
    unsigned char *buf = (unsigned char *)malloc(nbytes);
    if (buf == NULL) {
        PyErr_NoMemory();
        return NULL;
    }

    for (ssize_t i = 0; i < a->length; i++) {
        uint32_t d = a->digits[i];
        buf[i * 4 + 0] = (unsigned char)(d & 0xFF);
        buf[i * 4 + 1] = (unsigned char)((d >> 8) & 0xFF);
        buf[i * 4 + 2] = (unsigned char)((d >> 16) & 0xFF);
        buf[i * 4 + 3] = (unsigned char)((d >> 24) & 0xFF);
    }

    PyObject *result = _PyLong_FromByteArray(buf, nbytes, 1, 0);
    free(buf);
    if (result == NULL) {
        return NULL;
    }

    if (a->sign == -1) {
        PyObject *neg = PyNumber_Negative(result);
        Py_DECREF(result);
        return neg;
    }

    return result;
}

/*
 * Convert the integer a to a MenaiString in the given base (2, 8, 10, or 16).
 * Digit characters are ASCII and map 1:1 to codepoints, so the conversion
 * writes directly into the MenaiString's uint32_t data array — no intermediate
 * char buffer or UTF-8 decode step is needed.
 */
MenaiValue *
menai_bigint_to_menai_string(const MenaiBigInt *a, int base)
{
    if (base != 2 && base != 8 && base != 10 && base != 16) {
        PyErr_SetString(PyExc_ValueError, "invalid base");
        return NULL;
    }

    if (a->length == 0) {
        return menai_string_from_codepoint((uint32_t)'0');
    }

    /* Work on a copy so we can do repeated division. */
    MenaiBigInt tmp;
    menai_bigint_init(&tmp);
    if (menai_bigint_copy(a, &tmp) < 0) {
        return NULL;
    }

    tmp.sign = 1; /* work with magnitude */

    /*
     * Upper bound on codepoint count: ceil(bits / log2(base)) + 1 for sign.
     * We allocate a temporary uint32_t scratch buffer, fill it in reverse
     * (least-significant digit first), then allocate the final MenaiString
     * and copy in forward order.
     */
    ssize_t bits = a->length * 32;
    ssize_t max_chars;
    if (base == 2) {
        max_chars = bits + 1;
    } else if (base == 8) {
        max_chars = bits / 3 + 2;
    } else if (base == 16) {
        max_chars = bits / 4 + 2;
    } else {
        /* base 10: log10(2^32) ~ 9.63 per digit */
        max_chars = a->length * 10 + 1;
    }

    uint32_t *buf = (uint32_t *)malloc((size_t)max_chars * sizeof(uint32_t));
    if (buf == NULL) {
        menai_bigint_free(&tmp);
        PyErr_NoMemory();
        return NULL;
    }

    static const uint32_t hex_digits[] = {
        '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'
    };
    ssize_t pos = 0;

    MenaiBigInt quotient;
    menai_bigint_init(&quotient);

    while (tmp.length > 0) {
        uint32_t rem;
        if (_menai_bigint_divmod_1(&tmp, (uint32_t)base, &quotient, &rem) < 0) {
            menai_bigint_free(&tmp);
            menai_bigint_free(&quotient);
            free(buf);
            return NULL;
        }

        menai_bigint_free(&tmp);
        tmp = quotient;
        menai_bigint_init(&quotient);
        buf[pos++] = hex_digits[rem & 0xF];
    }

    menai_bigint_free(&tmp);
    menai_bigint_free(&quotient);

    if (pos == 0) {
        buf[pos++] = (uint32_t)'0';
    }

    if (a->sign == -1) {
        buf[pos++] = (uint32_t)'-';
    }

    /* Reverse the scratch buffer in place, then wrap it in a MenaiString. */
    for (ssize_t i = 0, j = pos - 1; i < j; i++, j--) {
        uint32_t c = buf[i];
        buf[i] = buf[j];
        buf[j] = c;
    }

    MenaiValue *result = menai_string_from_codepoints(buf, pos);
    free(buf);
    return result;
}

/*
 * Compute a hash for a using FNV-1a over the 32-bit digits, then mix in the
 * sign.  Zero always hashes to 0.  The result is never -1 (which is reserved
 * as an error sentinel by convention); -1 is remapped to -2.
 */
Py_hash_t
menai_bigint_hash(const MenaiBigInt *a)
{
    if (a->sign == 0) {
        return 0;
    }

    /* FNV-1a, 64-bit variant. */
    uint64_t h = 14695981039346656037ULL;
    for (ssize_t i = 0; i < a->length; i++) {
        uint32_t d = a->digits[i];
        h ^= (uint64_t)(d & 0xFF);         h *= 1099511628211ULL;
        h ^= (uint64_t)((d >> 8) & 0xFF);  h *= 1099511628211ULL;
        h ^= (uint64_t)((d >> 16) & 0xFF); h *= 1099511628211ULL;
        h ^= (uint64_t)((d >> 24) & 0xFF); h *= 1099511628211ULL;
    }

    if (a->sign == -1) {
        h = ~h;
    }

    Py_hash_t result = (Py_hash_t)h;
    return (result == -1) ? -2 : result;
}

/* result = a + b */
int
menai_bigint_add(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    /* Handle zero operands. */
    if (a->sign == 0) {
        MenaiBigInt tmp;
        menai_bigint_init(&tmp);
        if (menai_bigint_copy(b, &tmp) < 0) {
            return -1;
        }

        menai_bigint_free(result);
        *result = tmp;
        return 0;
    }

    if (b->sign == 0) {
        MenaiBigInt tmp;
        menai_bigint_init(&tmp);
        if (menai_bigint_copy(a, &tmp) < 0) {
            return -1;
        }

        menai_bigint_free(result);
        *result = tmp;
        return 0;
    }

    if (a->sign == b->sign) {
        /* Same sign: add magnitudes, keep sign. */
        int s = a->sign;
        MenaiBigInt tmp;
        menai_bigint_init(&tmp);
        if (_menai_bigint_add_mag(a, b, &tmp) < 0) {
            return -1;
        }

        if (tmp.sign != 0) {
            tmp.sign = s;
        }

        menai_bigint_free(result);
        *result = tmp;
        return 0;
    }

    /* Different signs: subtract smaller magnitude from larger. */
    int cmp = _menai_bigint_cmp_mag(a, b);
    if (cmp == 0) {
        menai_bigint_free(result);
        return 0;
    }

    MenaiBigInt tmp;
    menai_bigint_init(&tmp);
    int res_sign;
    if (cmp > 0) {
        /* |a| > |b|: result has sign of a */
        if (_menai_bigint_sub_mag(a, b, &tmp) < 0) {
            return -1;
        }

        res_sign = a->sign;
    } else {
        /* |b| > |a|: result has sign of b */
        if (_menai_bigint_sub_mag(b, a, &tmp) < 0) {
            return -1;
        }

        res_sign = b->sign;
    }

    if (tmp.sign != 0) {
        tmp.sign = res_sign;
    }

    menai_bigint_free(result);
    *result = tmp;
    return 0;
}

/* result = a - b */
int
menai_bigint_sub(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    /* Negate b and add. */
    MenaiBigInt neg_b;
    menai_bigint_init(&neg_b);
    if (menai_bigint_copy(b, &neg_b) < 0) {
        return -1;
    }

    if (neg_b.sign != 0) {
        neg_b.sign = -neg_b.sign;
    }

    int r = menai_bigint_add(a, &neg_b, result);
    menai_bigint_free(&neg_b);
    return r;
}

/* result = a * b */
int
menai_bigint_mul(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    if (a->sign == 0 || b->sign == 0) {
        menai_bigint_free(result);
        return 0;
    }

    ssize_t out_len = a->length + b->length;
    uint32_t *digits = (uint32_t *)malloc((size_t)out_len * sizeof(uint32_t));
    if (digits == NULL) {
        PyErr_NoMemory();
        return -1;
    }

    memset(digits, 0, (size_t)out_len * sizeof(uint32_t));

    for (ssize_t i = 0; i < a->length; i++) {
        uint64_t carry = 0;
        for (ssize_t j = 0; j < b->length; j++) {
            uint64_t prod = _mul32(a->digits[i], b->digits[j]) + (uint64_t)digits[i + j] + carry;
            digits[i + j] = (uint32_t)(prod & 0xFFFFFFFFULL);
            carry = prod >> 32;
        }

        /* Propagate carry through remaining high digits. */
        for (ssize_t k = i + b->length; carry != 0 && k < out_len; k++) {
            uint64_t s = (uint64_t)digits[k] + carry;
            digits[k] = (uint32_t)(s & 0xFFFFFFFFULL);
            carry = s >> 32;
        }
    }

    int res_sign = (a->sign == b->sign) ? 1 : -1;
    menai_bigint_free(result);
    result->digits = digits;
    result->length = out_len;
    result->sign = res_sign;
    _menai_bigint_normalize(result);
    return 0;
}

/* Compute floor-division and modulo simultaneously. */
int
menai_bigint_divmod(
    const MenaiBigInt *a,
    const MenaiBigInt *b,
    MenaiBigInt *quotient,
    MenaiBigInt *remainder)
{
    if (b->sign == 0) {
        PyErr_SetString(PyExc_ZeroDivisionError, "integer division or modulo by zero");
        return -1;
    }

    if (a->sign == 0) {
        menai_bigint_free(quotient);
        menai_bigint_free(remainder);
        return 0;
    }

    /* Compute truncated division on magnitudes. */
    MenaiBigInt q, r;
    menai_bigint_init(&q);
    menai_bigint_init(&r);

    int ret;
    if (b->length == 1) {
        uint32_t rem_digit;
        ret = _menai_bigint_divmod_1(a, b->digits[0], &q, &rem_digit);
        if (ret < 0) {
            return -1;
        }

        if (rem_digit != 0) {
            ret = menai_bigint_from_long((long)rem_digit, &r);
            if (ret < 0) {
                menai_bigint_free(&q);
                return -1;
            }
        }
    } else {
        ret = _menai_bigint_divmod_mag(a, b, &q, &r);
        if (ret < 0) {
            return -1;
        }
    }

    /* Apply signs: truncated division. */
    int q_sign = (a->sign == b->sign) ? 1 : -1;
    int r_sign = a->sign;

    if (q.sign != 0) {
        q.sign = q_sign;
    }

    if (r.sign != 0) {
        r.sign = r_sign;
    }

    /*
     * Adjust for floor semantics: if remainder != 0 and signs of a and b
     * differ, quotient -= 1, remainder += b.
     */
    if (r.sign != 0 && a->sign != b->sign) {
        /* quotient -= 1 */
        MenaiBigInt one;
        menai_bigint_init(&one);
        if (menai_bigint_from_long(1L, &one) < 0) {
            menai_bigint_free(&q);
            menai_bigint_free(&r);
            return -1;
        }

        MenaiBigInt q_adj;
        menai_bigint_init(&q_adj);
        if (menai_bigint_sub(&q, &one, &q_adj) < 0) {
            menai_bigint_free(&q);
            menai_bigint_free(&r);
            menai_bigint_free(&one);
            return -1;
        }

        menai_bigint_free(&q);
        q = q_adj;
        menai_bigint_free(&one);

        /* remainder += b */
        MenaiBigInt r_adj;
        menai_bigint_init(&r_adj);
        if (menai_bigint_add(&r, b, &r_adj) < 0) {
            menai_bigint_free(&q);
            menai_bigint_free(&r);
            return -1;
        }

        menai_bigint_free(&r);
        r = r_adj;
    }

    *quotient = q;
    *remainder = r;
    return 0;
}

/* result = floor(a / b) */
int
menai_bigint_floordiv(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    if (b->sign == 0) {
        PyErr_SetString(PyExc_ZeroDivisionError, "integer division or modulo by zero");
        return -1;
    }

    MenaiBigInt q, r;
    menai_bigint_init(&q);
    menai_bigint_init(&r);
    if (menai_bigint_divmod(a, b, &q, &r) < 0) {
        return -1;
    }

    menai_bigint_free(&r);
    menai_bigint_free(result);
    *result = q;
    return 0;
}

/* result = a mod b */
int
menai_bigint_mod(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    if (b->sign == 0) {
        PyErr_SetString(PyExc_ZeroDivisionError, "integer division or modulo by zero");
        return -1;
    }

    MenaiBigInt q, r;
    menai_bigint_init(&q);
    menai_bigint_init(&r);
    if (menai_bigint_divmod(a, b, &q, &r) < 0) {
        return -1;
    }

    menai_bigint_free(&q);
    menai_bigint_free(result);
    *result = r;
    return 0;
}

/* result = -a */
int
menai_bigint_neg(const MenaiBigInt *a, MenaiBigInt *result)
{
    MenaiBigInt tmp;
    menai_bigint_init(&tmp);
    if (menai_bigint_copy(a, &tmp) < 0) {
        return -1;
    }

    if (tmp.sign != 0) {
        tmp.sign = -tmp.sign;
    }

    menai_bigint_free(result);
    *result = tmp;
    return 0;
}

/* result = |a| */
int
menai_bigint_abs(const MenaiBigInt *a, MenaiBigInt *result)
{
    MenaiBigInt tmp;
    menai_bigint_init(&tmp);
    if (menai_bigint_copy(a, &tmp) < 0) {
        return -1;
    }

    if (tmp.sign == -1) {
        tmp.sign = 1;
    }

    menai_bigint_free(result);
    *result = tmp;
    return 0;
}

/* result = a ** exp */
int
menai_bigint_pow(const MenaiBigInt *a, const MenaiBigInt *exp, MenaiBigInt *result)
{
    if (exp->sign == -1) {
        PyErr_SetString(PyExc_ValueError, "negative exponent in integer pow");
        return -1;
    }

    /* result = 1 */
    MenaiBigInt res;
    menai_bigint_init(&res);
    if (menai_bigint_from_long(1L, &res) < 0) {
        return -1;
    }

    /* base = a (copy so we can square it) */
    MenaiBigInt base;
    menai_bigint_init(&base);
    if (menai_bigint_copy(a, &base) < 0) {
        menai_bigint_free(&res);
        return -1;
    }

    /* e = exp (copy so we can shift it) */
    MenaiBigInt e;
    menai_bigint_init(&e);
    if (menai_bigint_copy(exp, &e) < 0) {
        menai_bigint_free(&res);
        menai_bigint_free(&base);
        return -1;
    }

    MenaiBigInt tmp;
    menai_bigint_init(&tmp);
    MenaiBigInt half;
    menai_bigint_init(&half);

    while (e.sign != 0) {
        /* Check if lowest bit of e is set. */
        if (e.digits[0] & 1U) {
            if (menai_bigint_mul(&res, &base, &tmp) < 0) {
                goto fail;
            }

            menai_bigint_free(&res);
            res = tmp;
            menai_bigint_init(&tmp);
        }

        /* e >>= 1 */
        if (menai_bigint_shift_right(&e, 1, &half) < 0) {
            goto fail;
        }

        menai_bigint_free(&e);
        e = half;
        menai_bigint_init(&half);

        if (e.sign == 0) {
            break;
        }

        /* base = base * base */
        if (menai_bigint_mul(&base, &base, &tmp) < 0) {
            goto fail;
        }

        menai_bigint_free(&base);
        base = tmp;
        menai_bigint_init(&tmp);
    }

    menai_bigint_free(&base);
    menai_bigint_free(&e);
    menai_bigint_free(&tmp);
    menai_bigint_free(&half);
    menai_bigint_free(result);
    *result = res;
    return 0;

fail:
    menai_bigint_free(&res);
    menai_bigint_free(&base);
    menai_bigint_free(&e);
    menai_bigint_free(&tmp);
    menai_bigint_free(&half);
    return -1;
}

/*
 * Convert a MenaiBigInt to a two's complement digit array of length *len_out.
 * The caller must free the returned array with free().
 * For positive numbers: digits as-is, with a leading zero word to ensure
 * the sign bit is clear.
 * For negative numbers: flip bits and add 1.
 * Returns NULL on allocation failure (PyErr_NoMemory set).
 */
static uint32_t *
_to_twos_complement(const MenaiBigInt *a, ssize_t *len_out)
{
    ssize_t len = a->length + 1; /* extra word for sign bit */
    uint32_t *buf = (uint32_t *)malloc((size_t)len * sizeof(uint32_t));
    if (buf == NULL) {
        PyErr_NoMemory();
        return NULL;
    }

    if (a->sign >= 0) {
        /* Positive or zero: copy digits, pad with 0. */
        for (ssize_t i = 0; i < a->length; i++) {
            buf[i] = a->digits[i];
        }

        buf[a->length] = 0;
    } else {
        /* Negative: flip bits and add 1. */
        uint64_t carry = 1;
        for (ssize_t i = 0; i < a->length; i++) {
            uint64_t v = (~(uint64_t)a->digits[i] & 0xFFFFFFFFULL) + carry;
            buf[i] = (uint32_t)(v & 0xFFFFFFFFULL);
            carry = v >> 32;
        }

        /* The extra word: ~0 + carry. For a non-zero negative number,
         * the two's complement of the magnitude fills the lower words,
         * and the sign extension is all 1s. */
        buf[a->length] = 0xFFFFFFFFU;
    }

    *len_out = len;
    return buf;
}

/*
 * Convert a two's complement digit array back to a MenaiInt.
 * The sign bit is the MSB of buf[len-1].
 */
static int
_from_twos_complement(const uint32_t *buf, ssize_t len, MenaiBigInt *result)
{
    menai_bigint_free(result);
    if (len == 0) {
        return 0;
    }

    int is_neg = (buf[len - 1] & 0x80000000U) != 0;

    if (!is_neg) {
        /* Positive: copy digits directly. */
        uint32_t *digits = (uint32_t *)malloc((size_t)len * sizeof(uint32_t));
        if (digits == NULL) {
            PyErr_NoMemory();
            return -1;
        }

        for (ssize_t i = 0; i < len; i++) {
            digits[i] = buf[i];
        }

        result->digits = digits;
        result->length = len;
        result->sign = 1;
        _menai_bigint_normalize(result);
    } else {
        /* Negative: negate to get magnitude. */
        uint32_t *digits = (uint32_t *)malloc((size_t)len * sizeof(uint32_t));
        if (digits == NULL) {
            PyErr_NoMemory();
            return -1;
        }

        uint64_t carry = 1;
        for (ssize_t i = 0; i < len; i++) {
            uint64_t v = (~(uint64_t)buf[i] & 0xFFFFFFFFULL) + carry;
            digits[i] = (uint32_t)(v & 0xFFFFFFFFULL);
            carry = v >> 32;
        }

        result->digits = digits;
        result->length = len;
        result->sign = -1;
        _menai_bigint_normalize(result);
    }

    return 0;
}

/* result = a & b */
int
menai_bigint_and(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    ssize_t la;
    uint32_t *ta = _to_twos_complement(a, &la);
    if (ta == NULL) {
        return -1;
    }

    ssize_t lb;
    uint32_t *tb = _to_twos_complement(b, &lb);
    if (tb == NULL) {
        free(ta);
        return -1;
    }

    ssize_t out_len = (la > lb) ? la : lb;
    uint32_t *out = (uint32_t *)malloc((size_t)out_len * sizeof(uint32_t));
    if (out == NULL) {
        free(ta);
        free(tb);
        PyErr_NoMemory();
        return -1;
    }

    /* Sign-extend: positive extends with 0, negative extends with 0xFFFFFFFF. */
    uint32_t ext_a = (a->sign == -1) ? 0xFFFFFFFFU : 0U;
    uint32_t ext_b = (b->sign == -1) ? 0xFFFFFFFFU : 0U;

    for (ssize_t i = 0; i < out_len; i++) {
        uint32_t da = (i < la) ? ta[i] : ext_a;
        uint32_t db = (i < lb) ? tb[i] : ext_b;
        out[i] = da & db;
    }

    free(ta);
    free(tb);

    int ret = _from_twos_complement(out, out_len, result);
    free(out);
    return ret;
}

/* result = a | b */
int
menai_bigint_or(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    ssize_t la;
    uint32_t *ta = _to_twos_complement(a, &la);
    if (ta == NULL) {
        return -1;
    }

    ssize_t lb;
    uint32_t *tb = _to_twos_complement(b, &lb);
    if (tb == NULL) {
        free(ta);
        return -1;
    }

    ssize_t out_len = (la > lb) ? la : lb;
    uint32_t *out = (uint32_t *)malloc((size_t)out_len * sizeof(uint32_t));
    if (out == NULL) {
        free(ta);
        free(tb);
        PyErr_NoMemory();
        return -1;
    }

    uint32_t ext_a = (a->sign == -1) ? 0xFFFFFFFFU : 0U;
    uint32_t ext_b = (b->sign == -1) ? 0xFFFFFFFFU : 0U;

    for (ssize_t i = 0; i < out_len; i++) {
        uint32_t da = (i < la) ? ta[i] : ext_a;
        uint32_t db = (i < lb) ? tb[i] : ext_b;
        out[i] = da | db;
    }

    free(ta);
    free(tb);

    int ret = _from_twos_complement(out, out_len, result);
    free(out);
    return ret;
}

/* result = a ^ b */
int
menai_bigint_xor(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    ssize_t la;
    uint32_t *ta = _to_twos_complement(a, &la);
    if (ta == NULL) {
        return -1;
    }

    ssize_t lb;
    uint32_t *tb = _to_twos_complement(b, &lb);
    if (tb == NULL) {
        free(ta);
        return -1;
    }

    ssize_t out_len = (la > lb) ? la : lb;
    uint32_t *out = (uint32_t *)malloc((size_t)out_len * sizeof(uint32_t));
    if (out == NULL) {
        free(ta);
        free(tb);
        PyErr_NoMemory();
        return -1;
    }

    uint32_t ext_a = (a->sign == -1) ? 0xFFFFFFFFU : 0U;
    uint32_t ext_b = (b->sign == -1) ? 0xFFFFFFFFU : 0U;

    for (ssize_t i = 0; i < out_len; i++) {
        uint32_t da = (i < la) ? ta[i] : ext_a;
        uint32_t db = (i < lb) ? tb[i] : ext_b;
        out[i] = da ^ db;
    }

    free(ta);
    free(tb);

    int ret = _from_twos_complement(out, out_len, result);
    free(out);
    return ret;
}

/* result = ~a, equivalent to -(a + 1) */
int
menai_bigint_not(const MenaiBigInt *a, MenaiBigInt *result)
{
    MenaiBigInt one;
    menai_bigint_init(&one);
    if (menai_bigint_from_long(1L, &one) < 0) {
        return -1;
    }

    MenaiBigInt sum;
    menai_bigint_init(&sum);
    if (menai_bigint_add(a, &one, &sum) < 0) {
        menai_bigint_free(&one);
        return -1;
    }

    menai_bigint_free(&one);

    if (menai_bigint_neg(&sum, result) < 0) {
        menai_bigint_free(&sum);
        return -1;
    }

    menai_bigint_free(&sum);
    return 0;
}

/* result = a << shift */
int
menai_bigint_shift_left(const MenaiBigInt *a, ssize_t shift, MenaiBigInt *result)
{
    if (shift < 0) {
        PyErr_SetString(PyExc_ValueError, "negative shift count");
        return -1;
    }

    if (a->sign == 0 || shift == 0) {
        MenaiBigInt tmp;
        menai_bigint_init(&tmp);
        if (menai_bigint_copy(a, &tmp) < 0) {
            return -1;
        }

        menai_bigint_free(result);
        *result = tmp;
        return 0;
    }

    ssize_t word_shift = shift / 32;
    int bit_shift = (int)(shift % 32);

    ssize_t out_len = a->length + word_shift + 1;
    uint32_t *digits = (uint32_t *)malloc((size_t)out_len * sizeof(uint32_t));
    if (digits == NULL) {
        PyErr_NoMemory();
        return -1;
    }

    memset(digits, 0, (size_t)out_len * sizeof(uint32_t));

    if (bit_shift == 0) {
        for (ssize_t i = 0; i < a->length; i++) {
            digits[i + word_shift] = a->digits[i];
        }
    } else {
        uint32_t carry = 0;
        for (ssize_t i = 0; i < a->length; i++) {
            uint64_t v = ((uint64_t)a->digits[i] << bit_shift) | carry;
            digits[i + word_shift] = (uint32_t)(v & 0xFFFFFFFFULL);
            carry = (uint32_t)(v >> 32);
        }

        digits[a->length + word_shift] = carry;
    }

    menai_bigint_free(result);
    result->digits = digits;
    result->length = out_len;
    result->sign = a->sign;
    _menai_bigint_normalize(result);
    return 0;
}

/* result = a >> shift (arithmetic, floor toward -inf) */
int
menai_bigint_shift_right(const MenaiBigInt *a, ssize_t shift, MenaiBigInt *result)
{
    if (shift < 0) {
        PyErr_SetString(PyExc_ValueError, "negative shift count");
        return -1;
    }

    if (a->sign == 0) {
        menai_bigint_free(result);
        return 0;
    }

    if (shift == 0) {
        MenaiBigInt tmp;
        menai_bigint_init(&tmp);
        if (menai_bigint_copy(a, &tmp) < 0) {
            return -1;
        }

        menai_bigint_free(result);
        *result = tmp;
        return 0;
    }

    ssize_t word_shift = shift / 32;
    int bit_shift = (int)(shift % 32);

    /* If shifting away all digits, result is 0 (positive) or -1 (negative). */
    if (word_shift >= a->length) {
        menai_bigint_free(result);
        if (a->sign == -1) {
            return menai_bigint_from_long(-1L, result);
        }

        return 0;
    }

    /*
     * Check if any bits are shifted out (for negative floor adjustment).
     * Bits shifted out: the low word_shift words, plus the low bit_shift
     * bits of word at index word_shift.
     */
    int any_bits_out = 0;
    if (a->sign == -1) {
        for (ssize_t i = 0; i < word_shift && !any_bits_out; i++) {
            if (a->digits[i] != 0) {
                any_bits_out = 1;
            }
        }

        if (!any_bits_out && bit_shift > 0) {
            uint32_t mask = (1U << bit_shift) - 1U;
            if (a->digits[word_shift] & mask) {
                any_bits_out = 1;
            }
        }
    }

    ssize_t out_len = a->length - word_shift;
    uint32_t *digits = (uint32_t *)malloc((size_t)out_len * sizeof(uint32_t));
    if (digits == NULL) {
        PyErr_NoMemory();
        return -1;
    }

    if (bit_shift == 0) {
        for (ssize_t i = 0; i < out_len; i++) {
            digits[i] = a->digits[i + word_shift];
        }
    } else {
        for (ssize_t i = 0; i < out_len; i++) {
            uint32_t lo = a->digits[i + word_shift] >> bit_shift;
            uint32_t hi = 0;
            if (i + word_shift + 1 < a->length) {
                hi = a->digits[i + word_shift + 1] << (32 - bit_shift);
            }

            digits[i] = lo | hi;
        }
    }

    menai_bigint_free(result);
    result->digits = digits;
    result->length = out_len;
    result->sign = a->sign;
    _menai_bigint_normalize(result);

    /* For negative numbers with bits shifted out, subtract 1 (floor). */
    if (a->sign == -1 && any_bits_out) {
        MenaiBigInt one;
        menai_bigint_init(&one);
        if (menai_bigint_from_long(1L, &one) < 0) {
            menai_bigint_free(result);
            return -1;
        }

        MenaiBigInt adj;
        menai_bigint_init(&adj);
        if (menai_bigint_sub(result, &one, &adj) < 0) {
            menai_bigint_free(&one);
            menai_bigint_free(result);
            return -1;
        }

        menai_bigint_free(&one);
        menai_bigint_free(result);
        *result = adj;
    }

    return 0;
}

/* Return 1 if a == b, 0 otherwise. */
int
menai_bigint_eq(const MenaiBigInt *a, const MenaiBigInt *b)
{
    if (a->sign != b->sign) {
        return 0;
    }

    if (a->length != b->length) {
        return 0;
    }

    for (ssize_t i = 0; i < a->length; i++) {
        if (a->digits[i] != b->digits[i]) {
            return 0;
        }
    }

    return 1;
}

/* Return 1 if a != b, 0 otherwise. */
int
menai_bigint_ne(const MenaiBigInt *a, const MenaiBigInt *b)
{
    return !menai_bigint_eq(a, b);
}

/* Return 1 if a < b, 0 otherwise. */
int
menai_bigint_lt(const MenaiBigInt *a, const MenaiBigInt *b)
{
    if (a->sign != b->sign) {
        return a->sign < b->sign;
    }

    if (a->sign == 0) {
        return 0;
    }

    int cmp = _menai_bigint_cmp_mag(a, b);
    if (a->sign == 1) {
        return cmp < 0;
    }

    /* Both negative: larger magnitude means smaller value. */
    return cmp > 0;
}

/* Return 1 if a > b, 0 otherwise. */
int
menai_bigint_gt(const MenaiBigInt *a, const MenaiBigInt *b)
{
    return menai_bigint_lt(b, a);
}

/* Return 1 if a <= b, 0 otherwise. */
int
menai_bigint_le(const MenaiBigInt *a, const MenaiBigInt *b)
{
    return !menai_bigint_gt(a, b);
}

/* Return 1 if a >= b, 0 otherwise. */
int
menai_bigint_ge(const MenaiBigInt *a, const MenaiBigInt *b)
{
    return !menai_bigint_lt(a, b);
}
