/*
 * menai_vm_bigint.c — arbitrary-precision integer library for the Menai VM.
 *
 * Representation: sign-magnitude, base 2^32, little-endian digits.
 * Zero: sign=0, length=0, digits=NULL.
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
void
menai_bigint_normalize(MenaiBigInt *a)
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
        return MENAI_ERR_NOMEM;
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
    menai_bigint_normalize(result);
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
        return MENAI_ERR_NOMEM;
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
    menai_bigint_normalize(result);
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
        return MENAI_ERR_NOMEM;
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
    menai_bigint_normalize(quotient);
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
        return MENAI_ERR_NOMEM;
    }

    uint32_t *vn = (uint32_t *)calloc((size_t)n, sizeof(uint32_t));
    if (vn == NULL) {
        free(un);
        return MENAI_ERR_NOMEM;
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
        return MENAI_ERR_NOMEM;
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
    menai_bigint_normalize(quotient);

    /* Unnormalize remainder: shift un right by d bits. */
    uint32_t *rdigits = (uint32_t *)malloc((size_t)n * sizeof(uint32_t));
    if (rdigits == NULL) {
        free(un);
        free(vn);
        return MENAI_ERR_NOMEM;
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
    menai_bigint_normalize(remainder);

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
        return MENAI_ERR_NOMEM;
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
        return MENAI_ERR_NOMEM;
    }

    digits[0] = (uint32_t)(mag & 0xFFFFFFFFUL);
    if (len == 2) {
        digits[1] = (uint32_t)((uint64_t)mag >> 32);
    }

    a->digits = digits;
    a->length = len;
    a->sign = sign;
    menai_bigint_normalize(a);
    return 0;
}

/*
 * Set a to the signed 64-bit value v.
 * On platforms where long is 32-bit (e.g. MSVC/Windows), this handles
 * values that exceed the range of long.
 */
int
menai_bigint_from_long_long(long long v, MenaiBigInt *a)
{
    menai_bigint_free(a);
    if (v == 0) {
        return 0;
    }

    int sign;
    unsigned long long mag;
    if (v < 0) {
        sign = -1;
        /* Avoid UB for LLONG_MIN: cast to unsigned before negating. */
        mag = (unsigned long long)(-(v + 1)) + 1ULL;
    } else {
        sign = 1;
        mag = (unsigned long long)v;
    }

    ssize_t len;
    if (mag <= 0xFFFFFFFFULL) {
        len = 1;
    } else {
        len = 2;
    }

    uint32_t *digits = (uint32_t *)malloc((size_t)len * sizeof(uint32_t));
    if (digits == NULL) {
        return MENAI_ERR_NOMEM;
    }

    digits[0] = (uint32_t)(mag & 0xFFFFFFFFULL);
    if (len == 2) {
        digits[1] = (uint32_t)(mag >> 32);
    }

    a->digits = digits;
    a->length = len;
    a->sign = sign;
    menai_bigint_normalize(a);
    return 0;
}

/* Set a to the unsigned value of v. */
int
menai_bigint_from_unsigned_long_long(unsigned long long v, MenaiBigInt *a)
{
    menai_bigint_free(a);
    if (v == 0) {
        return 0;
    }

    uint64_t mag = (uint64_t)v;

    ssize_t len;
    if (mag <= 0xFFFFFFFFUL) {
        len = 1;
    } else {
        len = 2;
    }

    uint32_t *digits = (uint32_t *)malloc((size_t)len * sizeof(uint32_t));
    if (digits == NULL) {
        return MENAI_ERR_NOMEM;
    }

    digits[0] = (uint32_t)(mag & 0xFFFFFFFFUL);
    if (len == 2) {
        digits[1] = (uint32_t)(mag >> 32);
    }

    a->digits = digits;
    a->length = len;
    a->sign = 1;
    menai_bigint_normalize(a);
    return 0;
}

/* Parse a NUL-terminated string in the given base and store the result in a. */
int
menai_bigint_from_string(const char *s, int base, MenaiBigInt *a)
{
    if (s == NULL || (base != 2 && base != 8 && base != 10 && base != 16)) {
        return MENAI_ERR_VALUE;
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
        return MENAI_ERR_VALUE;
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
            return MENAI_ERR_VALUE;
        }

        if (digit >= base) {
            return MENAI_ERR_VALUE;
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
        return MENAI_ERR_VALUE;
    }

    /* Allow a leading sign plus all digit characters — all ASCII. */
    char *buf = (char *)malloc((size_t)(len + 1));
    if (!buf) {
        return MENAI_ERR_NOMEM;
    }

    for (ssize_t i = 0; i < len; i++) {
        if (data[i] > 0x7F) {
            free(buf);
            return MENAI_ERR_VALUE;
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
 * set the sign.
 */
int
menai_bigint_from_double(double v, MenaiBigInt *a)
{
    /* Work with the magnitude; v is already trunc()'d by the caller. */
    double t = v < 0.0 ? -v : v;
    if (!isfinite(t)) {
        return MENAI_ERR_VALUE;
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
        return MENAI_ERR_NOMEM;
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
    menai_bigint_normalize(a);
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
        return MENAI_ERR_OVERFLOW;
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

/*
 * Return 1 if the value of a fits in a C long long, 0 otherwise.
 * On platforms where long is already 64-bit, this is equivalent to
 * menai_bigint_fits_long.
 */
int
menai_bigint_fits_long_long(const MenaiBigInt *a)
{
    if (a->length == 0) {
        return 1;
    }

    if (a->length > 2) {
        return 0;
    }

    uint64_t mag = a->digits[0];
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

/* Store the value of a in *out as a C long long. */
int
menai_bigint_to_long_long(const MenaiBigInt *a, long long *out)
{
    if (!menai_bigint_fits_long_long(a)) {
        return MENAI_ERR_OVERFLOW;
    }

    if (a->length == 0) {
        *out = 0;
        return 0;
    }

    uint64_t mag = a->digits[0];
    if (a->length == 2) {
        mag |= ((uint64_t)a->digits[1] << 32);
    }

    if (a->sign == -1) {
        *out = (long long)(0ULL - mag);
    } else {
        *out = (long long)mag;
    }

    return 0;
}

/* Return 1 if the value of a fits in an unsigned long long, 0 otherwise. */
int
menai_bigint_fits_unsigned_long_long(const MenaiBigInt *a)
{
    if (a->sign == -1) {
        return 0;
    }

    if (a->length <= 2) {
        return 1;
    }

    return 0;
}

/* Store the value of a in *out as an unsigned long long. */
int
menai_bigint_to_unsigned_long_long(const MenaiBigInt *a, unsigned long long *out)
{
    if (!menai_bigint_fits_unsigned_long_long(a)) {
        return MENAI_ERR_OVERFLOW;
    }

    if (a->length == 0) {
        *out = 0;
        return 0;
    }

    uint64_t mag = a->digits[0];
    if (a->length == 2) {
        mag |= ((uint64_t)a->digits[1] << 32);
    }

    *out = (unsigned long long)mag;
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
        return MENAI_ERR_OVERFLOW;
    }

    *out = (a->sign == -1) ? -result : result;
    return 0;
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
hash_t
menai_bigint_hash(const MenaiBigInt *a)
{
    if (a->sign == 0) {
        return 0;
    }

    /* FNV-1a, 64-bit variant. */
    uint64_t h = 14695981039346656037ULL;
    for (ssize_t i = 0; i < a->length; i++) {
        uint32_t d = a->digits[i];
        h ^= (uint64_t)(d & 0xFF);
        h *= 1099511628211ULL;
        h ^= (uint64_t)((d >> 8) & 0xFF);
        h *= 1099511628211ULL;
        h ^= (uint64_t)((d >> 16) & 0xFF);
        h *= 1099511628211ULL;
        h ^= (uint64_t)((d >> 24) & 0xFF);
        h *= 1099511628211ULL;
    }

    if (a->sign == -1) {
        h = ~h;
    }

    hash_t result = (hash_t)h;
    return (result == -1) ? -2 : result;
}

/* result = a + b */
int
menai_bigint_add(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    /* Handle zero operands. */
    if (a->sign == 0) {
        return menai_bigint_copy(b, result);
    }

    if (b->sign == 0) {
        return menai_bigint_copy(a, result);
    }

    if (a->sign == b->sign) {
        /* Same sign: add magnitudes, keep sign. */
        int s = a->sign;
        if (_menai_bigint_add_mag(a, b, result) < 0) {
            return -1;
        }

        if (result->sign != 0) {
            result->sign = s;
        }
        return 0;
    }

    /* Different signs: subtract smaller magnitude from larger. */
    int cmp = _menai_bigint_cmp_mag(a, b);
    if (cmp == 0) {
        menai_bigint_free(result);
        return 0;
    }

    int res_sign;
    if (cmp > 0) {
        /* |a| > |b|: result has sign of a */
        if (_menai_bigint_sub_mag(a, b, result) < 0) {
            return -1;
        }

        res_sign = a->sign;
    } else {
        /* |b| > |a|: result has sign of b */
        if (_menai_bigint_sub_mag(b, a, result) < 0) {
            return -1;
        }

        res_sign = b->sign;
    }

    if (result->sign != 0) {
        result->sign = res_sign;
    }
    return 0;
}

/* result = a - b */
int
menai_bigint_sub(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
    /* Handle zero operands. */
    if (a->sign == 0) {
        /* 0 - b = -b */
        return menai_bigint_neg(b, result);
    }

    if (b->sign == 0) {
        /* a - 0 = a */
        return menai_bigint_copy(a, result);
    }

    if (a->sign != b->sign) {
        /*
         * Different signs: subtracting is adding magnitudes.
         * a - (-b) = a + b  (result has sign of a)
         * (-a) - b = -(a + b)  (result has sign of a)
         */
        int s = a->sign;
        if (_menai_bigint_add_mag(a, b, result) < 0) {
            return -1;
        }

        if (result->sign != 0) {
            result->sign = s;
        }

        return 0;
    }

    /* Same sign: subtract smaller magnitude from larger. */
    int cmp = _menai_bigint_cmp_mag(a, b);
    if (cmp == 0) {
        menai_bigint_free(result);
        return 0;
    }

    int res_sign;
    if (cmp > 0) {
        if (_menai_bigint_sub_mag(a, b, result) < 0) {
            return -1;
        }
        res_sign = a->sign;
    } else {
        if (_menai_bigint_sub_mag(b, a, result) < 0) {
            return -1;
        }
        res_sign = -a->sign;
    }

    if (result->sign != 0) {
        result->sign = res_sign;
    }

    return 0;
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
        return MENAI_ERR_NOMEM;
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
    menai_bigint_normalize(result);
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
        return MENAI_ERR_DIVISION_BY_ZERO;
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
        /* quotient -= 1 (in-place) */
        MenaiBigInt one;
        menai_bigint_init(&one);
        if (menai_bigint_from_long(1L, &one) < 0 ||
            menai_bigint_sub(&q, &one, &q) < 0) {
            menai_bigint_free(&q);
            menai_bigint_free(&r);
            menai_bigint_free(&one);
            return -1;
        }

        menai_bigint_free(&one);

        /* remainder += b (in-place) */
        if (menai_bigint_add(&r, b, &r) < 0) {
            menai_bigint_free(&q);
            menai_bigint_free(&r);
            return -1;
        }
    }

    *quotient = q;
    *remainder = r;
    return 0;
}

/* result = floor(a / b) */
int
menai_bigint_floordiv(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result)
{
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
    if (menai_bigint_copy(a, result) < 0) {
        return -1;
    }

    if (result->sign != 0) {
        result->sign = -result->sign;
    }
    return 0;
}

/* result = |a| */
int
menai_bigint_abs(const MenaiBigInt *a, MenaiBigInt *result)
{
    if (menai_bigint_copy(a, result) < 0) {
        return -1;
    }

    if (result->sign == -1) {
        result->sign = 1;
    }
    return 0;
}

/* result = a ** exp */
int
menai_bigint_pow(const MenaiBigInt *a, const MenaiBigInt *exp, MenaiBigInt *result)
{
    if (exp->sign == -1) {
        return MENAI_ERR_VALUE;
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
 * Returns NULL on allocation failure.
 */
static uint32_t *
_to_twos_complement(const MenaiBigInt *a, ssize_t *len_out)
{
    ssize_t len = a->length + 1; /* extra word for sign bit */
    uint32_t *buf = (uint32_t *)malloc((size_t)len * sizeof(uint32_t));
    if (buf == NULL) {
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
            return MENAI_ERR_NOMEM;
        }

        for (ssize_t i = 0; i < len; i++) {
            digits[i] = buf[i];
        }

        result->digits = digits;
        result->length = len;
        result->sign = 1;
        menai_bigint_normalize(result);
    } else {
        /* Negative: negate to get magnitude. */
        uint32_t *digits = (uint32_t *)malloc((size_t)len * sizeof(uint32_t));
        if (digits == NULL) {
            return MENAI_ERR_NOMEM;
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
        menai_bigint_normalize(result);
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
        return MENAI_ERR_NOMEM;
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
        return MENAI_ERR_NOMEM;
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
        return MENAI_ERR_NOMEM;
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
    if (a->sign == 0) {
        /* ~0 = -1 */
        menai_bigint_free(result);
        return menai_bigint_from_long(-1L, result);
    }

    if (a->sign == 1) {
        /*
         * Positive: ~a = -(a + 1).
         * Increment magnitude, then negate sign.
         */
        MenaiBigInt one;
        menai_bigint_init(&one);
        if (menai_bigint_from_long(1L, &one) < 0) {
            return -1;
        }

        int ret = _menai_bigint_add_mag(a, &one, result);
        menai_bigint_free(&one);
        if (ret < 0) {
            return -1;
        }

        if (result->sign != 0) {
            result->sign = -1;
        }

        return 0;
    }

    /*
     * Negative: ~a = -(a + 1) = |a| - 1.
     * Decrement magnitude, result is positive.
     */
    MenaiBigInt one;
    menai_bigint_init(&one);
    if (menai_bigint_from_long(1L, &one) < 0) {
        return -1;
    }

    int ret = _menai_bigint_sub_mag(a, &one, result);
    menai_bigint_free(&one);
    if (ret < 0) {
        return -1;
    }

    if (result->sign != 0) {
        result->sign = 1;
    }

    return 0;
}

/* result = a << shift */
int
menai_bigint_shift_left(const MenaiBigInt *a, ssize_t shift, MenaiBigInt *result)
{
    if (shift < 0) {
        return MENAI_ERR_VALUE;
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
        return MENAI_ERR_NOMEM;
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
    menai_bigint_normalize(result);
    return 0;
}

/* result = a >> shift (arithmetic, floor toward -inf) */
int
menai_bigint_shift_right(const MenaiBigInt *a, ssize_t shift, MenaiBigInt *result)
{
    if (shift < 0) {
        return MENAI_ERR_VALUE;
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
        return MENAI_ERR_NOMEM;
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
    menai_bigint_normalize(result);

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
