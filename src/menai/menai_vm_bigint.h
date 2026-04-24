/*
 * menai_vm_bigint.h — arbitrary-precision integer library for the Menai VM.
 *
 * Representation: sign-magnitude with base-2^32 digits stored little-endian.
 * Zero is the unique value with sign == 0, length == 0, digits == NULL.
 *
 * Lifecycle contract:
 *   Every MenaiBigInt used as an output parameter must first be initialised with
 *   menai_bigint_init.  Call menai_bigint_free when the value is no longer needed.
 *   Freeing a zero-initialised MenaiBigInt that was never written to is safe.
 *
 * Output aliasing:
 *   All arithmetic and bitwise functions accept an output pointer that may
 *   alias one or both inputs.  Implementations use temporaries as needed to
 *   ensure correct behaviour in the aliased case.
 *   Exception: menai_bigint_divmod requires that quotient and remainder do not
 *   alias each other or either input.
 *
 * Return values:
 *   Functions that can fail return 0 on success and -1 on error.
 *   On allocation failure PyErr_NoMemory() is set.
 *   On other errors (e.g. division by zero) an appropriate Python exception
 *   is set.
 *
 * Python API boundary:
 *   Only menai_bigint_from_pylong and menai_bigint_to_pylong may call the Python
 *   C API.  All other functions are pure C.
 *
 * Memory:
 *   All heap allocation uses malloc / free.
 *   Strings returned by menai_bigint_to_string must be freed with free().
 */
#ifndef MENAI_VM_BIGINT_H
#define MENAI_VM_BIGINT_H

/* Sign-magnitude arbitrary-precision integer. */
typedef struct {
    uint32_t *digits;  /* little-endian base-2^32 magnitude; NULL when zero */
    ssize_t length;    /* number of valid digits; 0 when zero */
    int sign;          /* -1, 0, or 1 */
} MenaiBigInt;

/* Initialise a MenaiBigInt to zero. Must be called before first use as output. */
#define menai_bigint_init(x) (memset((x), 0, sizeof(MenaiBigInt)))

void menai_bigint_free(MenaiBigInt *a);
int menai_bigint_copy(const MenaiBigInt *src, MenaiBigInt *dst);
int menai_bigint_from_long(long v, MenaiBigInt *a);
int menai_bigint_from_pylong(PyObject *obj, MenaiBigInt *a);
int menai_bigint_from_string(const char *s, int base, MenaiBigInt *a);
int menai_bigint_from_codepoints(const uint32_t *data, ssize_t len, int base, MenaiBigInt *a);
int menai_bigint_from_double(double v, MenaiBigInt *a);
int menai_bigint_fits_long(const MenaiBigInt *a);
int menai_bigint_to_long(const MenaiBigInt *a, long *out);
int menai_bigint_to_double(const MenaiBigInt *a, double *out);
PyObject *menai_bigint_to_pylong(const MenaiBigInt *a);
int menai_bigint_to_string(const MenaiBigInt *a, int base, char **out);
Py_hash_t menai_bigint_hash(const MenaiBigInt *a);
int menai_bigint_add(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_sub(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_mul(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_floordiv(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_mod(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_divmod(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *quotient, MenaiBigInt *remainder);
int menai_bigint_neg(const MenaiBigInt *a, MenaiBigInt *result);
int menai_bigint_abs(const MenaiBigInt *a, MenaiBigInt *result);
int menai_bigint_pow(const MenaiBigInt *a, const MenaiBigInt *exp, MenaiBigInt *result);
int menai_bigint_and(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_or(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_xor(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_not(const MenaiBigInt *a, MenaiBigInt *result);
int menai_bigint_shift_left(const MenaiBigInt *a, ssize_t shift, MenaiBigInt *result);
int menai_bigint_shift_right(const MenaiBigInt *a, ssize_t shift, MenaiBigInt *result);
int menai_bigint_eq(const MenaiBigInt *a, const MenaiBigInt *b);
int menai_bigint_ne(const MenaiBigInt *a, const MenaiBigInt *b);
int menai_bigint_lt(const MenaiBigInt *a, const MenaiBigInt *b);
int menai_bigint_gt(const MenaiBigInt *a, const MenaiBigInt *b);
int menai_bigint_le(const MenaiBigInt *a, const MenaiBigInt *b);
int menai_bigint_ge(const MenaiBigInt *a, const MenaiBigInt *b);

#endif /* MENAI_VM_BIGINT_H */
