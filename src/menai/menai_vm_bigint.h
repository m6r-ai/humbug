/*
 * menai_vm_bigint.h — arbitrary-precision integer library for the Menai VM.
 *
 * Representation: sign-magnitude with base-2^32 digits stored little-endian.
 * Zero is the unique value with sign == 0, length == 0, digits == NULL.
 *
 * Lifecycle contract:
 *   Every MenaiInt used as an output parameter must first be initialised with
 *   menai_int_init.  Call menai_int_free when the value is no longer needed.
 *   Freeing a zero-initialised MenaiInt that was never written to is safe.
 *
 * Output aliasing:
 *   All arithmetic and bitwise functions accept an output pointer that may
 *   alias one or both inputs.  Implementations use temporaries as needed to
 *   ensure correct behaviour in the aliased case.
 *   Exception: menai_int_divmod requires that quotient and remainder do not
 *   alias each other or either input.
 *
 * Return values:
 *   Functions that can fail return 0 on success and -1 on error.
 *   On allocation failure PyErr_NoMemory() is set.
 *   On other errors (e.g. division by zero) an appropriate Python exception
 *   is set.
 *
 * Python API boundary:
 *   Only menai_int_from_pylong, menai_int_to_pylong, and menai_int_hash
 *   may call the Python C API.  All other functions are pure C.
 *
 * Memory:
 *   All heap allocation uses PyMem_Malloc / PyMem_Realloc / PyMem_Free.
 *   Strings returned by menai_int_to_string must be freed with PyMem_Free.
 */

#ifndef MENAI_VM_BIGINT_H
#define MENAI_VM_BIGINT_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* Sign-magnitude arbitrary-precision integer. */
typedef struct {
    uint32_t *digits;  /* little-endian base-2^32 magnitude; NULL when zero */
    Py_ssize_t length; /* number of valid digits; 0 when zero */
    int sign;          /* -1, 0, or 1 */
} MenaiInt;

/* Initialise a MenaiInt to zero. Must be called before first use as output. */
#define menai_int_init(x) (memset((x), 0, sizeof(MenaiInt)))

/* Free the digit array and reset to zero. Safe on a zero-initialised value. */
void menai_int_free(MenaiInt *a);

/* Copy src into dst. Returns 0 on success, -1 on error. */
int menai_int_copy(const MenaiInt *src, MenaiInt *dst);

/*
 * Construct from primitive types.
 */

/* Set a to the value of v. Returns 0 on success, -1 on error. */
int menai_int_from_long(long v, MenaiInt *a);

/*
 * Set a to the value of the CPython integer object obj.
 * obj must be a PyLongObject. Returns 0 on success, -1 on error.
 * May use the Python C API.
 */
int menai_int_from_pylong(PyObject *obj, MenaiInt *a);

/*
 * Parse the NUL-terminated string s in the given base (2, 8, 10, or 16)
 * and store the result in a. An optional leading sign is accepted.
 * Returns 0 on success, -1 on error (ValueError set for bad input).
 */
int menai_int_from_string(const char *s, int base, MenaiInt *a);

/*
 * Parse a UTF-32 codepoint array of length len in the given base (2, 8, 10,
 * or 16) and store the result in a.  An optional leading '+' or '-' is
 * accepted.  All valid integer literals in any supported base are ASCII-only,
 * so each codepoint is simply cast to char after a range check.
 * Returns 0 on success, -1 on error (ValueError set for bad input).
 */
int menai_int_from_codepoints(const uint32_t *data, Py_ssize_t len, int base, MenaiInt *a);

/*
 * Convert the truncated value of v (i.e. trunc(v)) to a MenaiInt.
 * v must be finite. Returns 0 on success, -1 on error.
 */
int menai_int_from_double(double v, MenaiInt *a);

/*
 * Convert to primitive types.
 */

/* Return 1 if the value of a fits in a C long, 0 otherwise. Never fails. */
int menai_int_fits_long(const MenaiInt *a);

/*
 * Store the value of a in *out as a C long.
 * Returns 0 on success, -1 with OverflowError set if the value does not fit.
 */
int menai_int_to_long(const MenaiInt *a, long *out);

/*
 * Store the value of a as a double in *out.
 * Returns 0 on success, -1 with OverflowError set if the magnitude is too
 * large to represent as a finite double.
 */
int menai_int_to_double(const MenaiInt *a, double *out);

/*
 * Return a new CPython integer object representing the value of a, or NULL
 * on error. The caller owns the returned reference.
 * May use the Python C API.
 */
PyObject *menai_int_to_pylong(const MenaiInt *a);

/*
 * Write the string representation of a in the given base (2, 8, 10, or 16)
 * into *out. The returned buffer is NUL-terminated and must be freed by the
 * caller with PyMem_Free. On success stores the buffer in *out and returns 0.
 * On error sets *out to NULL and returns -1.
 */
int menai_int_to_string(const MenaiInt *a, int base, char **out);

/*
 * Compute a Py_hash_t for a compatible with CPython's integer hash algorithm,
 * so that MenaiInt values used as dict keys are consistent with Python int keys.
 * Returns -2 (not -1) for values whose hash would mathematically be -1,
 * following CPython convention. Returns -1 only to signal an internal error.
 * May use the Python C API.
 */
Py_hash_t menai_int_hash(const MenaiInt *a);

/*
 * Arithmetic operations.
 * All write their result to *result, which may alias *a and/or *b.
 * Return 0 on success, -1 on error.
 */

/* result = a + b */
int menai_int_add(const MenaiInt *a, const MenaiInt *b, MenaiInt *result);

/* result = a - b */
int menai_int_sub(const MenaiInt *a, const MenaiInt *b, MenaiInt *result);

/* result = a * b */
int menai_int_mul(const MenaiInt *a, const MenaiInt *b, MenaiInt *result);

/*
 * result = floor(a / b), rounding toward negative infinity.
 * Matches Python's // operator semantics.
 * Returns -1 with ZeroDivisionError set if b is zero.
 */
int menai_int_floordiv(const MenaiInt *a, const MenaiInt *b, MenaiInt *result);

/*
 * result = a mod b, with result having the same sign as b (or zero).
 * Matches Python's % operator semantics.
 * Returns -1 with ZeroDivisionError set if b is zero.
 */
int menai_int_mod(const MenaiInt *a, const MenaiInt *b, MenaiInt *result);

/*
 * Compute floor-division and modulo simultaneously.
 * *quotient = floor(a / b), *remainder = a mod b.
 * quotient and remainder must not alias each other or either input.
 * Returns -1 with ZeroDivisionError set if b is zero.
 */
int menai_int_divmod(
    const MenaiInt *a,
    const MenaiInt *b,
    MenaiInt *quotient,
    MenaiInt *remainder
);

/* result = -a */
int menai_int_neg(const MenaiInt *a, MenaiInt *result);

/* result = |a| */
int menai_int_abs(const MenaiInt *a, MenaiInt *result);

/*
 * result = a ** exp (exp must be non-negative).
 * Returns -1 with ValueError set if exp is negative.
 */
int menai_int_pow(const MenaiInt *a, const MenaiInt *exp, MenaiInt *result);

/*
 * Bitwise operations (two's complement, infinite precision).
 * All write their result to *result, which may alias *a and/or *b.
 * Return 0 on success, -1 on error.
 */

/* result = a & b */
int menai_int_and(const MenaiInt *a, const MenaiInt *b, MenaiInt *result);

/* result = a | b */
int menai_int_or(const MenaiInt *a, const MenaiInt *b, MenaiInt *result);

/* result = a ^ b */
int menai_int_xor(const MenaiInt *a, const MenaiInt *b, MenaiInt *result);

/*
 * result = ~a (bitwise NOT in two's complement infinite precision).
 * Equivalent to -(a + 1).
 */
int menai_int_not(const MenaiInt *a, MenaiInt *result);

/*
 * result = a << shift (left shift by shift bits, shift >= 0).
 */
int menai_int_shift_left(const MenaiInt *a, Py_ssize_t shift, MenaiInt *result);

/*
 * result = a >> shift (arithmetic right shift, sign-extending, shift >= 0).
 * Equivalent to floor(a / 2^shift).
 */
int menai_int_shift_right(const MenaiInt *a, Py_ssize_t shift, MenaiInt *result);

/*
 * Comparison operations.
 * Return 1 if the relation holds, 0 if not. Never fail.
 */

/* Return 1 if a == b, 0 otherwise. */
int menai_int_eq(const MenaiInt *a, const MenaiInt *b);

/* Return 1 if a != b, 0 otherwise. */
int menai_int_ne(const MenaiInt *a, const MenaiInt *b);

/* Return 1 if a < b, 0 otherwise. */
int menai_int_lt(const MenaiInt *a, const MenaiInt *b);

/* Return 1 if a > b, 0 otherwise. */
int menai_int_gt(const MenaiInt *a, const MenaiInt *b);

/* Return 1 if a <= b, 0 otherwise. */
int menai_int_le(const MenaiInt *a, const MenaiInt *b);

/* Return 1 if a >= b, 0 otherwise. */
int menai_int_ge(const MenaiInt *a, const MenaiInt *b);

#endif /* MENAI_VM_BIGINT_H */
