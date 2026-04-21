/*
 * menai_vm_integer.h — MenaiInteger type definition and API.
 */

#ifndef MENAI_VM_INTEGER_H
#define MENAI_VM_INTEGER_H

#include "menai_vm_object.h"
#include "menai_vm_bigint.h"

/*
 * Three-tier integer representation:
 *
 *   is_big == 0: value is stored inline as a C long in the small field.
 *                For values in [MENAI_INT_CACHE_MIN, MENAI_INT_CACHE_MAX]
 *                the object is a pre-allocated singleton and must never
 *                be freed.
 *
 *   is_big == 1: value is stored as a MenaiInt bignum in the big field.
 *                The MenaiInt owns its digit array.
 *
 * The ob_type is always &MenaiInteger_Type.
 */
typedef struct {
    MenaiObject_HEAD
    int is_big;
    long small;     /* valid when is_big == 0 */
    MenaiInt big;   /* valid when is_big == 1 */
} MenaiInteger_Object;

extern MenaiType MenaiInteger_Type;

/*
 * Small integer cache — covers [MENAI_INT_CACHE_MIN, MENAI_INT_CACHE_MAX].
 * menai_integer_from_long() returns a retained reference, hitting the
 * cache for in-range values.
 */
#define MENAI_INT_CACHE_MIN (-5)
#define MENAI_INT_CACHE_MAX 256
#define MENAI_INT_CACHE_SIZE (MENAI_INT_CACHE_MAX - MENAI_INT_CACHE_MIN + 1)

/*
 * menai_integer_from_long — return a MenaiInteger for the given long value.
 * Returns a new reference (retain'd from cache for in-range values).
 * Returns NULL on allocation failure.
 */
MenaiValue menai_integer_from_long(long n);

/*
 * menai_integer_from_bigint — return a MenaiInteger that takes ownership of
 * the given MenaiInt.  src must be heap-allocated (not stack) and is consumed:
 * the caller must not call menai_int_free on it after this call.
 * Returns a new reference, or NULL on allocation failure.
 */
MenaiValue menai_integer_from_bigint(MenaiInt src);

/*
 * menai_integer_bigint — return a pointer to the MenaiInt for a big integer.
 * The caller must ensure is_big == 1 before calling.
 */
static inline const MenaiInt *
menai_integer_bigint(MenaiValue o)
{
    return &((MenaiInteger_Object *)o)->big;
}

/*
 * menai_integer_small — return the small value for a non-big integer.
 * The caller must ensure is_big == 0 before calling.
 */
static inline long
menai_integer_small(MenaiValue o)
{
    return ((MenaiInteger_Object *)o)->small;
}

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure.
 */
int menai_vm_integer_init(void);

#endif /* MENAI_VM_INTEGER_H */
