/*
 * menai_vm_integer.h — MenaiInteger type definition and API.
 */
#ifndef MENAI_VM_INTEGER_H
#define MENAI_VM_INTEGER_H

/*
 * Three-tier integer representation:
 *
 *   is_big == 0: value is stored inline as a C long in the small field.
 *                For values in [MENAI_INT_CACHE_MIN, MENAI_INT_CACHE_MAX]
 *                the object is a pre-allocated singleton and must never
 *                be freed.
 *
 *   is_big == 1: value is stored as a MenaiBigInt bignum in the big field.
 *                The MenaiBigInt owns its digit array.
 *
 * The ob_type is always &MenaiInteger_Type.
 */
typedef struct {
    MenaiValue_HEAD
    int is_big;
    long small;     /* valid when is_big == 0 */
    MenaiBigInt big;   /* valid when is_big == 1 */
} MenaiInteger;

/*
 * Small integer cache — covers [MENAI_INT_CACHE_MIN, MENAI_INT_CACHE_MAX].
 * menai_integer_from_long() returns a retained reference, hitting the
 * cache for in-range values.
 */
#define MENAI_INT_CACHE_MIN (-5)
#define MENAI_INT_CACHE_MAX 256
#define MENAI_INT_CACHE_SIZE (MENAI_INT_CACHE_MAX - MENAI_INT_CACHE_MIN + 1)

MenaiValue *menai_integer_from_long(long n);
MenaiValue *menai_integer_from_bigint(MenaiBigInt src);

/*
 * menai_integer_bigint — return a pointer to the MenaiBigInt for a big integer.
 * The caller must ensure is_big == 1 before calling.
 */
static inline const MenaiBigInt *
menai_integer_bigint(MenaiValue *o)
{
    return &((MenaiInteger *)o)->big;
}

/*
 * menai_integer_small — return the small value for a non-big integer.
 * The caller must ensure is_big == 0 before calling.
 */
static inline long
menai_integer_small(MenaiValue *o)
{
    return ((MenaiInteger *)o)->small;
}

int menai_vm_integer_init(void);

#endif /* MENAI_VM_INTEGER_H */
