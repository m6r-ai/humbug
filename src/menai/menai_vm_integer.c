/*
 * menai_vm_integer.c — MenaiInteger type implementation.
 *
 * Three-tier representation: small integer cache for [-5, 256], inline long
 * for values that fit in a C long, and MenaiBigInt bignum for everything else.
 * The Python C API is only used at the boundary (menai_bigint_from_pylong /
 * menai_bigint_to_pylong in menai_vm_bigint.c).
 */
#include <stdlib.h>
#include <string.h>

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_alloc.h"
#include "menai_vm_value.h"
#include "menai_vm_bigint.h"

#include "menai_vm_integer.h"

static MenaiValue *_integer_cache[MENAI_INT_CACHE_SIZE];

static void
MenaiInteger_dealloc(MenaiValue *self)
{
    MenaiInteger *obj = (MenaiInteger *)self;
    if (!obj->is_big) {
        long v = obj->small;
        if (v >= MENAI_INT_CACHE_MIN && v <= MENAI_INT_CACHE_MAX) {
            /*
             * Cached singleton — must never be freed.  Restore refcount so
             * the object remains live.
             */
            obj->ob_refcnt = 1;
            return;
        }
    } else {
        menai_bigint_free(&obj->big);
    }

    menai_free(self);
}

MenaiValue *
menai_integer_from_long(long n)
{
    if (n >= MENAI_INT_CACHE_MIN && n <= MENAI_INT_CACHE_MAX) {
        MenaiValue *cached = _integer_cache[n - MENAI_INT_CACHE_MIN];
        menai_retain(cached);
        return cached;
    }

    MenaiInteger *r = (MenaiInteger *)menai_alloc(sizeof(MenaiInteger));
    if (r == NULL) {
        return NULL;
    }

    r->ob_refcnt = 1;
    r->ob_type = MENAITYPE_INTEGER;
    r->ob_destructor = MenaiInteger_dealloc;
    r->is_big = 0;
    r->small = n;
    menai_bigint_init(&r->big);

    return (MenaiValue *)r;
}

MenaiValue *
menai_integer_from_bigint(MenaiBigInt src)
{
    /*
     * If the value fits in a long, demote to small representation so the
     * inline fast path is used for subsequent operations.
     */
    if (menai_bigint_fits_long(&src)) {
        long v;
        if (menai_bigint_to_long(&src, &v) < 0) {
            menai_bigint_free(&src);
            return NULL;
        }

        menai_bigint_free(&src);
        return menai_integer_from_long(v);
    }

    MenaiInteger *r = (MenaiInteger *)menai_alloc(sizeof(MenaiInteger));
    if (r == NULL) {
        menai_bigint_free(&src);
        return NULL;
    }

    r->ob_refcnt = 1;
    r->ob_type = MENAITYPE_INTEGER;
    r->ob_destructor = MenaiInteger_dealloc;
    r->is_big = 1;
    r->small = 0;
    r->big = src; /* transfer ownership */

    return (MenaiValue *)r;
}

int
menai_vm_integer_init(void)
{
    for (long v = MENAI_INT_CACHE_MIN; v <= MENAI_INT_CACHE_MAX; v++) {
        MenaiInteger *obj = (MenaiInteger *)menai_alloc(sizeof(MenaiInteger));
        if (obj == NULL) {
            return -1;
        }

        obj->ob_refcnt = 1;
    	obj->ob_type = MENAITYPE_INTEGER;
        obj->ob_destructor = MenaiInteger_dealloc;
        obj->is_big = 0;
        obj->small = v;
        menai_bigint_init(&obj->big);

        _integer_cache[v - MENAI_INT_CACHE_MIN] = (MenaiValue *)obj;
    }

    return 0;
}
