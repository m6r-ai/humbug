/*
 * menai_vm_integer.c — MenaiInteger type implementation.
 *
 * Three-tier representation: small integer cache for [-5, 256], inline long
 * for values that fit in a C long, and MenaiInt bignum for everything else.
 * The Python C API is only used at the boundary (menai_int_from_pylong /
 * menai_int_to_pylong in menai_vm_bigint.c).
 */

#include <stdlib.h>
#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <string.h>

#include "menai_vm_integer.h"

static MenaiValue _integer_cache[MENAI_INT_CACHE_SIZE];

static void
MenaiInteger_dealloc(MenaiValue self)
{
    MenaiInteger_Object *obj = (MenaiInteger_Object *)self;
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
        menai_int_free(&obj->big);
    }

    free(self);
}

PyTypeObject MenaiInteger_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiInteger",          /* tp_name */
    sizeof(MenaiInteger_Object),   /* tp_basicsize */
    0,                             /* tp_itemsize */
    (destructor)MenaiInteger_dealloc, /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue
menai_integer_from_long(long n)
{
    if (n >= MENAI_INT_CACHE_MIN && n <= MENAI_INT_CACHE_MAX) {
        MenaiValue cached = _integer_cache[n - MENAI_INT_CACHE_MIN];
        menai_retain(cached);
        return cached;
    }

    MenaiInteger_Object *r = (MenaiInteger_Object *)malloc(sizeof(MenaiInteger_Object));
    if (r == NULL) {
        return NULL;
    }

    r->ob_refcnt = 1;
    r->ob_type = &MenaiInteger_Type;
    r->ob_destructor = MenaiInteger_dealloc;
    r->is_big = 0;
    r->small = n;
    menai_int_init(&r->big);

    return (MenaiValue)r;
}

MenaiValue
menai_integer_from_bigint(MenaiInt src)
{
    /*
     * If the value fits in a long, demote to small representation so the
     * inline fast path is used for subsequent operations.
     */
    if (menai_int_fits_long(&src)) {
        long v;
        if (menai_int_to_long(&src, &v) < 0) {
            menai_int_free(&src);
            return NULL;
        }

        menai_int_free(&src);
        return menai_integer_from_long(v);
    }

    MenaiInteger_Object *r = (MenaiInteger_Object *)malloc(sizeof(MenaiInteger_Object));
    if (r == NULL) {
        menai_int_free(&src);
        return NULL;
    }

    r->ob_refcnt = 1;
    r->ob_type = &MenaiInteger_Type;
    r->ob_destructor = MenaiInteger_dealloc;
    r->is_big = 1;
    r->small = 0;
    r->big = src; /* transfer ownership */

    return (MenaiValue)r;
}

int
menai_vm_integer_init(void)
{
    if (PyType_Ready(&MenaiInteger_Type) < 0) {
        return -1;
    }

    for (long v = MENAI_INT_CACHE_MIN; v <= MENAI_INT_CACHE_MAX; v++) {
        MenaiInteger_Object *obj = (MenaiInteger_Object *)malloc(sizeof(MenaiInteger_Object));
        if (obj == NULL) {
            return -1;
        }

        obj->ob_refcnt = 1;
        obj->ob_type = &MenaiInteger_Type;
        obj->ob_destructor = MenaiInteger_dealloc;
        obj->is_big = 0;
        obj->small = v;
        menai_int_init(&obj->big);

        _integer_cache[v - MENAI_INT_CACHE_MIN] = (MenaiValue)obj;
    }

    return 0;
}
