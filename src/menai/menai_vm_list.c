/*
 * menai_vm_list.c — MenaiList type implementation.
 *
 * MenaiList stores a C array of MenaiValue elements.  A two-level free-list
 * cache (one for object structs, one for element arrays bucketed by power-of-2
 * size) reduces allocation pressure in the hot VM loop.
 *
 * Also provides the three C-level constructors used by the VM:
 *   menai_list_from_array        — copy items, retain each
 *   menai_list_from_array_steal  — take ownership, no retain
 */

#include <stdlib.h>
#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <string.h>
#ifdef _MSC_VER
#include <intrin.h>
#endif

#include "menai_vm_list.h"
#include "menai_vm_memory.h"
#include "menai_vm_hashtable.h"

/* ---------------------------------------------------------------------------
 * Free-list cache
 * ------------------------------------------------------------------------- */

#define LIST_CACHE_NUM_BUCKETS 8
#define LIST_CACHE_MAX_BUCKET 256
#define LIST_CACHE_MAX_SIZE 128

static Py_ssize_t _list_size_classes[LIST_CACHE_NUM_BUCKETS] = {
    1, 2, 4, 8, 16, 32, 64, 128
};

/* Object free list — singly-linked via the elements pointer overlay */
static MenaiList_Object *_list_obj_free = NULL;

/* Element array cache — power-of-2 size buckets */
static MenaiValue **_list_arr_buckets[LIST_CACHE_NUM_BUCKETS];
static int _list_arr_counts[LIST_CACHE_NUM_BUCKETS];

static inline int
_bucket_index(Py_ssize_t n)
{
    if (n <= 1) return 0;

#if defined(_MSC_VER)
    unsigned long idx;
    _BitScanReverse(&idx, (unsigned long)(n - 1));
    int bucket = (int)(idx + 1);
#elif defined(__GNUC__) || defined(__clang__)
    int bucket = (int)(sizeof(unsigned long) * 8) - __builtin_clzl((unsigned long)(n - 1));
#else
    int bucket = 0;
    unsigned long v = (unsigned long)(n - 1);
    while (v >>= 1) bucket++;
    bucket++;
#endif
    return bucket < LIST_CACHE_NUM_BUCKETS ? bucket : LIST_CACHE_NUM_BUCKETS - 1;
}

static MenaiList_Object *
_menai_list_cache_alloc_obj(void)
{
    if (_list_obj_free) {
        MenaiList_Object *obj = _list_obj_free;
        _list_obj_free = (MenaiList_Object *)obj->elements;
        obj->elements = NULL;
        obj->length = 0;
        obj->owner = NULL;
        obj->ob_refcnt = 1;
        return obj;
    }

    MenaiList_Object *obj = (MenaiList_Object *)malloc(sizeof(MenaiList_Object));
    if (obj == NULL) return NULL;

    obj->ob_refcnt = 1;
    obj->ob_type = &MenaiList_Type;
    obj->elements = NULL;
    obj->length = 0;
    obj->owner = NULL;

    return obj;
}

static void
_menai_list_cache_free_obj(MenaiList_Object *obj)
{
    obj->elements = (MenaiValue *)_list_obj_free;
    obj->owner = NULL;
    obj->length = 0;
    _list_obj_free = obj;
}

static MenaiValue *
_menai_list_cache_alloc_arr(Py_ssize_t n)
{
    if (n > 0 && n <= LIST_CACHE_MAX_SIZE) {
        int bucket = _bucket_index(n);
        if (_list_arr_counts[bucket] > 0) {
            return _list_arr_buckets[bucket][--_list_arr_counts[bucket]];
        }

        /* No cached entry — allocate at the bucket's full size class so
         * it can be safely recycled into this bucket later. */
        n = _list_size_classes[bucket];
    }

    return (MenaiValue *)malloc((size_t)n * sizeof(MenaiValue));
}

static void
_menai_list_cache_free_arr(MenaiValue *arr, Py_ssize_t n)
{
    for (Py_ssize_t i = 0; i < n; i++) menai_release(arr[i]);
    if (arr && n > 0 && n <= LIST_CACHE_MAX_SIZE) {
        int bucket = _bucket_index(n);
        if (_list_arr_counts[bucket] < LIST_CACHE_MAX_BUCKET) {
            if (_list_arr_counts[bucket] == 0) {
                _list_arr_buckets[bucket] = (MenaiValue **)malloc(
                    LIST_CACHE_MAX_BUCKET * sizeof(MenaiValue *));
                if (!_list_arr_buckets[bucket]) {
                    free(arr);
                    return;
                }
            }

            _list_arr_buckets[bucket][_list_arr_counts[bucket]++] = arr;
            return;
        }
    }
    free(arr);
}

static void
_menai_list_cache_clear(void)
{
    MenaiList_Object *obj = _list_obj_free;
    while (obj) {
        MenaiList_Object *next = (MenaiList_Object *)obj->elements;
        free(obj);
        obj = next;
    }

    _list_obj_free = NULL;

    for (int i = 0; i < LIST_CACHE_NUM_BUCKETS; i++) {
        for (int j = 0; j < _list_arr_counts[i]; j++)
            free(_list_arr_buckets[i][j]);

        if (_list_arr_buckets[i]) {
            free(_list_arr_buckets[i]);
            _list_arr_buckets[i] = NULL;
        }

        _list_arr_counts[i] = 0;
    }
}

/* ---------------------------------------------------------------------------
 * Type implementation
 * ------------------------------------------------------------------------- */

static void
MenaiList_dealloc(PyObject *self)
{
    MenaiList_Object *lst = (MenaiList_Object *)self;
    if (lst->owner != NULL) {
        /* View — release the backing list; do not touch the element array. */
        MenaiValue owner = lst->owner;
        lst->owner = NULL;
        lst->elements = NULL;
        lst->length = 0;
        menai_release(owner);
    } else {
        /* Owner — free the element array. */
        Py_ssize_t n = lst->length;
        lst->length = 0;
        MenaiValue *arr = lst->elements;
        lst->elements = NULL;
        _menai_list_cache_free_arr(arr, n);
    }
    _menai_list_cache_free_obj(lst);
}

PyTypeObject MenaiList_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiList",          /* tp_name */
    sizeof(MenaiList_Object),   /* tp_basicsize */
    0,                             /* tp_itemsize */
    MenaiList_dealloc,                  /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue
menai_list_from_array(MenaiValue *items, Py_ssize_t n)
{
    MenaiValue *arr = NULL;
    if (n > 0) {
        arr = _menai_list_cache_alloc_arr(n);
        if (!arr) return NULL;

        for (Py_ssize_t i = 0; i < n; i++) {
            arr[i] = items[i];
            menai_retain(arr[i]);
        }
    }

    MenaiList_Object *obj = _menai_list_cache_alloc_obj();
    if (!obj) {
        _menai_list_cache_free_arr(arr, n);
        return NULL;
    }
    obj->elements = arr;
    obj->length = n;

    return (MenaiValue)obj;
}

MenaiValue
menai_list_from_array_steal(MenaiValue *items, Py_ssize_t n)
{
    MenaiList_Object *obj = _menai_list_cache_alloc_obj();
    if (!obj) {
        /* Free the stolen array and its references on failure. */
        for (Py_ssize_t i = 0; i < n; i++) menai_release(items[i]);
        free(items);
        return NULL;
    }
    obj->elements = items;
    obj->length = n;

    return (MenaiValue)obj;
}

MenaiValue
menai_list_new_empty(void)
{
    MenaiList_Object *obj = _menai_list_cache_alloc_obj();
    if (!obj) return NULL;

    obj->elements = NULL;
    obj->length = 0;

    return (MenaiValue)obj;
}

MenaiValue
menai_list_rest(MenaiValue lst_val)
{
    MenaiList_Object *lst = (MenaiList_Object *)lst_val;
    if (lst->length == 0) {
        /* Error reporting still goes through Python exceptions for now. */
        PyErr_SetString(PyExc_RuntimeError,
            "Function 'list-rest' requires a non-empty list");
        return NULL;
    }

    /*
     * Resolve the owner: if lst is itself a view, use its owner so we never
     * build a chain — all views point directly at the root array owner.
     */
    MenaiValue owner = (lst->owner != NULL) ? lst->owner : lst_val;

    MenaiList_Object *view = _menai_list_cache_alloc_obj();
    if (view == NULL) return NULL;

    menai_retain(owner);
    view->owner = owner;
    view->elements = lst->elements + 1;
    view->length = lst->length - 1;

    return (MenaiValue)view;
}

MenaiValue
menai_list_slice(MenaiValue lst_val, Py_ssize_t start, Py_ssize_t end)
{
    MenaiList_Object *lst = (MenaiList_Object *)lst_val;

    /*
     * Resolve the owner: if lst is itself a view, point at its owner so
     * all views are depth-1 from the root array owner.
     */
    MenaiValue owner = (lst->owner != NULL) ? lst->owner : lst_val;

    MenaiList_Object *view = _menai_list_cache_alloc_obj();
    if (view == NULL) return NULL;

    menai_retain(owner);
    view->owner = owner;
    view->elements = lst->elements + start;
    view->length = end - start;

    return (MenaiValue)view;
}

int
menai_vm_list_init(void)
{
    if (PyType_Ready(&MenaiList_Type) < 0) return -1;
    _menai_list_cache_clear();
    return 0;
}
