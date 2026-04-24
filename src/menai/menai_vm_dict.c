/*
 * menai_vm_dict.c — MenaiDict type implementation.
 *
 * MenaiDict stores an ordered sequence of key-value entries as three parallel
 * C arrays (keys, values, hashes) plus a pure-C MenaiHashTable for O(1) index
 * lookup.  Hash values are computed once at construction time via
 * menai_value_hash() and stored in hashes[], so no Python objects are
 * allocated during dict operations.
 *
 * Primary construction path for VM operations: menai_dict_from_arrays_steal.
 * menai_dict_new_empty() creates the empty-dict singleton.
 */
#include <stdlib.h>

#include "menai_vm_alloc.h"
#include "menai_vm_memory.h"
#include "menai_vm_hashtable.h"
#include "menai_vm_value.h"

#include "menai_vm_dict.h"

/*
 * _dict_free_arrays — release n owned references in keys and values, then
 * free all three arrays.  NULL pointers are safely ignored.
 */
static void
_dict_free_arrays(MenaiValue **keys, MenaiValue **values, Py_hash_t *hashes, ssize_t n)
{
    if (keys) {
        for (ssize_t i = 0; i < n; i++) {
            menai_xrelease(keys[i]);
        }

        free(keys);
    }

    if (values) {
        for (ssize_t i = 0; i < n; i++) {
            menai_xrelease(values[i]);
        }

        free(values);
    }

    free(hashes);
}

static void
MenaiDict_dealloc(MenaiValue *self)
{
    MenaiDict *d = (MenaiDict *)self;
    _dict_free_arrays(d->keys, d->values, d->hashes, d->length);
    menai_ht_free(&d->ht);
    menai_free(self, sizeof(MenaiDict));
}

MenaiValue *
menai_dict_from_arrays_steal(MenaiValue **keys, MenaiValue **values, Py_hash_t *hashes, ssize_t n)
{
    MenaiDict *obj = (MenaiDict *)menai_alloc(sizeof(MenaiDict));
    if (!obj) {
        _dict_free_arrays(keys, values, hashes, n);
        return NULL;
    }

    obj->ob_refcnt = 1;
    obj->ob_type = MENAITYPE_DICT;
    obj->ob_destructor = MenaiDict_dealloc;

    if (menai_ht_build(&obj->ht, keys, hashes, n) < 0) {
        _dict_free_arrays(keys, values, hashes, n);
        menai_free(obj, sizeof(MenaiDict));
        return NULL;
    }

    obj->keys = keys;
    obj->values = values;
    obj->hashes = hashes;
    obj->length = n;

    return (MenaiValue *)obj;
}

MenaiValue *
menai_dict_new_empty(void)
{
    MenaiDict *obj = (MenaiDict *)menai_alloc(sizeof(MenaiDict));
    if (!obj) {
        return NULL;
    }

    obj->ob_refcnt = 1;
    obj->ob_type = MENAITYPE_DICT;
    obj->ob_destructor = MenaiDict_dealloc;
    obj->keys = NULL;
    obj->values = NULL;
    obj->hashes = NULL;
    obj->ht.slots = NULL;
    obj->ht.slot_count = 0;
    obj->ht.used = 0;
    obj->length = 0;

    return (MenaiValue *)obj;
}
