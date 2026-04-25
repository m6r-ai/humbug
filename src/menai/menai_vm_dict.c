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
#include <string.h>

#include "menai_vm_c.h"

MenaiValue *
menai_dict_from_arrays_steal(MenaiValue **keys, MenaiValue **values, hash_t *hashes, ssize_t n)
{
    MenaiDict *obj = (MenaiDict *)menai_alloc(sizeof(MenaiDict));
    if (!obj) {
        _dict_free_arrays(keys, values, hashes, n);
        return NULL;
    }

    obj->ob_refcnt = 1;
    obj->ob_type = MENAITYPE_DICT;

    if (menai_ht_build(&obj->ht, keys, hashes, n) < 0) {
        _dict_free_arrays(keys, values, hashes, n);
        menai_free(obj);
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
    obj->keys = NULL;
    obj->values = NULL;
    obj->hashes = NULL;
    obj->ht.slots = NULL;
    obj->ht.slot_count = 0;
    obj->ht.used = 0;
    obj->length = 0;

    return (MenaiValue *)obj;
}
