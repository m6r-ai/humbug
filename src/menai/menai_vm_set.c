/*
 * menai_vm_set.c — MenaiSet type implementation.
 *
 * MenaiSet stores an ordered, deduplicated sequence of elements as two
 * parallel C arrays (elements, hashes) plus a pure-C MenaiHashTable for O(1)
 * membership testing.  Hash values are computed once at construction time via
 * menai_value_hash() and reused for all subsequent set operations.
 *
 * Primary construction path for VM operations: menai_set_from_arrays_steal.
 * menai_set_new_empty() creates the empty-set singleton.
 */
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "menai_vm_alloc.h"
#include "menai_vm_value.h"
#include "menai_vm_hashtable.h"
#include "menai_vm_memory.h"

#include "menai_vm_set.h"

/*
 * _set_free_arrays — release n owned references in elements, then free
 * both arrays.  NULL pointers are safely ignored.
 */
static void
_set_free_arrays(MenaiValue **elements, hash_t *hashes, ssize_t n)
{
    if (elements) {
        for (ssize_t i = 0; i < n; i++) {
            menai_xrelease(elements[i]);
        }

        free(elements);
    }

    free(hashes);
}

static void
MenaiSet_dealloc(MenaiValue *self)
{
    MenaiSet *s = (MenaiSet *)self;
    _set_free_arrays(s->elements, s->hashes, s->length);
    menai_ht_free(&s->ht);
    menai_free(self, sizeof(MenaiSet));
}

MenaiValue *
menai_set_from_arrays_steal(MenaiValue **elements, hash_t *hashes, ssize_t n)
{
    MenaiSet *obj = (MenaiSet *)menai_alloc(sizeof(MenaiSet));
    if (!obj) {
        _set_free_arrays(elements, hashes, n);
        return NULL;
    }

    obj->ob_refcnt = 1;
    obj->ob_type = MENAITYPE_SET;
    obj->ob_destructor = MenaiSet_dealloc;

    if (menai_ht_build(&obj->ht, elements, hashes, n) < 0) {
        _set_free_arrays(elements, hashes, n);
        menai_free(obj, sizeof(MenaiSet));
        return NULL;
    }

    obj->elements = elements;
    obj->hashes = hashes;
    obj->length = n;

    return (MenaiValue *)obj;
}

MenaiValue *
menai_set_new_empty(void)
{
    MenaiSet *obj = (MenaiSet *)menai_alloc(sizeof(MenaiSet));
    if (!obj) {
        return NULL;
    }

    obj->ob_refcnt = 1;
    obj->ob_type = MENAITYPE_SET;
    obj->ob_destructor = MenaiSet_dealloc;
    obj->elements = NULL;
    obj->hashes = NULL;
    obj->ht.slots = NULL;
    obj->ht.slot_count = 0;
    obj->ht.used = 0;
    obj->length = 0;

    return (MenaiValue *)obj;
}
