/*
 * menai_vm_set.c — MenaiSet type implementation.
 *
 * MenaiSet stores its elements and hashes inline in the same allocation as the
 * struct, using a C99 flexible array member.  inline_data holds the elements
 * array followed immediately by the hashes array.  A single menai_alloc call
 * covers the header and both arrays.  The MenaiHashTable (ht) is a separate
 * allocation managed by menai_ht_build/menai_ht_free.
 *
 * The primary constructor is menai_set_alloc(n), which allocates for n
 * elements and returns a set with uninitialised elements ready for the caller
 * to fill.  The caller then sets length and calls menai_ht_build.
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

static void
MenaiSet_dealloc(MenaiValue *self)
{
    MenaiSet *s = (MenaiSet *)self;
    ssize_t n = s->length;
    for (ssize_t i = 0; i < n; i++) {
        menai_release(s->elements[i]);
    }

    menai_ht_free(&s->ht);
    menai_free(self);
}

MenaiValue *
menai_set_alloc(ssize_t cap)
{
    size_t sz = sizeof(MenaiSet) + (size_t)cap * (sizeof(MenaiValue *) + sizeof(hash_t));
    MenaiSet *obj = (MenaiSet *)menai_alloc(sz);
    if (!obj) {
        return NULL;
    }

    obj->ob_refcnt = 1;
    obj->ob_type = MENAITYPE_SET;
    obj->ob_destructor = MenaiSet_dealloc;
    obj->elements = (MenaiValue **)obj->inline_data;
    obj->hashes = (hash_t *)(obj->inline_data + cap);
    obj->length = 0;
    obj->ht.slots = NULL;
    obj->ht.slot_count = 0;
    obj->ht.used = 0;

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
