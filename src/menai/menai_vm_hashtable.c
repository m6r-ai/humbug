/*
 * menai_vm_hashtable.c — pure-C hash table and value operations.
 *
 * MenaiHashTable is an open-addressing table with power-of-2 slot counts and
 * a 2/3 maximum load factor.  Probing uses the same quadratic-ish sequence
 * CPython uses: slot = (5*slot + 1 + perturb) & mask, perturb >>= 5.
 * Tables are built once and never mutated (Menai collections are immutable),
 * so there is no deletion or rehashing logic.
 */
#define _POSIX_C_SOURCE 200809L
#include <limits.h>
#include <stdlib.h>
#include <string.h>

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_alloc.h"
#include "menai_vm_value.h"
#include "menai_vm_hashtable.h"
#include "menai_vm_code.h"
#include "menai_vm_bigint.h"
#include "menai_vm_boolean.h"
#include "menai_vm_complex.h"
#include "menai_vm_float.h"
#include "menai_vm_integer.h"
#include "menai_vm_none.h"
#include "menai_vm_string.h"
#include "menai_vm_struct.h"
#include "menai_vm_symbol.h"
#include "menai_vm_list.h"
#include "menai_vm_dict.h"
#include "menai_vm_set.h"
#include "menai_vm_function.h"

/* Defined in menai_vm_value.c, */
extern PyObject *MenaiEvalError_type;

static inline uhash_t
_hash_combine(uhash_t acc, hash_t h)
{
    return acc * 1000003UL ^ (uhash_t)h;
}

static inline hash_t
_hash_finalise(uhash_t acc, ssize_t n)
{
    acc ^= (uhash_t)n;
    return (hash_t)(acc == (uhash_t)-1 ? (uhash_t)-2 : acc);
}

hash_t
menai_value_hash(MenaiValue *val)
{
    MenaiType t = val->ob_type;

    switch (t) {
    case MENAITYPE_NONE:
        return (hash_t)0x4e6f6e65UL;

    case MENAITYPE_BOOLEAN:
        return (hash_t)((MenaiBoolean *)val)->value;

    case MENAITYPE_INTEGER: {
        MenaiInteger *obj = (MenaiInteger *)val;
        if (!obj->is_big) {
            hash_t h = (hash_t)obj->small;
            return h == -1 ? -2 : h;
        }

        return menai_bigint_hash(&obj->big);
    }

    case MENAITYPE_FLOAT:
        return menai_hash_double(((MenaiFloat *)val)->value);

    case MENAITYPE_COMPLEX: {
        MenaiComplex *c = (MenaiComplex *)val;
        hash_t hr = menai_hash_double(c->real);
        hash_t hi = menai_hash_double(c->imag);
        uhash_t acc = (uhash_t)hr * 1000003UL ^ (uhash_t)hi;
        hash_t h = (hash_t)(acc & (uhash_t)SSIZE_MAX);
        return h == -1 ? -2 : h;
    }

    case MENAITYPE_STRING:
        return menai_string_hash(val);

    case MENAITYPE_SYMBOL:
        return menai_string_hash(((MenaiSymbol *)val)->name);

    case MENAITYPE_STRUCTTYPE:
        return (hash_t)((MenaiStructType *)val)->tag;

    case MENAITYPE_STRUCT: {
        MenaiStruct *s = (MenaiStruct *)val;
        int tag = ((MenaiStructType *)s->struct_type)->tag;
        int n = s->nfields;
        uhash_t acc = 0x345678UL ^ (uhash_t)tag;
        for (int i = 0; i < n; i++) {
            hash_t fh = menai_value_hash(s->items[i]);
            if (fh == -1) {
                return -1;
            }

            acc = _hash_combine(acc, fh);
        }

        return _hash_finalise(acc, n);
    }
    }

    PyErr_Format(MenaiEvalError_type,
        "Dict keys must be strings, numbers, booleans, or symbols, got %s",
        menai_short_type_name(t));
    return -1;
}

int
menai_value_equal(MenaiValue *a, MenaiValue *b)
{
    if (a == b) {
        return 1;
    }

    MenaiType ta = a->ob_type;
    MenaiType tb = b->ob_type;

    if (ta != tb) {
        return 0;
    }

    switch (ta) {
    case MENAITYPE_NONE:
        return 1;

    case MENAITYPE_BOOLEAN:
        return ((MenaiBoolean *)a)->value == ((MenaiBoolean *)b)->value;

    case MENAITYPE_INTEGER: {
        MenaiInteger *ia = (MenaiInteger *)a;
        MenaiInteger *ib = (MenaiInteger *)b;
        if (!ia->is_big && !ib->is_big) {
            return ia->small == ib->small;
        }

        if (ia->is_big != ib->is_big) {
            return 0;
        }

        return menai_bigint_eq(&ia->big, &ib->big);
    }

    case MENAITYPE_FLOAT:
        return ((MenaiFloat *)a)->value == ((MenaiFloat *)b)->value;

    case MENAITYPE_COMPLEX: {
        MenaiComplex *ca = (MenaiComplex *)a;
        MenaiComplex *cb = (MenaiComplex *)b;
        return ca->real == cb->real && ca->imag == cb->imag;
    }

    case MENAITYPE_STRING:
        return menai_string_equal(a, b);

    case MENAITYPE_SYMBOL:
        return menai_string_equal(((MenaiSymbol *)a)->name, ((MenaiSymbol *)b)->name);

    case MENAITYPE_STRUCTTYPE:
        return ((MenaiStructType *)a)->tag == ((MenaiStructType *)b)->tag;

    case MENAITYPE_STRUCT: {
        MenaiStruct *sa = (MenaiStruct *)a;
        MenaiStruct *sb = (MenaiStruct *)b;
        if (((MenaiStructType *)sa->struct_type)->tag != ((MenaiStructType *)sb->struct_type)->tag) {
            return 0;
        }

        int n = sa->nfields;
        if (n != sb->nfields) {
            return 0;
        }

        for (int i = 0; i < n; i++) {
            if (!menai_value_equal(sa->items[i], sb->items[i])) {
                return 0;
            }
        }

        return 1;
    }

    case MENAITYPE_LIST: {
        MenaiList *la = (MenaiList *)a;
        MenaiList *lb = (MenaiList *)b;
        if (la->length != lb->length) {
            return 0;
        }

        for (ssize_t i = 0; i < la->length; i++) {
            if (!menai_value_equal(la->elements[i], lb->elements[i])) {
                return 0;
            }
        }

        return 1;
    }

    case MENAITYPE_DICT: {
        MenaiDict *da = (MenaiDict *)a;
        MenaiDict *db = (MenaiDict *)b;
        if (da->length != db->length) {
            return 0;
        }

        for (ssize_t i = 0; i < da->length; i++) {
            if (da->hashes[i] != db->hashes[i]) {
                return 0;
            }

            if (!menai_value_equal(da->keys[i], db->keys[i])) {
                return 0;
            }

            if (!menai_value_equal(da->values[i], db->values[i])) {
                return 0;
            }
        }

        return 1;
    }

    case MENAITYPE_SET: {
        MenaiSet *sa = (MenaiSet *)a;
        MenaiSet *sb = (MenaiSet *)b;
        if (sa->length != sb->length) {
            return 0;
        }

        for (ssize_t i = 0; i < sa->length; i++) {
            if (menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]) == -1) {
                return 0;
            }
        }

        return 1;
    }
    }

    return 0;
}

static ssize_t
_ht_slot_count(ssize_t n)
{
    if (n == 0) {
        return 0;
    }

    ssize_t min_slots = (n * MENAI_HT_MAX_LOAD_DEN + MENAI_HT_MAX_LOAD_NUM - 1) / MENAI_HT_MAX_LOAD_NUM;
    ssize_t sc = 4;
    while (sc < min_slots) {
        sc <<= 1;
    }

    return sc;
}

int
menai_ht_init(MenaiHashTable *ht, ssize_t n)
{
    ssize_t sc = _ht_slot_count(n);
    if (sc == 0) {
        ht->slots = NULL;
        ht->slot_count = 0;
        ht->used = 0;
        return 0;
    }

    ht->slots = (MenaiHashSlot *)malloc((size_t)sc * sizeof(MenaiHashSlot));
    if (!ht->slots) {
        PyErr_NoMemory();
        return -1;
    }

    memset(ht->slots, 0, (size_t)sc * sizeof(MenaiHashSlot));
    ht->slot_count = sc;
    ht->used = 0;
    return 0;
}

void
menai_ht_free(MenaiHashTable *ht)
{
    free(ht->slots);
    ht->slots = NULL;
    ht->slot_count = 0;
    ht->used = 0;
}

ssize_t
menai_ht_lookup(const MenaiHashTable *ht, MenaiValue *key, hash_t hash)
{
    if (ht->slot_count == 0) {
        return -1;
    }

    ssize_t mask = ht->slot_count - 1;
    uhash_t perturb = (uhash_t)hash;
    ssize_t slot = (ssize_t)(perturb & (uhash_t)mask);

    for (;;) {
        MenaiHashSlot *s = &ht->slots[slot];
        if (s->key == NULL) {
            return -1;
        }

        if (s->hash == hash && menai_value_equal(s->key, key)) {
            return s->index;
        }

        perturb >>= 5;
        slot = (ssize_t)((5 * (uhash_t)slot + 1 + perturb) & (uhash_t)mask);
    }
}

void
menai_ht_insert(MenaiHashTable *ht, MenaiValue *key, hash_t hash, ssize_t index)
{
    ssize_t mask = ht->slot_count - 1;
    uhash_t perturb = (uhash_t)hash;
    ssize_t slot = (ssize_t)(perturb & (uhash_t)mask);

    for (;;) {
        MenaiHashSlot *s = &ht->slots[slot];
        if (s->key == NULL) {
            s->key = key;
            s->hash = hash;
            s->index = index;
            ht->used++;
            return;
        }

        perturb >>= 5;
        slot = (ssize_t)((5 * (uhash_t)slot + 1 + perturb) & (uhash_t)mask);
    }
}

int
menai_ht_build(MenaiHashTable *ht, MenaiValue **keys, const hash_t *hashes, ssize_t n)
{
    if (menai_ht_init(ht, n) < 0) {
        return -1;
    }

    for (ssize_t i = 0; i < n; i++) {
        menai_ht_insert(ht, keys[i], hashes[i], i);
    }

    return 0;
}
