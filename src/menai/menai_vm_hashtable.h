/*
 * menai_vm_hashtable.h — pure-C hash table and value operations for
 * MenaiDict, MenaiSet, and the collection types.
 */

#ifndef MENAI_VM_HASHTABLE_H
#define MENAI_VM_HASHTABLE_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <stdint.h>
#include <string.h>

#include "menai_vm_types.h"
#include "menai_vm_value.h"

/*
 * menai_hash_double — hash a C double without any Python API calls.
 *
 * Reinterprets the IEEE 754 bit pattern as a uint64_t via memcpy (safe
 * under strict aliasing rules) then applies a finalisation mix so that
 * nearby values produce well-distributed hashes.  NaN is normalised to a
 * fixed bit pattern before mixing so all NaN values hash identically.
 * The result is mapped away from -1 (the CPython "error" sentinel).
 *
 * This is a Menai-internal hash — it does not need to match Python's
 * float hash, because Menai floats and integers are never equal and are
 * never mixed in the same dict or set.
 */
static inline Py_hash_t
menai_hash_double(double v)
{
    uint64_t bits;
    if (v != v) {
        bits = 0x7FF8000000000000ULL;
    } else {
        memcpy(&bits, &v, sizeof(bits));
    }

    /* Finalisation mix from SplitMix64 */
    bits ^= bits >> 30;
    bits *= 0xbf58476d1ce4e5b9ULL;
    bits ^= bits >> 27;
    bits *= 0x94d049bb133111ebULL;
    bits ^= bits >> 31;
    Py_hash_t h = (Py_hash_t)(bits & (uint64_t)PTRDIFF_MAX);
    return h == -1 ? -2 : h;
}

/*
 * menai_name_str_hash — FNV-1a hash of a UTF-8 C string.
 *
 * Used to precompute hashes for global name strings stored in
 * MenaiCodeObject::name_hashes, and to hash entries when building
 * GlobalsTable slots.  Returns a value in [0, PY_SSIZE_T_MAX]; never -1.
 */
static inline Py_hash_t
menai_name_str_hash(const char *s)
{
    Py_uhash_t h = 14695981039346656037ULL;  /* FNV offset basis */
    const unsigned char *p = (const unsigned char *)s;
    while (*p) {
        h ^= (Py_uhash_t)*p++;
        h *= 1099511628211ULL;              /* FNV prime */
    }

    Py_hash_t r = (Py_hash_t)(h & (Py_uhash_t)PTRDIFF_MAX);
    return r == -1 ? -2 : r;
}

Py_hash_t menai_value_hash(MenaiValue *val);
int menai_value_equal(MenaiValue *a, MenaiValue *b);
PyObject *menai_value_describe(MenaiValue *val);
PyObject *menai_value_to_python(MenaiValue *val);

/* ---------------------------------------------------------------------------
 * MenaiHashTable — open-addressing hash table
 *
 * Maps MenaiValue *keys to Py_ssize_t indices.  Used as the internal
 * acceleration structure for MenaiDict (key -> entry index) and MenaiSet
 * (element -> entry index, for membership testing).
 *
 * Invariants:
 *   - slot_count is always a power of 2 (or 0 for an empty table).
 *   - used <= slot_count * MENAI_HT_MAX_LOAD.
 *   - A slot is empty when its key pointer is NULL.
 *   - Deleted slots are not used (tables are immutable after construction).
 * ------------------------------------------------------------------------- */

#define MENAI_HT_MAX_LOAD_NUM 2   /* load factor numerator   */
#define MENAI_HT_MAX_LOAD_DEN 3   /* load factor denominator */

typedef struct {
    MenaiValue *key;     /* borrowed ref to MenaiValue *; NULL = empty slot */
    Py_hash_t hash;     /* cached hash of key */
    Py_ssize_t index;   /* index into the owning dict/set's element arrays */
} MenaiHashSlot;

typedef struct {
    MenaiHashSlot *slots;
    Py_ssize_t slot_count;  /* power of 2; 0 means uninitialised */
    Py_ssize_t used;
} MenaiHashTable;

int menai_ht_init(MenaiHashTable *ht, Py_ssize_t n);
void menai_ht_free(MenaiHashTable *ht);
Py_ssize_t menai_ht_lookup(const MenaiHashTable *ht, MenaiValue *key, Py_hash_t hash);
void menai_ht_insert(MenaiHashTable *ht, MenaiValue *key, Py_hash_t hash, Py_ssize_t index);
int menai_ht_build(MenaiHashTable *ht, MenaiValue **keys, const Py_hash_t *hashes, Py_ssize_t n);

#endif /* MENAI_VM_HASHTABLE_H */
