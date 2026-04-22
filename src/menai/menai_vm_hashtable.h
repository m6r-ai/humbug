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

#include "menai_vm_object.h"

/* ---------------------------------------------------------------------------
 * Value hash and equality
 * ------------------------------------------------------------------------- */

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
 * menai_value_hash — compute a Py_hash_t for a MenaiValue *.
 *
 * Never allocates Python objects.  Returns -1 only on genuine error
 * (MenaiEvalError set), following the CPython convention that -1 means
 * "unhashable or error".
 *
 * Supported types: none, boolean, integer, float, complex, string, symbol,
 * struct, structtype.  Lists, dicts, sets, and functions are not hashable
 * (returns -1 with MenaiEvalError set).
 */
Py_hash_t menai_value_hash(MenaiValue *val);

/*
 * menai_value_equal — structural equality for two MenaiValue *s.
 *
 * Returns 1 if equal, 0 if not equal.  Never fails, never returns -1.
 * All Menai value types are comparable by value without Python API calls.
 */
int menai_value_equal(MenaiValue *a, MenaiValue *b);

/*
 * menai_value_describe — return a new Python unicode string describing val.
 *
 * Dispatches directly to the C-level describe function for each value type,
 * bypassing Python method dispatch.  Returns a new reference, or NULL on
 * error.
 */
PyObject *menai_value_describe(MenaiValue *val);

/*
 * menai_value_to_python — convert val to the nearest Python equivalent.
 *
 * Dispatches directly to the C-level to_python function for each value type,
 * bypassing Python method dispatch.  Returns a new reference, or NULL on
 * error.
 */
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

/*
 * menai_ht_init — initialise a MenaiHashTable for n entries.
 *
 * Allocates slot_count slots where slot_count is the smallest power of 2
 * satisfying slot_count * MENAI_HT_MAX_LOAD_NUM / MENAI_HT_MAX_LOAD_DEN >= n.
 * All slots are zeroed (key = NULL).
 *
 * Returns 0 on success, -1 on MemoryError.
 */
int menai_ht_init(MenaiHashTable *ht, Py_ssize_t n);

/*
 * menai_ht_free — release the slot array.
 *
 * Does not release any keys — the owning dict/set manages key lifetimes
 * through its element arrays.
 */
void menai_ht_free(MenaiHashTable *ht);

/*
 * menai_ht_lookup — find the index stored for key.
 *
 * hash must equal menai_value_hash(key).
 * Returns the stored index (>= 0) if found, -1 if not found.
 */
Py_ssize_t menai_ht_lookup(const MenaiHashTable *ht, MenaiValue *key, Py_hash_t hash);

/*
 * menai_ht_insert — insert a key/index pair into the table.
 *
 * hash must equal menai_value_hash(key).  The caller must guarantee that
 * key is not already present and that the table has sufficient capacity
 * (i.e. menai_ht_init was called with the correct n).
 * The key pointer is stored as a borrowed reference — the owning
 * dict/set's element arrays keep it alive.
 */
void menai_ht_insert(MenaiHashTable *ht, MenaiValue *key, Py_hash_t hash, Py_ssize_t index);

/*
 * menai_ht_build — build a hash table from parallel key and hash arrays.
 *
 * Equivalent to calling menai_ht_init(ht, n) followed by n calls to
 * menai_ht_insert.  keys[i] and hashes[i] must satisfy
 * hashes[i] == menai_value_hash(keys[i]) and all keys must be distinct.
 *
 * Returns 0 on success, -1 on MemoryError.
 */
int menai_ht_build(MenaiHashTable *ht, MenaiValue **keys, const Py_hash_t *hashes, Py_ssize_t n);

#endif /* MENAI_VM_HASHTABLE_H */
