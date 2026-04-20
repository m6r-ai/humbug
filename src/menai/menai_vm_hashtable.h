/*
 * menai_vm_hashtable.h — pure-C hash table and value hash/equality for
 * MenaiDict and MenaiSet.
 *
 * Provides:
 *   menai_value_hash()    — compute Py_hash_t for any MenaiValue without
 *                           allocating Python objects
 *   menai_value_equal()   — structural equality for any two MenaiValues
 *                           without allocating Python objects
 *   MenaiHashTable        — open-addressing hash table mapping MenaiValue
 *                           keys to Py_ssize_t indices
 *
 * These replace the Python dict (lookup) and Python frozenset (members) that
 * MenaiDict and MenaiSet previously used, removing all Python-object
 * dependencies from the collection hot paths.
 */

#ifndef MENAI_VM_HASHTABLE_H
#define MENAI_VM_HASHTABLE_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

/* ---------------------------------------------------------------------------
 * Value hash and equality
 * ------------------------------------------------------------------------- */

/*
 * menai_value_hash — compute a Py_hash_t for a MenaiValue.
 *
 * Never allocates Python objects.  Returns -1 only on genuine error
 * (MenaiEvalError set), following the CPython convention that -1 means
 * "unhashable or error".
 *
 * Supported types: none, boolean, integer, float, complex, string, symbol,
 * struct, structtype.  Lists, dicts, sets, and functions are not hashable
 * (returns -1 with MenaiEvalError set).
 */
Py_hash_t menai_value_hash(PyObject *val);

/*
 * menai_value_equal — structural equality for two MenaiValues.
 *
 * Returns 1 if equal, 0 if not equal, -1 on error (Python exception set).
 * Never allocates Python objects for primitive types.
 */
int menai_value_equal(PyObject *a, PyObject *b);

/* ---------------------------------------------------------------------------
 * MenaiHashTable — open-addressing hash table
 *
 * Maps MenaiValue keys to Py_ssize_t indices.  Used as the internal
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
    PyObject *key;      /* borrowed ref to MenaiValue; NULL = empty slot */
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
 * Does not DECREF any keys — the owning dict/set manages key lifetimes
 * through its element arrays.
 */
void menai_ht_free(MenaiHashTable *ht);

/*
 * menai_ht_lookup — find the index stored for key.
 *
 * hash must equal menai_value_hash(key).
 * Returns the stored index (>= 0) if found, -1 if not found, -2 on error.
 */
Py_ssize_t menai_ht_lookup(const MenaiHashTable *ht, PyObject *key, Py_hash_t hash);

/*
 * menai_ht_insert — insert a key/index pair into the table.
 *
 * hash must equal menai_value_hash(key).  The caller must guarantee that
 * key is not already present and that the table has sufficient capacity
 * (i.e. menai_ht_init was called with the correct n).
 * The key pointer is stored as a borrowed reference — the owning
 * dict/set's element arrays keep it alive.
 */
void menai_ht_insert(MenaiHashTable *ht, PyObject *key, Py_hash_t hash, Py_ssize_t index);

/*
 * menai_ht_build — build a hash table from parallel key and hash arrays.
 *
 * Equivalent to calling menai_ht_init(ht, n) followed by n calls to
 * menai_ht_insert.  keys[i] and hashes[i] must satisfy
 * hashes[i] == menai_value_hash(keys[i]) and all keys must be distinct.
 *
 * Returns 0 on success, -1 on MemoryError.
 */
int menai_ht_build(MenaiHashTable *ht, PyObject **keys, const Py_hash_t *hashes, Py_ssize_t n);

#endif /* MENAI_VM_HASHTABLE_H */
