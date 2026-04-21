/*
 * menai_vm_dict.h — MenaiDict type definition and API.
 *
 * MenaiDict stores an ordered key-value mapping as three parallel C arrays
 * (keys, values, hashes) plus a pure-C MenaiHashTable for O(1) lookup.
 * Hash values are computed once at construction time via menai_value_hash()
 * and stored in hashes[], so dict operations never recompute them.
 *
 * Invariants:
 *   - keys[i] and values[i] are owned references.
 *   - hashes[i] == menai_value_hash(keys[i]), computed once at construction.
 *   - ht maps keys[i] (by value equality) to index i.
 *   - No duplicate keys: all keys[i] are distinct by menai_value_equal.
 *   - Insertion order is preserved by the array indices.
 */

#ifndef MENAI_VM_DICT_H
#define MENAI_VM_DICT_H

#include "menai_vm_object.h"
#include "menai_vm_hashtable.h"

typedef struct {
    MenaiObject_HEAD
    MenaiValue    *keys;     /* C array of owned MenaiValues */
    MenaiValue    *values;   /* C array of owned MenaiValues */
    Py_hash_t     *hashes;   /* C array of menai_value_hash(keys[i]) */
    MenaiHashTable ht;       /* pure-C hash table for O(1) key lookup */
    Py_ssize_t     length;
} MenaiDict_Object;

extern MenaiType MenaiDict_Type;

/*
 * menai_dict_new_empty — create a zero-entry MenaiDict.
 * Used by _menai_vm_value_init() to build the Menai_DICT_EMPTY singleton.
 * Returns a new reference, or NULL on error.
 */
MenaiValue menai_dict_new_empty(void);

/*
 * menai_dict_from_arrays_steal — build a MenaiDict from three parallel C arrays.
 *
 * keys, values, and hashes must each have n entries.  hashes[i] must equal
 * menai_value_hash(keys[i]) for every i.  All three arrays and their contents
 * are stolen: on success they are owned by the new dict; on failure the arrays
 * are freed and all contained references are released.
 * The caller must guarantee that all keys are distinct (already deduplicated).
 *
 * Returns a new reference, or NULL on error.
 */
MenaiValue menai_dict_from_arrays_steal(MenaiValue *keys, MenaiValue *values,
                                        Py_hash_t *hashes, Py_ssize_t n);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure.
 */
int menai_vm_dict_init(void);

#endif /* MENAI_VM_DICT_H */
