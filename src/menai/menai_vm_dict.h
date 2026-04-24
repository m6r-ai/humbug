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

typedef struct {
    MenaiValue_HEAD
    MenaiValue **keys;       /* C array of owned MenaiValue *s */
    MenaiValue **values;     /* C array of owned MenaiValue *s */
    Py_hash_t *hashes;       /* C array of menai_value_hash(keys[i]) */
    MenaiHashTable ht;       /* pure-C hash table for O(1) key lookup */
    ssize_t length;
} MenaiDict;

MenaiValue *menai_dict_new_empty(void);
MenaiValue *menai_dict_from_arrays_steal(MenaiValue **keys, MenaiValue **values, Py_hash_t *hashes, ssize_t n);
MenaiValue *menai_dict_new_empty(void);

#endif /* MENAI_VM_DICT_H */
