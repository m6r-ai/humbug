/*
 * menai_vm_set.h — MenaiSet type definition and API.
 *
 * MenaiSet stores an ordered, deduplicated sequence of elements as two
 * parallel C arrays (elements, hashes) plus a pure-C MenaiHashTable for O(1)
 * membership testing.  Hash values are computed once at construction time via
 * menai_value_hash() and stored in hashes[], so set operations never
 * recompute them.
 *
 * Invariants:
 *   - elements[i] is an owned reference.
 *   - hashes[i] == menai_value_hash(elements[i]), computed once at construction.
 *   - ht maps elements[i] (by value equality) to index i.
 *   - No duplicate elements: all elements[i] are distinct by menai_value_equal.
 *   - Insertion order is preserved by the array indices.
 */

#ifndef MENAI_VM_SET_H
#define MENAI_VM_SET_H

#include "menai_vm_value.h"
#include "menai_vm_hashtable.h"

typedef struct {
    MenaiValue_HEAD
    MenaiValue **elements;   /* C array of owned MenaiValue *s */
    Py_hash_t *hashes;       /* C array of menai_value_hash(elements[i]) */
    MenaiHashTable ht;       /* pure-C hash table for O(1) membership */
    Py_ssize_t length;
} MenaiSet;

MenaiValue *menai_set_new_empty(void);
MenaiValue *menai_set_from_arrays_steal(MenaiValue **elements, Py_hash_t *hashes, Py_ssize_t n);

#endif /* MENAI_VM_SET_H */
