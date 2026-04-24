/*
 * menai_vm_set.h — MenaiSet type definition and API.
 *
 * MenaiSet stores an ordered, deduplicated sequence of elements as two
 * parallel arrays (elements, hashes) inlined into the same allocation as the
 * struct itself, using a C99 flexible array member.  The inline_data FAM
 * holds elements[0..capacity-1] followed by hashes[0..capacity-1]; the
 * elements and hashes pointers point into inline_data at
 * construction time.  The MenaiHashTable (ht) remains a separate allocation.
 *
 * length records the number of live elements.  Operations that do not know
 * the final count upfront (set-union, set-intersection, set-difference,
 * list->set) allocate at worst-case capacity; the unused tail of inline_data
 * is harmless dead space for the lifetime of the set.
 *
 * Invariants:
 *   - elements[i] is an owned reference for i in [0, length).
 *   - hashes[i] == menai_value_hash(elements[i]), computed once at construction.
 *   - ht maps elements[i] (by value equality) to index i, for i in [0, length).
 *   - No duplicate elements: all elements[i] are distinct by menai_value_equal.
 *   - Insertion order is preserved by the array indices.
 */
#ifndef MENAI_VM_SET_H
#define MENAI_VM_SET_H

typedef struct {
    MenaiValue_HEAD
    MenaiValue **elements;   /* points into inline_data[0..length-1] */
    hash_t *hashes;          /* points into inline_data past the elements */
    MenaiHashTable ht;       /* pure-C hash table for O(1) membership; separate allocation */
    ssize_t length;          /* number of live elements */
    MenaiValue *inline_data[]; /* FAM: elements[0..cap-1] then hashes[0..cap-1] */
} MenaiSet;

MenaiValue *menai_set_alloc(ssize_t cap);
MenaiValue *menai_set_new_empty(void);

#endif /* MENAI_VM_SET_H */
