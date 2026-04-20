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

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_hashtable.h"

typedef struct {
    PyObject_HEAD
    PyObject     **elements; /* C array of original MenaiValues */
    Py_hash_t     *hashes;   /* C array of menai_value_hash(elements[i]) */
    MenaiHashTable ht;       /* pure-C hash table for O(1) membership */
    Py_ssize_t     length;
} MenaiSet_Object;

extern PyTypeObject MenaiSet_Type;

/*
 * menai_set_new_empty — create a zero-element MenaiSet.
 * Used by _menai_vm_value_init() to build the Menai_SET_EMPTY singleton.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_set_new_empty(void);

/*
 * menai_set_from_arrays_steal — build a MenaiSet from two parallel C arrays.
 *
 * elements must have n entries; hashes[i] must equal
 * menai_value_hash(elements[i]) for every i.  Both arrays and their contents
 * are stolen: on success the arrays are owned by the new set; on failure the
 * arrays are freed and all contained references are released.
 * The caller must guarantee that all elements are distinct (already
 * deduplicated).
 *
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_set_from_arrays_steal(PyObject **elements, Py_hash_t *hashes,
                                      Py_ssize_t n);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_set_init(void);

#endif /* MENAI_VM_SET_H */
