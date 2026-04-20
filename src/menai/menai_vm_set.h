/*
 * menai_vm_set.h — MenaiSet type definition and API.
 *
 * MenaiSet stores an ordered, deduplicated sequence of elements as two
 * parallel C arrays (elements, hkeys) plus a Python frozenset for O(1)
 * membership testing.  Canonical hash keys are computed once at construction
 * time and stored in hkeys, so set operations never recompute them.
 *
 * Invariants:
 *   - elements[i] and hkeys[i] are owned references.
 *   - hkeys[i] == menai_hashable_key(elements[i]), computed once at construction.
 *   - members == frozenset(hkeys[i] for i in range(length)).
 *   - No duplicate elements: all hkeys[i] are distinct.
 *   - Insertion order is preserved by the array indices.
 */

#ifndef MENAI_VM_SET_H
#define MENAI_VM_SET_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    PyObject **elements; /* C array of original MenaiValues, length entries */
    PyObject **hkeys;    /* C array of canonical hashable keys, length entries */
    PyObject *members;   /* Python frozenset of hkeys for O(1) membership */
    Py_ssize_t length;
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
 * elements and hkeys must each have n entries.  Both arrays and their
 * contents are stolen: on success the arrays are owned by the new set; on
 * failure the arrays are freed and all contained references are released.
 * The caller must guarantee that hkeys[i] == menai_hashable_key(elements[i])
 * and that all hkeys are distinct (i.e. elements are already deduplicated).
 *
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_set_from_arrays_steal(PyObject **elements, PyObject **hkeys,
                                      Py_ssize_t n);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_set_init(void);

#endif /* MENAI_VM_SET_H */
