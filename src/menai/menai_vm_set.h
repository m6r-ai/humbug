/*
 * menai_vm_set.h — MenaiSet type definition and API.
 *
 * MenaiSet stores an ordered, deduplicated tuple of elements and a frozenset
 * of hashable keys for O(1) membership testing.  There are no singletons
 * beyond the empty-set singleton created at init time via menai_set_new_empty().
 */

#ifndef MENAI_VM_SET_H
#define MENAI_VM_SET_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    PyObject *elements; /* Python tuple of MenaiValue* (ordered, deduplicated) */
    PyObject *members;  /* Python frozenset of hashable keys */
    Py_ssize_t length;  /* number of elements */
} MenaiSet_Object;

extern PyTypeObject MenaiSet_Type;

/*
 * menai_set_new_empty — create a zero-element MenaiSet.
 * Used by _menai_vm_value_init() to build the Menai_SET_EMPTY singleton.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_set_new_empty(void);

/*
 * menai_set_from_fast_tuple — build a MenaiSet from a tuple of already-fast
 * MenaiValues.  Handles deduplication and builds the members frozenset.
 * Steals the reference to fast_tup.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_set_from_fast_tuple(PyObject *fast_tup);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_set_init(void);

#endif /* MENAI_VM_SET_H */
