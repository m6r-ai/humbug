/*
 * menai_vm_dict.h — MenaiDict type definition and API.
 *
 * MenaiDict stores an ordered sequence of key-value entries as three parallel
 * C arrays (keys, values, hkeys) plus a Python dict for O(1) index lookup.
 * Canonical hash keys are computed once at construction time and stored in
 * hkeys, so dict transforms never recompute them.
 *
 * Invariants:
 *   - keys[i], values[i], hkeys[i] are all owned references.
 *   - hkeys[i] == menai_hashable_key(keys[i]), computed once at construction.
 *   - lookup maps hkeys[i] -> PyLong(i) for every i in [0, length).
 *   - No duplicate keys: all hkeys[i] are distinct.
 *   - Insertion order is preserved by the array indices.
 */

#ifndef MENAI_VM_DICT_H
#define MENAI_VM_DICT_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    PyObject **keys;    /* C array of original key MenaiValues, length entries */
    PyObject **values;  /* C array of value MenaiValues, length entries */
    PyObject **hkeys;   /* C array of canonical hashable keys, length entries */
    PyObject *lookup;   /* Python dict: hkeys[i] -> PyLong(i) */
    Py_ssize_t length;
} MenaiDict_Object;

extern PyTypeObject MenaiDict_Type;

/*
 * menai_dict_new_empty — create a zero-entry MenaiDict.
 * Used by _menai_vm_value_init() to build the Menai_DICT_EMPTY singleton.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_dict_new_empty(void);

/*
 * menai_dict_from_arrays_steal — build a MenaiDict from three parallel C arrays.
 *
 * keys, values, and hkeys must each have n entries.  All three arrays and
 * their contents are stolen: on success the arrays are owned by the new dict;
 * on failure the arrays are freed and all contained references are released.
 * The caller must guarantee that hkeys[i] == menai_hashable_key(keys[i]) and
 * that all hkeys are distinct.
 *
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_dict_from_arrays_steal(PyObject **keys, PyObject **values,
                                       PyObject **hkeys, Py_ssize_t n);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_dict_init(void);

#endif /* MENAI_VM_DICT_H */
