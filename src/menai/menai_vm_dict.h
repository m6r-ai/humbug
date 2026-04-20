/*
 * menai_vm_dict.h — MenaiDict type definition and API.
 *
 * MenaiDict stores an ordered sequence of key-value entries as three parallel
 * C arrays (keys, values, hashes) plus a pure-C MenaiHashTable for O(1) index
 * lookup.  Hash values are computed once at construction time and stored in
 * hashes[], so dict transforms never recompute them.
 *
 * Invariants:
 *   - keys[i], values[i] are owned references.
 *   - hashes[i] == menai_value_hash(keys[i]), computed once at construction.
 *   - ht maps keys[i] (by value equality) to index i for every i in [0, length).
 *   - No duplicate keys: all keys[i] are distinct by menai_value_equal.
 *   - Insertion order is preserved by the array indices.
 */

#ifndef MENAI_VM_DICT_H
#define MENAI_VM_DICT_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_hashtable.h"

typedef struct {
    PyObject_HEAD
    PyObject     **keys;    /* C array of original key MenaiValues */
    PyObject     **values;  /* C array of value MenaiValues */
    Py_hash_t     *hashes;  /* C array of menai_value_hash(keys[i]) */
    MenaiHashTable ht;      /* pure-C hash table: key -> index */
    Py_ssize_t     length;
} MenaiDict_Object;

extern PyTypeObject MenaiDict_Type;

/*
 * menai_dict_new_empty — create a zero-entry MenaiDict.
 * Used by _menai_vm_value_init() to build the Menai_DICT_EMPTY singleton.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_dict_new_empty(void);

/*
 * menai_dict_from_arrays_steal — build a MenaiDict from parallel C arrays.
 *
 * keys and values must each have n entries; hashes[i] must equal
 * menai_value_hash(keys[i]) for every i.  All three arrays and their contents
 * are stolen: on success the arrays are owned by the new dict; on failure the
 * arrays are freed and all contained references are released.
 * The caller must guarantee that all keys are distinct.
 *
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_dict_from_arrays_steal(PyObject **keys, PyObject **values,
                                       Py_hash_t *hashes, Py_ssize_t n);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_dict_init(void);

#endif /* MENAI_VM_DICT_H */
