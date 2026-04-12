/*
 * menai_vm_dict.h — MenaiDict type definition and API.
 *
 * MenaiDict stores an ordered tuple of (key, value) pairs alongside a Python
 * dict mapping hashable keys to pairs for O(1) lookup.
 */

#ifndef MENAI_VM_DICT_H
#define MENAI_VM_DICT_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    PyObject *pairs;    /* Python tuple of (key, value) 2-tuples */
    PyObject *lookup;   /* Python dict: hashable_key -> (key, value) */
    Py_ssize_t length;  /* number of key-value pairs */
} MenaiDict_Object;

extern PyTypeObject MenaiDict_Type;

/*
 * menai_dict_new_empty — create a zero-pair MenaiDict.
 * Used by _menai_vm_value_init() to build the Menai_DICT_EMPTY singleton.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_dict_new_empty(void);

/*
 * menai_dict_from_fast_pairs — build a MenaiDict from a tuple of already-fast
 * (key, value) 2-tuples.  Steals the reference to fast_pairs.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_dict_from_fast_pairs(PyObject *fast_pairs);

/*
 * Module init — called once from _menai_vm_value_init().
 * eval_error_type is a borrowed reference to MenaiEvalError.
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_dict_init(PyObject *eval_error_type);

#endif /* MENAI_VM_DICT_H */
