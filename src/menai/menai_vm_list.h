/*
 * menai_vm_list.h — MenaiList type definition and API.
 *
 * MenaiList stores a C array of PyObject* elements with a length count.
 * It uses a free-list cache for both object structs and element arrays to
 * reduce allocation pressure in the hot VM loop.
 *
 * The three C-level constructors (menai_list_from_array,
 * menai_list_from_array_steal, menai_list_from_tuple) are the primary
 * allocation paths used by the VM.  The Python-level MenaiList() constructor
 * is available via menai_list_new_empty() for creating the empty-list singleton.
 */

#ifndef MENAI_VM_LIST_H
#define MENAI_VM_LIST_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    PyObject **elements; /* C array of MenaiValue* */
    Py_ssize_t length;   /* number of elements */
} MenaiList_Object;

extern PyTypeObject MenaiList_Type;

/*
 * menai_list_from_array — copy items, INCREF each element.
 * Returns a new reference, or NULL on MemoryError.
 */
PyObject *menai_list_from_array(PyObject **items, Py_ssize_t n);

/*
 * menai_list_from_array_steal — take ownership of items without INCREFing.
 * Returns a new reference, or NULL on MemoryError (items freed on failure).
 */
PyObject *menai_list_from_array_steal(PyObject **items, Py_ssize_t n);

/*
 * menai_list_from_tuple — copy from tuple, INCREF each, DECREF tuple.
 * Returns a new reference, or NULL on MemoryError.
 */
PyObject *menai_list_from_tuple(PyObject *tup);

/*
 * menai_list_new_empty — create a zero-element MenaiList.
 * Used by _menai_vm_value_init() to build the Menai_LIST_EMPTY singleton.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_list_new_empty(void);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_list_init(void);

/* ---------------------------------------------------------------------------
 * Inline accessors — used heavily in the hot VM loop
 * ------------------------------------------------------------------------- */

static inline PyObject *
menai_list_get(MenaiList_Object *list, Py_ssize_t i)
{
    return list->elements[i];
}

static inline PyObject **
menai_list_elements(PyObject *list_obj)
{
    return ((MenaiList_Object *)list_obj)->elements;
}

static inline Py_ssize_t
menai_list_length(PyObject *list_obj)
{
    return ((MenaiList_Object *)list_obj)->length;
}

#endif /* MENAI_VM_LIST_H */
