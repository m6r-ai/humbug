/*
 * menai_vm_list.h — MenaiList type definition and API.
 *
 * MenaiList stores a C array of PyObject* elements with a length count.
 * It uses a free-list cache for both object structs and element arrays to
 * reduce allocation pressure in the hot VM loop.
 *
 * The three C-level constructors (menai_list_from_array, * menai_list_from_array_steal) 
 * are the primary allocation paths used by the VM.  The Python-level MenaiList()
 * constructor is available via menai_list_new_empty() for creating the empty-list singleton.
 */

#ifndef MENAI_VM_LIST_H
#define MENAI_VM_LIST_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    PyObject **elements; /* pointer to first live element */
    Py_ssize_t length;   /* number of live elements */
    /*
     * owner is non-NULL when this list is a slice view into another list's
     * element array.  In that case elements points into owner->elements and
     * must not be freed; only Py_DECREF(owner) is needed on dealloc.
     * owner always points to a list with owner == NULL (never a chain).
     */
    PyObject *owner;
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
 * menai_list_new_empty — create a zero-element MenaiList.
 * Used by _menai_vm_value_init() to build the Menai_LIST_EMPTY singleton.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_list_new_empty(void);

/*
 * menai_list_rest — return a slice view of lst starting at element 1.
 *
 * If lst is empty, raises MenaiEvalError and returns NULL.
 * If lst has one element, returns the Menai_EMPTY_LIST singleton (borrowed
 * from the caller — the caller must reg_set_borrow it).
 * Otherwise returns a new MenaiList_Object that shares lst's backing array
 * without copying or INCREFing any elements.
 */
PyObject *menai_list_rest(PyObject *lst);

/*
 * menai_list_slice — return a slice view of lst covering [start, end).
 *
 * start and end must already be validated by the caller (0 <= start <= end
 * <= lst->length).  Returns a new MenaiList_Object that shares lst's backing
 * array without copying or INCREFing any elements.
 */
PyObject *menai_list_slice(PyObject *lst, Py_ssize_t start, Py_ssize_t end);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_list_init(void);

PyObject *MenaiList_describe(PyObject *self, PyObject *args);
PyObject *MenaiList_to_python(PyObject *self, PyObject *args);

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
