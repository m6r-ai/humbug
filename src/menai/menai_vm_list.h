/*
 * menai_vm_list.h — MenaiList type definition and API.
 *
 * MenaiList stores a C array of MenaiValue elements with a length count.
 * It uses a free-list cache for both object structs and element arrays to
 * reduce allocation pressure in the hot VM loop.
 *
 * The three C-level constructors (menai_list_from_array,
 * menai_list_from_array_steal) are the primary allocation paths used by the VM.
 * menai_list_new_empty() creates the empty-list singleton.
 */

#ifndef MENAI_VM_LIST_H
#define MENAI_VM_LIST_H

#include "menai_vm_object.h"

typedef struct {
    MenaiObject_HEAD
    MenaiValue *elements; /* pointer to first live element */
    Py_ssize_t length;    /* number of live elements */
    /*
     * owner is non-NULL when this list is a slice view into another list's
     * element array.  In that case elements points into owner->elements and
     * must not be freed; only menai_release(owner) is needed on dealloc.
     * owner always points to a list with owner == NULL (never a chain).
     */
    MenaiValue owner;
} MenaiList_Object;

extern MenaiType MenaiList_Type;

/*
 * menai_list_from_array — copy items, retain each element.
 * Returns a new reference, or NULL on MemoryError.
 */
MenaiValue menai_list_from_array(MenaiValue *items, Py_ssize_t n);

/*
 * menai_list_from_array_steal — take ownership of items without retaining.
 * Returns a new reference, or NULL on MemoryError (items freed on failure).
 */
MenaiValue menai_list_from_array_steal(MenaiValue *items, Py_ssize_t n);

/*
 * menai_list_new_empty — create a zero-element MenaiList.
 * Used by _menai_vm_value_init() to build the Menai_LIST_EMPTY singleton.
 * Returns a new reference, or NULL on error.
 */
MenaiValue menai_list_new_empty(void);

/*
 * menai_list_rest — return a slice view of lst starting at element 1.
 *
 * If lst is empty, raises MenaiEvalError and returns NULL.
 * If lst has one element, returns the Menai_EMPTY_LIST singleton (borrowed
 * from the caller — the caller must reg_set_borrow it).
 * Otherwise returns a new MenaiList_Object that shares lst's backing array
 * without copying or retaining any elements.
 */
MenaiValue menai_list_rest(MenaiValue lst);

/*
 * menai_list_slice — return a slice view of lst covering [start, end).
 *
 * start and end must already be validated by the caller (0 <= start <= end
 * <= lst->length).  Returns a new MenaiList_Object that shares lst's backing
 * array without copying or retaining any elements.
 */
MenaiValue menai_list_slice(MenaiValue lst, Py_ssize_t start, Py_ssize_t end);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure.
 */
int menai_vm_list_init(void);

/* ---------------------------------------------------------------------------
 * Inline accessors — used heavily in the hot VM loop
 * ------------------------------------------------------------------------- */

static inline MenaiValue
menai_list_get(MenaiList_Object *list, Py_ssize_t i)
{
    return list->elements[i];
}

static inline MenaiValue *
menai_list_elements(MenaiValue list_obj)
{
    return ((MenaiList_Object *)list_obj)->elements;
}

static inline Py_ssize_t
menai_list_length(MenaiValue list_obj)
{
    return ((MenaiList_Object *)list_obj)->length;
}

#endif /* MENAI_VM_LIST_H */
