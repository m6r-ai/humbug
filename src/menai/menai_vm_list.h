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

#include "menai_vm_value.h"

typedef struct {
    MenaiValue_HEAD
    MenaiValue **elements; /* pointer to first live element */
    Py_ssize_t length;    /* number of live elements */
    /*
     * owner is non-NULL when this list is a slice view into another list's
     * element array.  In that case elements points into owner->elements and
     * must not be freed; only menai_release(owner) is needed on dealloc.
     * owner always points to a list with owner == NULL (never a chain).
     */
    MenaiValue *owner;
} MenaiList;

MenaiValue *menai_list_from_array(MenaiValue **items, Py_ssize_t n);
MenaiValue *menai_list_from_array_steal(MenaiValue **items, Py_ssize_t n);
MenaiValue *menai_list_new_empty(void);
MenaiValue *menai_list_rest(MenaiValue *lst);
MenaiValue *menai_list_slice(MenaiValue *lst, Py_ssize_t start, Py_ssize_t end);

static inline MenaiValue *
menai_list_get(MenaiList *list, Py_ssize_t i)
{
    return list->elements[i];
}

static inline MenaiValue **
menai_list_elements(MenaiValue *list_obj)
{
    return ((MenaiList *)list_obj)->elements;
}

static inline Py_ssize_t
menai_list_length(MenaiValue *list_obj)
{
    return ((MenaiList *)list_obj)->length;
}

#endif /* MENAI_VM_LIST_H */
