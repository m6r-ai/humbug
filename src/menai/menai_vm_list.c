/*
 * menai_vm_list.c — MenaiList type implementation.
 *
 * MenaiList stores a C array of MenaiValue elements.  Object structs and
 * element arrays are allocated via menai_alloc/menai_free.  Also provides
 * the three C-level constructors used by the VM:
 *   menai_list_from_array        — copy items, retain each
 *   menai_list_from_array_steal  — take ownership, no retain
 */

#include <stdlib.h>
#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <string.h>

#include "menai_vm_alloc.h"
#include "menai_vm_list.h"
#include "menai_vm_memory.h"
#include "menai_vm_hashtable.h"

static void MenaiList_dealloc(MenaiValue *self);

/* ---------------------------------------------------------------------------
 * Type implementation
 * ------------------------------------------------------------------------- */

static void
MenaiList_dealloc(MenaiValue *self)
{
    MenaiList *lst = (MenaiList *)self;
    if (lst->owner != NULL) {
        /* View — release the backing list; do not touch the element array. */
        MenaiValue *owner = lst->owner;
        lst->owner = NULL;
        lst->elements = NULL;
        lst->length = 0;
        menai_release(owner);
    } else {
        /* Owner — release all elements then free the element array. */
        Py_ssize_t n = lst->length;
        lst->length = 0;
        MenaiValue **arr = lst->elements;
        lst->elements = NULL;
        for (Py_ssize_t i = 0; i < n; i++) {
            menai_release(arr[i]);
        }

        menai_free(arr, (size_t)n * sizeof(MenaiValue *));
    }

    menai_free(lst, sizeof(MenaiList));
}

PyTypeObject MenaiList_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiList",          /* tp_name */
    sizeof(MenaiList),   /* tp_basicsize */
    0,                             /* tp_itemsize */
    (destructor)MenaiList_dealloc, /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

MenaiValue *
menai_list_from_array(MenaiValue **items, Py_ssize_t n)
{
    MenaiValue **arr = NULL;
    if (n > 0) {
        arr = (MenaiValue **)menai_alloc((size_t)n * sizeof(MenaiValue *));
        if (!arr) {
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            arr[i] = items[i];
            menai_retain(arr[i]);
        }
    }

    MenaiList *obj = (MenaiList *)menai_alloc(sizeof(MenaiList));
    if (!obj) {
        for (Py_ssize_t i = 0; i < n; i++) {
            menai_release(arr[i]);
        }

        menai_free(arr, (size_t)n * sizeof(MenaiValue *));
        return NULL;
    }

    obj->ob_refcnt = 1;
    obj->ob_type = &MenaiList_Type;
    obj->ob_destructor = MenaiList_dealloc;
    obj->elements = arr;
    obj->length = n;
    obj->owner = NULL;

    return (MenaiValue *)obj;
}

MenaiValue *
menai_list_from_array_steal(MenaiValue **items, Py_ssize_t n)
{
    MenaiList *obj = (MenaiList *)menai_alloc(sizeof(MenaiList));
    if (!obj) {
        /* Free the stolen array and its references on failure. */
        for (Py_ssize_t i = 0; i < n; i++) {
            menai_release(items[i]);
        }

        free(items);
        return NULL;
    }

    obj->ob_refcnt = 1;
    obj->ob_type = &MenaiList_Type;
    obj->ob_destructor = MenaiList_dealloc;
    obj->elements = items;
    obj->length = n;
    obj->owner = NULL;

    return (MenaiValue *)obj;
}

MenaiValue *
menai_list_new_empty(void)
{
    MenaiList *obj = (MenaiList *)menai_alloc(sizeof(MenaiList));
    if (!obj) {
        return NULL;
    }

    obj->ob_refcnt = 1;
    obj->ob_type = &MenaiList_Type;
    obj->ob_destructor = MenaiList_dealloc;
    obj->elements = NULL;
    obj->length = 0;
    obj->owner = NULL;

    return (MenaiValue *)obj;
}

MenaiValue *
menai_list_rest(MenaiValue *lst_val)
{
    MenaiList *lst = (MenaiList *)lst_val;
    if (lst->length == 0) {
        /* Error reporting still goes through Python exceptions for now. */
        PyErr_SetString(PyExc_RuntimeError,
            "Function 'list-rest' requires a non-empty list");
        return NULL;
    }

    /*
     * Resolve the owner: if lst is itself a view, use its owner so we never
     * build a chain — all views point directly at the root array owner.
     */
    MenaiValue *owner = (lst->owner != NULL) ? lst->owner : lst_val;

    MenaiList *view = (MenaiList *)menai_alloc(sizeof(MenaiList));
    if (view == NULL) {
        return NULL;
    }

    view->ob_refcnt = 1;
    view->ob_type = &MenaiList_Type;
    view->ob_destructor = MenaiList_dealloc;
    menai_retain(owner);
    view->owner = owner;
    view->elements = lst->elements + 1;
    view->length = lst->length - 1;

    return (MenaiValue *)view;
}

MenaiValue *
menai_list_slice(MenaiValue *lst_val, Py_ssize_t start, Py_ssize_t end)
{
    MenaiList *lst = (MenaiList *)lst_val;

    /*
     * Resolve the owner: if lst is itself a view, point at its owner so
     * all views are depth-1 from the root array owner.
     */
    MenaiValue *owner = (lst->owner != NULL) ? lst->owner : lst_val;

    MenaiList *view = (MenaiList *)menai_alloc(sizeof(MenaiList));
    if (view == NULL) {
        return NULL;
    }

    view->ob_refcnt = 1;
    view->ob_type = &MenaiList_Type;
    view->ob_destructor = MenaiList_dealloc;
    menai_retain(owner);
    view->owner = owner;
    view->elements = lst->elements + start;
    view->length = end - start;

    return (MenaiValue *)view;
}

int
menai_vm_list_init(void)
{
    return PyType_Ready(&MenaiList_Type);
}
