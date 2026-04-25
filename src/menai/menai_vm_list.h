/*
 * menai_vm_list.h — MenaiList type definition and API.
 *
 * MenaiList stores its elements inline in the same allocation as the struct
 * itself, using a C99 flexible array member.  Owning lists set elements to
 * point at inline_elements; slice views set elements to point into the owner's
 * inline_elements array and hold a retain on the owner.
 *
 * The primary allocation path is menai_list_alloc(n), which allocates the
 * combined struct+elements block and returns an uninitialised-elements list
 * ready for the caller to fill.  menai_list_new_empty() creates the
 * empty-list singleton.
 */
#ifndef MENAI_VM_LIST_H
#define MENAI_VM_LIST_H

typedef struct {
    MenaiValue_HEAD
    MenaiValue **elements; /* points to inline_elements for owners, into owner for views */
    ssize_t length;        /* number of live elements */
    /*
     * owner is non-NULL when this list is a slice view into another list's
     * inline_elements array.  In that case elements points into owner's storage
     * and must not be freed; only menai_release(owner) is needed on dealloc.
     * owner always points to a list with owner == NULL (never a chain).
     */
    MenaiValue *owner;
    MenaiValue *inline_elements[]; /* FAM — storage for owning lists */
} MenaiList;

MenaiValue *menai_list_alloc(ssize_t n);
MenaiValue *menai_list_new_empty(void);
MenaiValue *menai_list_rest(MenaiValue *lst);
MenaiValue *menai_list_slice(MenaiValue *lst, ssize_t start, ssize_t end);

static inline MenaiValue *
menai_list_get(MenaiList *list, ssize_t i)
{
    return list->elements[i];
}

static inline MenaiValue **
menai_list_elements(MenaiValue *list_obj)
{
    return ((MenaiList *)list_obj)->elements;
}

static inline ssize_t
menai_list_length(MenaiValue *list_obj)
{
    return ((MenaiList *)list_obj)->length;
}

static inline void
menai_list_dealloc(MenaiValue *self)
{
    MenaiList *lst = (MenaiList *)self;
    if (lst->owner != NULL) {
        /* View — release the backing list; do not touch the element array. */
        menai_release(lst->owner);
        menai_free(lst);
        return;
    }

    /* Owner — release all elements then free the combined block. */
    ssize_t n = lst->length;
    MenaiValue **arr = lst->elements;
    for (ssize_t i = 0; i < n; i++) {
        menai_release(*arr++);
    }

    menai_free(lst);
}

#endif /* MENAI_VM_LIST_H */
