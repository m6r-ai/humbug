/*
 * menai_vm_none.h — MenaiNone type definition and API.
 *
 * MenaiNone is a singleton type with no payload beyond the object header.
 * It represents the absence of a value in the Menai runtime.
 */
#ifndef MENAI_VM_NONE_H
#define MENAI_VM_NONE_H

typedef struct {
    MenaiValue_HEAD
} MenaiNone;

MenaiValue *menai_none_singleton(void);
void menai_vm_none_init(void);

static inline void
menai_none_dealloc(MenaiValue *self)
{
    /*
     * The singleton is never freed — its refcount should never reach zero.
     */
    (void)self;
}

#endif /* MENAI_VM_NONE_H */
