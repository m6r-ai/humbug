/*
 * menai_vm_symbol.h — MenaiSymbol type definition and API.
 *
 * MenaiSymbol stores its name as a MenaiString *.  Equality is
 * determined by menai_string_equal() on the name field.
 */
#ifndef MENAI_VM_SYMBOL_H
#define MENAI_VM_SYMBOL_H

typedef struct {
    MenaiValue_HEAD
    MenaiValue *name;    /* owned MenaiString * */
} MenaiSymbol;

MenaiValue *menai_symbol_alloc(MenaiValue *name);

static inline void
menai_symbol_dealloc(MenaiValue *self)
{
    menai_xrelease(((MenaiSymbol *)self)->name);
    menai_free(self);
}

#endif /* MENAI_VM_SYMBOL_H */
