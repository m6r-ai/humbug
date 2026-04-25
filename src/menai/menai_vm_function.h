/*
 * menai_vm_function.h — MenaiFunction type definition and API.
 *
 * MenaiFunction represents a Menai closure.  It holds a retained reference to
 * a MenaiCodeObject (which owns all frame metadata) and an inline C array of
 * captured MenaiValues.  No Python objects are held after construction.
 */
#ifndef MENAI_VM_FUNCTION_H
#define MENAI_VM_FUNCTION_H

/*
 * MenaiFunction — a Menai closure.
 *
 * bytecode is a retained MenaiCodeObject *.  All frame metadata (instrs,
 * constants, names, children) is read directly from it.  The Python-facing
 * getsets (parameters, name, bytecode, is_variadic, param_count,
 * captured_values) reconstruct Python objects from the native fields on
 * demand and are used only by the legacy Python VM path and tests.
 */
typedef struct {
    MenaiValue_HEAD
    ssize_t ncap;                  /* number of captured values */
    MenaiCodeObject *bytecode;     /* retained — owns all frame metadata */

    /* Inline capture array — ncap elements follow immediately. */
    MenaiValue *captures[1];       /* flexible array member (C99 [1] for MSVC compat) */
} MenaiFunction;

MenaiValue *menai_function_alloc(MenaiCodeObject *co, MenaiValue *none_val);

static inline void
menai_function_dealloc(MenaiValue *self)
{
    MenaiFunction *f = (MenaiFunction *)self;
    menai_code_object_release(f->bytecode);
    ssize_t ncap = f->ncap;
    for (ssize_t i = 0; i < ncap; i++) {
        menai_xrelease(f->captures[i]);
    }

    menai_free(self);
}


#endif /* MENAI_VM_FUNCTION_H */
