/*
 * menai_vm_function.h — MenaiFunction type definition and API.
 *
 * MenaiFunction represents a Menai closure.  It holds a retained reference to
 * a MenaiCodeObject (which owns all frame metadata) and an inline C array of
 * captured MenaiValues.  No Python objects are held after construction.
 */

#ifndef MENAI_VM_FUNCTION_H
#define MENAI_VM_FUNCTION_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>

#include "menai_vm_value.h"
#include "menai_vm_code.h"

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

#endif /* MENAI_VM_FUNCTION_H */
