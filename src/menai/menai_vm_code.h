/*
 * menai_vm_code.h — MenaiCodeObject type definition and API.
 *
 * MenaiCodeObject is the native C representation of a compiled Menai code
 * object.  It is built once from the Python CodeObject tree at the start of
 * execution and is never modified thereafter.  The VM holds retained
 * references to MenaiCodeObject instances; Python objects are not referenced
 * after the initial conversion.
 *
 * Ownership:
 *   instrs        — owned (malloc'd copy of the instruction array)
 *   constants     — owned (array of retained MenaiValue * references)
 *   names         — owned (array of strdup'd UTF-8 name strings)
 *   param_names   — owned (array of strdup'd UTF-8 parameter name strings)
 *   children      — owned (array of retained MenaiCodeObject * pointers)
 *   name          — owned (strdup'd UTF-8 name string, may be NULL)
 */

#ifndef MENAI_VM_CODE_H
#define MENAI_VM_CODE_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>

#include "menai_vm_object.h"

typedef struct MenaiCodeObject_s {
    size_t ob_refcnt;

    uint64_t *instrs;                    /* packed instruction words */
    int code_len;                        /* number of instructions */

    MenaiValue **constants;              /* fast constant pool */
    Py_ssize_t nconst;

    const char **names;                  /* global name strings for OP_LOAD_NAME */
    Py_ssize_t nnames;

    struct MenaiCodeObject_s **children; /* child code objects, one per closure */
    Py_ssize_t nchildren;

    int param_count;
    int local_count;
    int outgoing_arg_slots;
    int is_variadic;
    Py_ssize_t ncap;                     /* number of free variables (capture slots) */

    char **param_names;                  /* parameter name strings, parallel to param_count */
    Py_ssize_t nparam_names;             /* number of elements in param_names */

    char *name;                          /* function name for error messages, or NULL */
} MenaiCodeObject;

/*
 * menai_code_object_retain — increment the reference count.
 */
static inline void
menai_code_object_retain(MenaiCodeObject *co)
{
    co->ob_refcnt++;
}

/*
 * menai_code_object_release — decrement the reference count and free if zero.
 */
void menai_code_object_release(MenaiCodeObject *co);

/*
 * menai_code_object_from_python — build a MenaiCodeObject tree from a Python
 * CodeObject.  All constants are converted to fast MenaiValues.  Returns a
 * new reference (ob_refcnt == 1), or NULL on error with a Python exception set.
 */
MenaiCodeObject *menai_code_object_from_python(PyObject *py_code);

/*
 * menai_code_object_max_locals — return the maximum (local_count +
 * outgoing_arg_slots) across the entire subtree rooted at co.
 */
int menai_code_object_max_locals(const MenaiCodeObject *co);

#endif /* MENAI_VM_CODE_H */
