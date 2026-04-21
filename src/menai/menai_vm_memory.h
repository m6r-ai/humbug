/*
 * menai_vm_memory.h — Memory management API for the Menai VM.
 *
 * All lifetime operations on Menai values in the VM dispatch loop go through
 * this API.  The current implementation is backed by CPython reference
 * counting, but the interface is defined in terms of Menai semantics so the
 * backing implementation can be replaced without touching the dispatch loop.
 *
 * Naming convention:
 *   menai_retain / menai_release  — claim / relinquish an interest in a value
 *   menai_xrelease                — release if non-NULL (safe on NULL)
 *   menai_is_unique               — true if this is the sole live reference
 *   menai_reg_*                   — register file operations
 *   menai_regs_alloc / _free      — register array lifetime
 */

#ifndef MENAI_VM_MEMORY_H
#define MENAI_VM_MEMORY_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

/*
 * menai_retain — claim an interest in val.
 *
 * Must be called whenever a Menai value is stored into a location whose
 * lifetime is independent of the source location (e.g. a closure capture
 * array, a temporary pin before a loop that may overwrite the source slot).
 */
static inline void
menai_retain(PyObject *val)
{
    Py_INCREF(val);
}

/*
 * menai_release — relinquish an interest in val.
 *
 * val must not be NULL.  Paired with every menai_retain and every
 * menai_reg_set_own / menai_reg_set_borrow that displaced a live value.
 */
static inline void
menai_release(PyObject *val)
{
    Py_DECREF(val);
}

/*
 * menai_xrelease — relinquish an interest in val if val is non-NULL.
 *
 * Used where val may legitimately be NULL (e.g. frame->code_obj before first
 * use, partially-built GlobalsTable entries).
 */
static inline void
menai_xrelease(PyObject *val)
{
    Py_XDECREF(val);
}

/*
 * menai_is_unique — return non-zero if val has exactly one live reference.
 *
 * Used to enable in-place mutation optimisations: if the caller holds the
 * only reference to an immutable value, it can be safely reused rather than
 * copying.
 */
static inline int
menai_is_unique(PyObject *val)
{
    return Py_REFCNT(val) == 1;
}

/*
 * menai_reg_set_own — store an owned reference into a register slot.
 *
 * val is an already-owned reference (e.g. freshly allocated, or returned from
 * a constructor).  The old slot value is released.  The slot must not be NULL.
 */
static inline void
menai_reg_set_own(PyObject **regs, int slot, PyObject *val)
{
    PyObject *old = regs[slot];
    regs[slot] = val;
    Py_DECREF(old);
}

/*
 * menai_reg_set_borrow — store a borrowed reference into a register slot.
 *
 * val is a borrowed reference (e.g. read from another register, a constant
 * table, or a container element).  A retain is taken on val, then the old
 * slot value is released.  The slot must not be NULL.
 */
static inline void
menai_reg_set_borrow(PyObject **regs, int slot, PyObject *val)
{
    PyObject *old = regs[slot];
    Py_INCREF(val);
    regs[slot] = val;
    Py_DECREF(old);
}

/*
 * menai_reg_init — write an owned reference into a slot that is known to hold
 * Menai_NONE (i.e. freshly allocated or reset to the default).
 *
 * Used when populating a callee's register window with arguments or captures
 * before a call.  The old slot value (Menai_NONE) is released.
 */
static inline void
menai_reg_init(PyObject **regs, int slot, PyObject *val, PyObject *none_val)
{
    regs[slot] = val;
    Py_DECREF(none_val);  /* release the Menai_NONE that was there */
}

/*
 * menai_regs_alloc — allocate and initialise a register array of n slots.
 *
 * Every slot is initialised to Menai_NONE with an owned reference.
 * Returns NULL and sets MemoryError on failure.
 */
PyObject **menai_regs_alloc(Py_ssize_t n, PyObject *none_val);

/*
 * menai_regs_free — release all owned references in the register array and
 * free it.  Every slot holds either Menai_NONE or an owned Menai value
 * reference; all are released.
 */
void menai_regs_free(PyObject **regs, Py_ssize_t n);

#endif /* MENAI_VM_MEMORY_H */
