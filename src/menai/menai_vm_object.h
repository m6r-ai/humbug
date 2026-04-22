/*
 * menai_vm_object.h — base object model for the Menai VM.
 *
 * Defines the MenaiValue pointer type, the MenaiObject_HEAD macro, and the
 * MenaiType typedef.
 *
 * MenaiType is currently a typedef for PyTypeObject.  This means every live
 * MenaiObject has an ob_type that Python can safely dereference as a
 * PyTypeObject *, which is required for correct interaction with Python's GC
 * and type system.  The value type implementation files define their type
 * objects as PyTypeObject instances with tp_dealloc set to a native C
 * function that calls free() (or the type-specific free-list).
 *
 * The VM reads ob_type->tp_name for error messages.  Deallocation goes
 * through ob_destructor, a direct function pointer stored in every object at
 * construction time.  This avoids the two-pointer-chase and unpredictable
 * indirect branch through PyTypeObject.tp_dealloc on every release.
 *
 * Future: once the Python embedding is removed, MenaiType can be replaced
 * with a smaller native struct.  The MenaiObject_HEAD layout and all call
 * sites will remain unchanged.
 */

#ifndef MENAI_VM_OBJECT_H
#define MENAI_VM_OBJECT_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_types.h"

typedef struct MenaiObject_s MenaiObject;
typedef MenaiObject MenaiValue;

/*
 * MenaiType — the type descriptor for a Menai value type.
 *
 * Currently an alias for PyTypeObject.  The VM uses tp_name for error
 * messages and tp_dealloc for object destruction.  tp_flags must not include
 * Py_TPFLAGS_HAVE_GC; tp_traverse and tp_clear must be NULL so that Python's
 * GC does not attempt to traverse our objects.
 */
typedef PyTypeObject MenaiType;

/*
 * MenaiObject_HEAD — common prefix for every Menai value struct.
 *
 * ob_refcnt  — reference count.  Starts at 1 on allocation.
 * ob_type    — pointer to the PyTypeObject for this object.
 * ob_destructor — called by menai_release when ob_refcnt reaches zero.
 *                 Set once at construction; never changes.
 *
 * The layout (ob_refcnt at offset 0, ob_type at offset sizeof(size_t)) is
 * identical to PyObject_HEAD, so MenaiValue pointers can be cast to
 * PyObject * at the boundary without any field reordering.  ob_destructor
 * follows ob_type and is invisible to Python.
 *
 * Usage:
 *
 *   typedef struct {
 *       MenaiObject_HEAD
 *       double value;
 *   } MenaiFloat_Object;
 */
typedef void (*menai_destructor)(MenaiValue *);

#define MenaiObject_HEAD              \
    size_t ob_refcnt;                 \
    MenaiType *ob_type;               \
    menai_destructor ob_destructor;

/*
 * MenaiObject — the minimal struct that every MenaiValue pointer can be
 * safely cast to in order to read ob_refcnt and ob_type.
 */
struct MenaiObject_s {
    MenaiObject_HEAD
};

/*
 * menai_retain — claim an interest in val.
 */
static inline void
menai_retain(MenaiValue *val)
{
    val->ob_refcnt++;
}

/*
 * menai_release — relinquish an interest in val.
 *
 * val must not be NULL.  When ob_refcnt reaches zero, tp_dealloc is called.
 */
static inline void
menai_release(MenaiValue *val)
{
    if (--val->ob_refcnt == 0) {
        val->ob_destructor(val);
    }
}

/*
 * menai_xrelease — relinquish an interest in val if val is non-NULL.
 */
static inline void
menai_xrelease(MenaiValue *val)
{
    if (val != NULL) {
        menai_release(val);
    }
}

/*
 * menai_is_unique — return non-zero if val has exactly one live reference.
 */
static inline int
menai_is_unique(MenaiValue *val)
{
    return val->ob_refcnt == 1;
}

#endif /* MENAI_VM_OBJECT_H */
