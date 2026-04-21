/*
 * menai_vm_types.h — portable typedefs for Py_ssize_t and Py_hash_t.
 *
 * Headers in the Menai VM that no longer include <Python.h> directly still
 * use Py_ssize_t and Py_hash_t in their public APIs because these types
 * appear in Python-facing signatures and in internal data structures that
 * were originally sized to match Python's conventions.
 *
 * When this header is included before <Python.h>, it provides compatible
 * definitions from standard C headers.  When <Python.h> has already been
 * included (detected via Py_INCREF, which Python always defines as a macro),
 * all definitions below are skipped and this header is a no-op.
 */

#ifndef MENAI_VM_TYPES_H
#define MENAI_VM_TYPES_H

#include <stddef.h>
#include <stdint.h>

/*
 * Py_INCREF is defined as a macro by every version of Python.h.
 * If it is not defined, Python.h has not yet been included.
 */
#ifndef Py_INCREF
typedef ptrdiff_t Py_ssize_t;
typedef Py_ssize_t Py_hash_t;
#endif

#endif /* MENAI_VM_TYPES_H */
