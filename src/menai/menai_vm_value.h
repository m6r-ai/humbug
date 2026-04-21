/*
 * menai_vm_value.h — C struct definitions for all Menai runtime value types.
 *
 * Included by menai_vm_c.c and menai_vm_value.c.  Provides the concrete
 * PyObject struct layouts so that the C VM can access fields directly by cast
 * rather than via PyObject_GetAttrString.
 *
 * All types are defined in menai_vm_value.c and exposed via the
 * menai_vm_value module.  The VM imports that module at init time to obtain
 * the type objects and singleton values; after that it uses these structs
 * directly.
 */

#ifndef MENAI_VM_VALUE_H
#define MENAI_VM_VALUE_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_float.h"
#include "menai_vm_boolean.h"
#include "menai_vm_complex.h"
#include "menai_vm_dict.h"
#include "menai_vm_function.h"
#include "menai_vm_integer.h"
#include "menai_vm_list.h"
#include "menai_vm_none.h"
#include "menai_vm_string.h"
#include "menai_vm_set.h"
#include "menai_vm_struct.h"
#include "menai_vm_symbol.h"

/*
 * Conversion functions — defined in menai_vm_value.c, called by the C VM.
 *
 * menai_convert_value: translate one slow menai_value.py object to a fast C
 *   type.  Returns a new reference, or NULL on error.
 *
 * menai_convert_code_constants: walk a CodeObject tree, converting all
 *   constants lists in-place from slow to fast types.  Returns the same code
 *   object (borrowed), or NULL on error.  Must be called before
 *   menai_build_closure_caches.
 *
 * menai_build_closure_caches: walk a CodeObject tree, building a ClosureCache
 *   struct for each child code object and storing it as a PyCapsule.  Returns
 *   the same code object (borrowed), or NULL on error.  Must be called after
 *   menai_convert_code_constants.
 */
PyObject *menai_convert_value(PyObject *src);
PyObject *menai_convert_code_constants(PyObject *code);
PyObject *menai_build_closure_caches(PyObject *code);

#endif /* MENAI_VM_VALUE_H */
