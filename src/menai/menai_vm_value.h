/*
 * menai_vm_value.h — C struct definitions for all Menai runtime value types.
 *
 * Included by menai_vm_shim.h and menai_value_c.c.  Provides the concrete
 * PyObject struct layouts so that the C VM can access fields directly by cast
 * rather than via PyObject_GetAttrString.
 *
 * All types are defined in menai_value_c.c and exposed via the
 * menai_value_c module.  The VM imports that module at init time to obtain
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
 * Conversion functions — defined in menai_value_c.c, called by the C VM
 *
 * convert_value: translate one slow menai_value.py object to a fast C type.
 *   Returns a new reference, or NULL on error.
 *
 * convert_code_object: walk a CodeObject tree, converting all constants
 *   in-place.  Returns the same code object (borrowed), or NULL on error.
 *
 * to_slow: translate one fast C value back to a slow menai_value.py object.
 *   Returns a new reference, or NULL on error.
 */
PyObject *menai_convert_value(PyObject *src);
PyObject *menai_convert_code_object(PyObject *code);
PyObject *menai_to_slow(PyObject *src);

#endif /* MENAI_VM_VALUE_H */
