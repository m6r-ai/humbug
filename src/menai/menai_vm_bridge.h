/*
 * menai_vm_bridge.h — C struct definitions for all Menai runtime value types.
 *
 * All types are defined in menai_vm_bridge.c and exposed via the
 * menai_vm_bridge module.  The VM imports that module at init time to obtain
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

MenaiValue *menai_convert_value(PyObject *src);
PyObject *menai_value_to_slow_value(MenaiValue *raw);
int menai_vm_bridge_init(void);

#endif /* MENAI_VM_VALUE_H */
