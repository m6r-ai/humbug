/*
 * menai_vm_bridge.h — C struct definitions for all Menai runtime value types.
 *
 * All types are defined in menai_vm_bridge.c and exposed via the
 * menai_vm_bridge module.  The VM imports that module at init time to obtain
 * the type objects and singleton values; after that it uses these structs
 * directly.
 */
#ifndef MENAI_VM_BRIDGE_H
#define MENAI_VM_BRIDGE_H

MenaiValue *menai_convert_value(PyObject *src);
PyObject *menai_value_to_slow_value(MenaiValue *raw);
int menai_vm_bridge_init(void);

#endif /* MENAI_VM_BRIDGE_H */
