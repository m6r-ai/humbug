/*
 * menai_vm_struct.h — MenaiStructType and MenaiStruct type definitions and API.
 *
 * MenaiStructType describes a struct schema (name, tag, field names).
 * MenaiStruct is an instance of a MenaiStructType holding a tuple of fields.
 *
 * Also provides menai_struct_alloc(), the direct C constructor used by the VM.
 */

#ifndef MENAI_VM_STRUCT_H
#define MENAI_VM_STRUCT_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

typedef struct {
    PyObject_HEAD
    PyObject *name;         /* Python str */
    int tag;                /* unique integer tag */
    PyObject *field_names;  /* Python tuple of str */
    PyObject *_field_index; /* Python dict: str -> int */
} MenaiStructType_Object;

typedef struct {
    PyObject_HEAD
    PyObject *struct_type;  /* MenaiStructType_Object* */
    PyObject *fields;       /* Python tuple of MenaiValue* */
} MenaiStruct_Object;

extern PyTypeObject MenaiStructType_Type;
extern PyTypeObject MenaiStruct_Type;

/*
 * menai_struct_alloc — direct C constructor for MenaiStruct.
 * struct_type is borrowed (INCREF'd internally); fields_tup is stolen.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_struct_alloc(PyObject *struct_type, PyObject *fields_tup);

/*
 * menai_struct_type_new_from_args — public wrapper used by menai_convert_value.
 * args is a positional tuple (name, tag, field_names).  Returns new reference.
 */
PyObject *menai_struct_type_new_from_args(PyObject *args);

/*
 * menai_struct_new_from_fast — build a MenaiStruct from already-fast values.
 * fast_st is borrowed; fast_fields_tup is stolen.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_struct_new_from_fast(PyObject *fast_st, PyObject *fast_fields_tup);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_struct_init(void);

#endif /* MENAI_VM_STRUCT_H */
