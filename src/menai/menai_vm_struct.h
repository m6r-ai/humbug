/*
 * menai_vm_struct.h — MenaiStructType and MenaiStruct type definitions and API.
 *
 * MenaiStructType describes a struct schema (name, tag, field names).  The
 * field lookup table is stored as an inline C array of (interned name pointer,
 * index) pairs rather than a Python dict, so field lookup by symbol name
 * reduces to a pointer-comparison linear scan — faster than a dict lookup for
 * the small field counts typical in Menai structs.
 *
 * MenaiStruct is an instance of a MenaiStructType.  Field values are stored
 * in an inline C array (nfields entries), eliminating the Python tuple object
 * that was previously heap-allocated on every struct construction.
 *
 * The name, tag, and field_names fields on MenaiStructType_Object remain as
 * PyObject * because they originate from Python source and field_names is
 * returned directly to Python by OP_STRUCT_FIELDS.
 */

#ifndef MENAI_VM_STRUCT_H
#define MENAI_VM_STRUCT_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_object.h"

/*
 * One entry in the MenaiStructType field-index table.
 * name is an interned PyUnicode object; index is the 0-based field position.
 */
typedef struct {
    PyObject *name;
    int index;
} MenaiFieldEntry;

typedef struct {
    MenaiObject_HEAD
    PyObject *name;             /* Python str — struct type name */
    int tag;                    /* unique integer tag */
    PyObject *field_names;      /* Python tuple of str — kept for OP_STRUCT_FIELDS */
    int nfields;                /* number of fields */
    MenaiFieldEntry fields[];   /* inline field-index table, nfields entries */
} MenaiStructType_Object;

typedef struct {
    MenaiObject_HEAD
    int nfields;                /* number of fields */
    MenaiValue struct_type;     /* owned reference to MenaiStructType_Object */
    MenaiValue items[1];        /* inline field values, nfields entries */
} MenaiStruct_Object;

extern MenaiType MenaiStructType_Type;
extern MenaiType MenaiStruct_Type;

/*
 * menai_struct_field_index — look up a field by interned name pointer.
 * Returns the 0-based index, or -1 if not found.
 */
static inline int
menai_struct_field_index(MenaiStructType_Object *st, PyObject *name)
{
    int n = st->nfields;
    MenaiFieldEntry *fe = st->fields;
    for (int i = 0; i < n; i++) {
        if (fe[i].name == name) return fe[i].index;
    }

    return -1;
}

/*
 * menai_struct_alloc — direct C constructor for MenaiStruct.
 *
 * struct_type is borrowed (retain'd internally).  field_values is an array
 * of nfields borrowed references — each is retain'd into the inline array.
 * Returns a new reference, or NULL on error.
 */
MenaiValue menai_struct_alloc(MenaiValue struct_type, MenaiValue *field_values,
                              Py_ssize_t nfields);

/*
 * menai_struct_type_new_from_args — public wrapper used by menai_convert_value.
 * args is a positional Python tuple (name, tag, field_names).
 * Returns a new reference.
 */
MenaiValue menai_struct_type_new_from_args(PyObject *args);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure.
 */
int menai_vm_struct_init(void);

#endif /* MENAI_VM_STRUCT_H */
