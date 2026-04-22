/*
 * menai_vm_struct.h — MenaiStructType and MenaiStruct type definitions and API.
 *
 * MenaiStructType describes a struct schema (name, tag, field names).  The
 * field lookup table is stored as an inline C array of (interned name pointer,
 * index) pairs rather than a Python dict, so field lookup by symbol name
 * reduces to a menai_string_equal() linear scan — fast for the small field
 * counts typical in Menai structs.
 *
 * MenaiStruct is an instance of a MenaiStructType.  Field values are stored
 * in an inline C array (nfields entries), eliminating the Python tuple object
 * that was previously heap-allocated on every struct construction.
 */

#ifndef MENAI_VM_STRUCT_H
#define MENAI_VM_STRUCT_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_object.h"
#include "menai_vm_string.h"

/*
 * One entry in the MenaiStructType field-index table.
 * name is an owned MenaiString_Object *; index is the 0-based field position.
 */
typedef struct {
    MenaiValue name;
    int index;
} MenaiFieldEntry;

typedef struct {
    MenaiObject_HEAD
    MenaiValue name;            /* owned MenaiString_Object * — struct type name */
    int tag;                    /* unique integer tag */
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
 * name must be a MenaiString_Object *.  Returns the 0-based index, or -1 if
 * not found.
 */
static inline int
menai_struct_field_index(MenaiStructType_Object *st, MenaiValue name)
{
    int n = st->nfields;
    MenaiFieldEntry *fe = st->fields;
    for (int i = 0; i < n; i++) {
        if (menai_string_equal(fe[i].name, name)) {
            return fe[i].index;
        }
    }

    return -1;
}

/*
 * menai_struct_alloc — direct C constructor for MenaiStruct.
 *
 * struct_type is borrowed (retained internally).  field_values is an array
 * of nfields borrowed references — each is retained into the inline array.
 * Returns a new reference, or NULL on error.
 */
MenaiValue menai_struct_alloc(MenaiValue struct_type, MenaiValue *field_values,
                              Py_ssize_t nfields);

/*
 * menai_struct_type_new_from_args — public wrapper used by menai_convert_value.
 * args is a positional Python tuple (name: PyUnicode, tag: int, field_names: sequence of PyUnicode).
 * Converts strings to MenaiString internally.  Returns a new reference.
 */
MenaiValue menai_struct_type_new_from_args(PyObject *args);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure.
 */
int menai_vm_struct_init(void);

#endif /* MENAI_VM_STRUCT_H */
