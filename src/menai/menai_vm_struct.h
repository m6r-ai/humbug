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
 * in an inline C array (ob_size == nfields), eliminating the Python tuple
 * object that was previously heap-allocated on every struct construction.
 */

#ifndef MENAI_VM_STRUCT_H
#define MENAI_VM_STRUCT_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

/*
 * One entry in the MenaiStructType field-index table.
 * name is an interned PyUnicode object; index is the 0-based field position.
 */
typedef struct {
    PyObject *name;
    int index;
} MenaiFieldEntry;

typedef struct {
    PyObject_HEAD
    PyObject *name;             /* Python str — struct type name */
    int tag;                    /* unique integer tag */
    PyObject *field_names;      /* Python tuple of str — kept for OP_STRUCT_FIELDS */
    int nfields;                /* number of fields */
    MenaiFieldEntry fields[];   /* inline field-index table, nfields entries */
} MenaiStructType_Object;

typedef struct {
    PyObject_VAR_HEAD              /* ob_size == number of fields */
    PyObject *struct_type;         /* MenaiStructType_Object* */
    PyObject *items[1];            /* inline field values, ob_size entries */
} MenaiStruct_Object;

extern PyTypeObject MenaiStructType_Type;
extern PyTypeObject MenaiStruct_Type;

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
 * struct_type is borrowed (INCREF'd internally).  field_values is an array
 * of nfields borrowed references — each is INCREF'd into the inline array.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_struct_alloc(PyObject *struct_type, PyObject **field_values, Py_ssize_t nfields);

/*
 * menai_struct_type_new_from_args — public wrapper used by menai_convert_value.
 * args is a positional tuple (name, tag, field_names).  Returns new reference.
 */
PyObject *menai_struct_type_new_from_args(PyObject *args);

/*
 * Module init — called once from _menai_vm_value_init().
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_struct_init(void);

#endif /* MENAI_VM_STRUCT_H */
