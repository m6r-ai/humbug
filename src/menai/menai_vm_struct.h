/*
 * menai_vm_struct.h — MenaiStructType and MenaiStruct type definitions and API.
 *
 * MenaiStructType describes a struct schema (name, tag, field names).  Field
 * names are stored in an inline C array of (name, index) pairs; a MenaiHashTable
 * provides O(1) lookup by name.  The array is retained for ordered enumeration
 * and deallocation; the hash table is used for all name-to-index queries.
 *
 * MenaiStruct is an instance of a MenaiStructType.  Field values are stored
 * in an inline C array (nfields entries), eliminating the Python tuple object
 * that was previously heap-allocated on every struct construction.
 */

#ifndef MENAI_VM_STRUCT_H
#define MENAI_VM_STRUCT_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_value.h"
#include "menai_vm_hashtable.h"
#include "menai_vm_string.h"

/*
 * One entry in the MenaiStructType field-index table.
 * name is an owned MenaiString *; index is the 0-based field position.
 */
typedef struct {
    MenaiValue *name;
    int index;
} MenaiFieldEntry;

typedef struct {
    MenaiValue_HEAD
    MenaiValue *name;            /* owned MenaiString * — struct type name */
    int tag;                     /* unique integer tag */
    int nfields;                 /* number of fields */
    MenaiHashTable field_ht;     /* name -> index hash table; keys are borrowed from fields[] */
    MenaiFieldEntry fields[];    /* inline field-index table, nfields entries */
} MenaiStructType;

typedef struct {
    MenaiValue_HEAD
    int nfields;                 /* number of fields */
    MenaiValue *struct_type;     /* owned reference to MenaiStructType */
    MenaiValue *items[1];        /* inline field values, nfields entries */
} MenaiStruct;

/*
 * menai_struct_field_index — look up a field index by name in O(1).
 * name must be a MenaiString *.  Returns the 0-based index, or -1
 * if not found.
 */
static inline int
menai_struct_field_index(MenaiStructType *st, MenaiValue *name)
{
    Py_hash_t h = menai_string_hash(name);
    return (int)menai_ht_lookup(&st->field_ht, name, h);
}

MenaiValue *menai_struct_alloc(MenaiValue *struct_type, MenaiValue **field_values, Py_ssize_t nfields);
MenaiValue *menai_struct_type_new_from_args(PyObject *args);

#endif /* MENAI_VM_STRUCT_H */
