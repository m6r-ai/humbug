/*
 * menai_vm_hashtable.c — pure-C hash table and value operations.
 *
 * menai_value_hash, menai_value_equal, menai_value_describe, and
 * menai_value_to_python operate directly on MenaiValue C structs, dispatching
 * to the C-level type functions without going through Python method lookup.
 *
 * MenaiHashTable is an open-addressing table with power-of-2 slot counts and
 * a 2/3 maximum load factor.  Probing uses the same quadratic-ish sequence
 * CPython uses: slot = (5*slot + 1 + perturb) & mask, perturb >>= 5.
 * Tables are built once and never mutated (Menai collections are immutable),
 * so there is no deletion or rehashing logic.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdlib.h>
#include <string.h>

#include "menai_vm_hashtable.h"
#include "menai_vm_boolean.h"
#include "menai_vm_complex.h"
#include "menai_vm_float.h"
#include "menai_vm_integer.h"
#include "menai_vm_none.h"
#include "menai_vm_string.h"
#include "menai_vm_struct.h"
#include "menai_vm_symbol.h"
#include "menai_vm_list.h"
#include "menai_vm_dict.h"
#include "menai_vm_set.h"
#include "menai_vm_function.h"

/*
 * Boundary-layer describe/to_python functions — defined in menai_vm_value.c.
 * Forward-declared here so the dispatch tables below can call them without
 * a circular include dependency.
 */
PyObject *menai_value_describe_none(MenaiValue val);
PyObject *menai_value_describe_boolean(MenaiValue val);
PyObject *menai_value_describe_integer(MenaiValue val);
PyObject *menai_value_describe_float(MenaiValue val);
PyObject *menai_value_describe_complex(MenaiValue val);
PyObject *menai_value_describe_string(MenaiValue val);
PyObject *menai_value_describe_symbol(MenaiValue val);
PyObject *menai_value_describe_structtype(MenaiValue val);
PyObject *menai_value_describe_struct(MenaiValue val);
PyObject *menai_value_describe_list(MenaiValue val);
PyObject *menai_value_describe_dict(MenaiValue val);
PyObject *menai_value_describe_set(MenaiValue val);
PyObject *menai_value_describe_function(MenaiValue val);

PyObject *menai_value_to_python_none(MenaiValue val);
PyObject *menai_value_to_python_boolean(MenaiValue val);
PyObject *menai_value_to_python_integer(MenaiValue val);
PyObject *menai_value_to_python_float(MenaiValue val);
PyObject *menai_value_to_python_complex(MenaiValue val);
PyObject *menai_value_to_python_string(MenaiValue val);
PyObject *menai_value_to_python_symbol(MenaiValue val);
PyObject *menai_value_to_python_structtype(MenaiValue val);
PyObject *menai_value_to_python_struct(MenaiValue val);
PyObject *menai_value_to_python_list(MenaiValue val);
PyObject *menai_value_to_python_dict(MenaiValue val);
PyObject *menai_value_to_python_set(MenaiValue val);
PyObject *menai_value_to_python_function(MenaiValue val);

/* Defined in menai_vm_value.c, initialised during _menai_vm_value_init(). */
extern PyObject *MenaiEvalError_type;

/*
 * _menai_short_type_name — return the short lowercase Menai type name for
 * use in error messages (e.g. "string", "integer", "dict").
 *
 * tp_name is "menai.MenaiXxx"; we return the canonical short name instead.
 */
static const char *
_menai_short_type_name(MenaiType *t)
{
    if (t == &MenaiNone_Type) {
        return "none";
    }

    if (t == &MenaiBoolean_Type) {
        return "boolean";
    }

    if (t == &MenaiInteger_Type) {
        return "integer";
    }

    if (t == &MenaiFloat_Type) {
        return "float";
    }

    if (t == &MenaiComplex_Type) {
        return "complex";
    }

    if (t == &MenaiString_Type) {
        return "string";
    }

    if (t == &MenaiSymbol_Type) {
        return "symbol";
    }

    if (t == &MenaiList_Type) {
        return "list";
    }

    if (t == &MenaiDict_Type) {
        return "dict";
    }

    if (t == &MenaiSet_Type) {
        return "set";
    }

    if (t == &MenaiFunction_Type) {
        return "function";
    }

    if (t == &MenaiStructType_Type) {
        return "struct-type";
    }

    if (t == &MenaiStruct_Type) {
        return "struct";
    }

    return t->tp_name;
}

/* ---------------------------------------------------------------------------
 * Hash mixing helpers
 * ------------------------------------------------------------------------- */

static inline Py_uhash_t
_hash_combine(Py_uhash_t acc, Py_hash_t h)
{
    return acc * 1000003UL ^ (Py_uhash_t)h;
}

static inline Py_hash_t
_hash_finalise(Py_uhash_t acc, Py_ssize_t n)
{
    acc ^= (Py_uhash_t)n;
    return (Py_hash_t)(acc == (Py_uhash_t)-1 ? -2 : acc);
}

/* ---------------------------------------------------------------------------
 * menai_value_hash
 * ------------------------------------------------------------------------- */

Py_hash_t
menai_value_hash(MenaiValue val)
{
    MenaiType *t = val->ob_type;

    if (t == &MenaiNone_Type) {
        return (Py_hash_t)0x4e6f6e65UL;
    }

    if (t == &MenaiBoolean_Type) {
        return (Py_hash_t)((MenaiBoolean_Object *)val)->value;
    }

    if (t == &MenaiInteger_Type) {
        MenaiInteger_Object *obj = (MenaiInteger_Object *)val;
        if (!obj->is_big) {
            Py_hash_t h = (Py_hash_t)obj->small;
            return h == -1 ? -2 : h;
        }

        return menai_int_hash(&obj->big);
    }

    if (t == &MenaiFloat_Type) {
        return menai_hash_double(((MenaiFloat_Object *)val)->value);
    }

    if (t == &MenaiComplex_Type) {
        MenaiComplex_Object *c = (MenaiComplex_Object *)val;
        Py_hash_t hr = menai_hash_double(c->real);
        Py_hash_t hi = menai_hash_double(c->imag);
        Py_uhash_t acc = (Py_uhash_t)hr * 1000003UL ^ (Py_uhash_t)hi;
        Py_hash_t h = (Py_hash_t)(acc & (Py_uhash_t)PY_SSIZE_T_MAX);
        return h == -1 ? -2 : h;
    }

    if (t == &MenaiString_Type) {
        return menai_string_hash(val);
    }

    if (t == &MenaiSymbol_Type) {
        return menai_string_hash(((MenaiSymbol_Object *)val)->name);
    }

    if (t == &MenaiStructType_Type) {
        return (Py_hash_t)((MenaiStructType_Object *)val)->tag;
    }

    if (t == &MenaiStruct_Type) {
        MenaiStruct_Object *s = (MenaiStruct_Object *)val;
        int tag = ((MenaiStructType_Object *)s->struct_type)->tag;
        int n = s->nfields;
        Py_uhash_t acc = 0x345678UL ^ (Py_uhash_t)tag;
        for (int i = 0; i < n; i++) {
            Py_hash_t fh = menai_value_hash(s->items[i]);
            if (fh == -1) {
                return -1;
            }

            acc = _hash_combine(acc, fh);
        }

        return _hash_finalise(acc, n);
    }

    PyErr_Format(MenaiEvalError_type,
        "Dict keys must be strings, numbers, booleans, or symbols, got %s",
        _menai_short_type_name(t));
    return -1;
}

/* ---------------------------------------------------------------------------
 * menai_value_equal
 * ------------------------------------------------------------------------- */

int
menai_value_equal(MenaiValue a, MenaiValue b)
{
    if (a == b) {
        return 1;
    }

    MenaiType *ta = a->ob_type;
    MenaiType *tb = b->ob_type;

    if (ta != tb) {
        return 0;
    }

    if (ta == &MenaiNone_Type) {
        return 1;
    }

    if (ta == &MenaiBoolean_Type) {
        return ((MenaiBoolean_Object *)a)->value == ((MenaiBoolean_Object *)b)->value;
    }

    if (ta == &MenaiInteger_Type) {
        MenaiInteger_Object *ia = (MenaiInteger_Object *)a;
        MenaiInteger_Object *ib = (MenaiInteger_Object *)b;
        if (!ia->is_big && !ib->is_big) {
            return ia->small == ib->small;
        }

        if (ia->is_big != ib->is_big) {
            return 0;
        }

        return menai_int_eq(&ia->big, &ib->big);
    }

    if (ta == &MenaiFloat_Type) {
        return ((MenaiFloat_Object *)a)->value == ((MenaiFloat_Object *)b)->value;
    }

    if (ta == &MenaiComplex_Type) {
        MenaiComplex_Object *ca = (MenaiComplex_Object *)a;
        MenaiComplex_Object *cb = (MenaiComplex_Object *)b;
        return ca->real == cb->real && ca->imag == cb->imag;
    }

    if (ta == &MenaiString_Type) {
        return menai_string_equal(a, b);
    }

    if (ta == &MenaiSymbol_Type) {
        return menai_string_equal(((MenaiSymbol_Object *)a)->name, ((MenaiSymbol_Object *)b)->name);
    }

    if (ta == &MenaiStructType_Type) {
        return ((MenaiStructType_Object *)a)->tag == ((MenaiStructType_Object *)b)->tag;
    }

    if (ta == &MenaiStruct_Type) {
        MenaiStruct_Object *sa = (MenaiStruct_Object *)a;
        MenaiStruct_Object *sb = (MenaiStruct_Object *)b;
        if (((MenaiStructType_Object *)sa->struct_type)->tag != ((MenaiStructType_Object *)sb->struct_type)->tag) {
            return 0;
        }

        int n = sa->nfields;
        if (n != sb->nfields) {
            return 0;
        }

        for (int i = 0; i < n; i++) {
            if (!menai_value_equal(sa->items[i], sb->items[i])) {
                return 0;
            }
        }

        return 1;
    }

    if (ta == &MenaiList_Type) {
        MenaiList_Object *la = (MenaiList_Object *)a;
        MenaiList_Object *lb = (MenaiList_Object *)b;
        if (la->length != lb->length) {
            return 0;
        }

        for (Py_ssize_t i = 0; i < la->length; i++) {
            if (!menai_value_equal(la->elements[i], lb->elements[i])) {
                return 0;
            }
        }

        return 1;
    }

    if (ta == &MenaiDict_Type) {
        MenaiDict_Object *da = (MenaiDict_Object *)a;
        MenaiDict_Object *db = (MenaiDict_Object *)b;
        if (da->length != db->length) {
            return 0;
        }

        for (Py_ssize_t i = 0; i < da->length; i++) {
            if (da->hashes[i] != db->hashes[i]) {
                return 0;
            }

            if (!menai_value_equal(da->keys[i], db->keys[i])) {
                return 0;
            }

            if (!menai_value_equal(da->values[i], db->values[i])) {
                return 0;
            }
        }

        return 1;
    }

    if (ta == &MenaiSet_Type) {
        MenaiSet_Object *sa = (MenaiSet_Object *)a;
        MenaiSet_Object *sb = (MenaiSet_Object *)b;
        if (sa->length != sb->length) {
            return 0;
        }

        for (Py_ssize_t i = 0; i < sa->length; i++) {
            if (menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]) == -1) {
                return 0;
            }
        }

        return 1;
    }

    return 0;
}

/* ---------------------------------------------------------------------------
 * menai_value_describe
 * ------------------------------------------------------------------------- */

PyObject *
menai_value_describe(MenaiValue val)
{
    MenaiType *t = val->ob_type;

    if (t == &MenaiNone_Type) {
        return menai_value_describe_none(val);
    }

    if (t == &MenaiBoolean_Type) {
        return menai_value_describe_boolean(val);
    }

    if (t == &MenaiInteger_Type) {
        return menai_value_describe_integer(val);
    }

    if (t == &MenaiFloat_Type) {
        return menai_value_describe_float(val);
    }

    if (t == &MenaiComplex_Type) {
        return menai_value_describe_complex(val);
    }

    if (t == &MenaiString_Type) {
        return menai_value_describe_string(val);
    }

    if (t == &MenaiSymbol_Type) {
        return menai_value_describe_symbol(val);
    }

    if (t == &MenaiStructType_Type) {
        return menai_value_describe_structtype(val);
    }

    if (t == &MenaiStruct_Type) {
        return menai_value_describe_struct(val);
    }

    if (t == &MenaiList_Type) {
        return menai_value_describe_list(val);
    }

    if (t == &MenaiDict_Type) {
        return menai_value_describe_dict(val);
    }

    if (t == &MenaiSet_Type) {
        return menai_value_describe_set(val);
    }

    if (t == &MenaiFunction_Type) {
        return menai_value_describe_function(val);
    }

    PyErr_Format(PyExc_TypeError, "menai_value_describe: unknown type %s", _menai_short_type_name(t));
    return NULL;
}

/* ---------------------------------------------------------------------------
 * menai_value_to_python
 * ------------------------------------------------------------------------- */

PyObject *
menai_value_to_python(MenaiValue val)
{
    MenaiType *t = val->ob_type;

    if (t == &MenaiNone_Type) {
        return menai_value_to_python_none(val);
    }

    if (t == &MenaiBoolean_Type) {
        return menai_value_to_python_boolean(val);
    }

    if (t == &MenaiInteger_Type) {
        return menai_value_to_python_integer(val);
    }

    if (t == &MenaiFloat_Type) {
        return menai_value_to_python_float(val);
    }

    if (t == &MenaiComplex_Type) {
        return menai_value_to_python_complex(val);
    }

    if (t == &MenaiString_Type) {
        return menai_value_to_python_string(val);
    }

    if (t == &MenaiSymbol_Type) {
        return menai_value_to_python_symbol(val);
    }

    if (t == &MenaiStructType_Type) {
        return menai_value_to_python_structtype(val);
    }

    if (t == &MenaiStruct_Type) {
        return menai_value_to_python_struct(val);
    }

    if (t == &MenaiList_Type) {
        return menai_value_to_python_list(val);
    }

    if (t == &MenaiDict_Type) {
        return menai_value_to_python_dict(val);
    }

    if (t == &MenaiSet_Type) {
        return menai_value_to_python_set(val);
    }

    if (t == &MenaiFunction_Type) {
        return menai_value_to_python_function(val);
    }

    PyErr_Format(PyExc_TypeError, "menai_value_to_python: unknown type %s", _menai_short_type_name(t));
    return NULL;
}

/* ---------------------------------------------------------------------------
 * MenaiHashTable
 * ------------------------------------------------------------------------- */

static Py_ssize_t
_ht_slot_count(Py_ssize_t n)
{
    if (n == 0) {
        return 0;
    }

    Py_ssize_t min_slots =
        (n * MENAI_HT_MAX_LOAD_DEN + MENAI_HT_MAX_LOAD_NUM - 1)
        / MENAI_HT_MAX_LOAD_NUM;
    Py_ssize_t sc = 4;
    while (sc < min_slots) {
        sc <<= 1;
    }

    return sc;
}

int
menai_ht_init(MenaiHashTable *ht, Py_ssize_t n)
{
    Py_ssize_t sc = _ht_slot_count(n);
    if (sc == 0) {
        ht->slots = NULL;
        ht->slot_count = 0;
        ht->used = 0;
        return 0;
    }

    ht->slots = (MenaiHashSlot *)malloc((size_t)sc * sizeof(MenaiHashSlot));
    if (!ht->slots) {
        PyErr_NoMemory();
        return -1;
    }

    memset(ht->slots, 0, (size_t)sc * sizeof(MenaiHashSlot));
    ht->slot_count = sc;
    ht->used = 0;
    return 0;
}

void
menai_ht_free(MenaiHashTable *ht)
{
    free(ht->slots);
    ht->slots = NULL;
    ht->slot_count = 0;
    ht->used = 0;
}

Py_ssize_t
menai_ht_lookup(const MenaiHashTable *ht, MenaiValue key, Py_hash_t hash)
{
    if (ht->slot_count == 0) {
        return -1;
    }

    Py_ssize_t mask = ht->slot_count - 1;
    Py_uhash_t perturb = (Py_uhash_t)hash;
    Py_ssize_t slot = (Py_ssize_t)(perturb & (Py_uhash_t)mask);

    for (;;) {
        MenaiHashSlot *s = &ht->slots[slot];
        if (s->key == NULL) {
            return -1;
        }

        if (s->hash == hash && menai_value_equal(s->key, key)) {
            return s->index;
        }

        perturb >>= 5;
        slot = (Py_ssize_t)((5 * (Py_uhash_t)slot + 1 + perturb) & (Py_uhash_t)mask);
    }
}

void
menai_ht_insert(MenaiHashTable *ht, MenaiValue key, Py_hash_t hash, Py_ssize_t index)
{
    Py_ssize_t mask = ht->slot_count - 1;
    Py_uhash_t perturb = (Py_uhash_t)hash;
    Py_ssize_t slot = (Py_ssize_t)(perturb & (Py_uhash_t)mask);

    for (;;) {
        MenaiHashSlot *s = &ht->slots[slot];
        if (s->key == NULL) {
            s->key = key;
            s->hash = hash;
            s->index = index;
            ht->used++;
            return;
        }

        perturb >>= 5;
        slot = (Py_ssize_t)((5 * (Py_uhash_t)slot + 1 + perturb) & (Py_uhash_t)mask);
    }
}

int
menai_ht_build(MenaiHashTable *ht, MenaiValue *keys, const Py_hash_t *hashes,
               Py_ssize_t n)
{
    if (menai_ht_init(ht, n) < 0) {
        return -1;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        menai_ht_insert(ht, keys[i], hashes[i], i);
    }

    return 0;
}
