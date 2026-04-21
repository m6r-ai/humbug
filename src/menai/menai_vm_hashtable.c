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

/* Defined (non-static) in menai_vm_value.c, initialised during
 * _menai_vm_value_init(). */
extern PyObject *MenaiEvalError_type;

/* ---------------------------------------------------------------------------
 * Hash mixing helpers
 * ------------------------------------------------------------------------- */

/*
 * _hash_combine — combine two hash values using the same multiplier CPython
 * uses for tuple hashing (1000003).
 */
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
menai_value_hash(PyObject *val)
{
    PyTypeObject *t = Py_TYPE(val);

    /* MenaiNone — singleton, use a fixed hash */
    if (t == &MenaiNone_Type)
        return (Py_hash_t)0x4e6f6e65UL;  /* "None" */

    /* MenaiBoolean — hash 0 or 1, matching Python's True/False hash */
    if (t == &MenaiBoolean_Type)
        return (Py_hash_t)((MenaiBoolean_Object *)val)->value;

    /* MenaiInteger — inline fast path for small values; bigint path for large. */
    if (t == &MenaiInteger_Type) {
        MenaiInteger_Object *obj = (MenaiInteger_Object *)val;
        if (!obj->is_big) {
            /*
             * CPython's hash of a small integer is the value itself, with
             * -1 mapped to -2 (the unhashable sentinel is never a valid hash).
             */
            Py_hash_t h = (Py_hash_t)obj->small;
            return h == -1 ? -2 : h;
        }
        return menai_int_hash(&obj->big);
    }

    /* MenaiFloat */
    if (t == &MenaiFloat_Type) {
        return menai_hash_double(((MenaiFloat_Object *)val)->value);
    }

    /* MenaiComplex */
    if (t == &MenaiComplex_Type) {
        MenaiComplex_Object *c = (MenaiComplex_Object *)val;
        Py_hash_t hr = menai_hash_double(c->real);
        Py_hash_t hi = menai_hash_double(c->imag);
        Py_uhash_t acc = (Py_uhash_t)hr * 1000003UL ^ (Py_uhash_t)hi;
        Py_hash_t h = (Py_hash_t)(acc & (Py_uhash_t)PY_SSIZE_T_MAX);
        return h == -1 ? -2 : h;
    }

    /* MenaiString — use cached FNV-1a hash */
    if (t == &MenaiString_Type)
        return menai_string_hash(val);

    /* MenaiSymbol — interned name pointer: use pointer hash */
    if (t == &MenaiSymbol_Type) {
        PyObject *name = ((MenaiSymbol_Object *)val)->name;
        Py_uhash_t p = (Py_uhash_t)(uintptr_t)name;
        /* Mix the pointer to reduce clustering */
        p ^= p >> 4;
        p *= 0x9e3779b97f4a7c15ULL;
        p ^= p >> 27;
        Py_hash_t h = (Py_hash_t)(p & (Py_uhash_t)PY_SSIZE_T_MAX);
        return h == -1 ? -2 : h;
    }

    /* MenaiStructType — use tag */
    if (t == &MenaiStructType_Type)
        return (Py_hash_t)((MenaiStructType_Object *)val)->tag;

    /* MenaiStruct — combine tag with field hashes (recursive) */
    if (t == &MenaiStruct_Type) {
        MenaiStruct_Object *s = (MenaiStruct_Object *)val;
        int tag = ((MenaiStructType_Object *)s->struct_type)->tag;
        Py_ssize_t n = Py_SIZE(s);
        Py_uhash_t acc = 0x345678UL ^ (Py_uhash_t)tag;
        for (Py_ssize_t i = 0; i < n; i++) {
            Py_hash_t fh = menai_value_hash(s->items[i]);
            if (fh == -1) return -1;
            acc = _hash_combine(acc, fh);
        }
        return _hash_finalise(acc, n);
    }

    /* Unhashable types */
    PyErr_Format(MenaiEvalError_type,
        "Dict keys must be strings, numbers, booleans, or symbols, got %s",
        t->tp_name);
    return -1;
}

/* ---------------------------------------------------------------------------
 * menai_value_equal
 * ------------------------------------------------------------------------- */

int
menai_value_equal(PyObject *a, PyObject *b)
{
    /* Pointer identity is always equal */
    if (a == b) return 1;

    PyTypeObject *ta = Py_TYPE(a);
    PyTypeObject *tb = Py_TYPE(b);

    /* Different types are never equal */
    if (ta != tb) return 0;

    if (ta == &MenaiNone_Type) return 1;  /* singleton */

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
            /* One small, one big — a long value can never equal a bignum. */
            return 0;
        }
        /* Both big. */
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
        /* Symbols are interned — pointer equality suffices */
        return ((MenaiSymbol_Object *)a)->name == ((MenaiSymbol_Object *)b)->name;
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
        Py_ssize_t n = Py_SIZE(sa);
        if (n != Py_SIZE(sb)) return 0;
        for (Py_ssize_t i = 0; i < n; i++) {
            int eq = menai_value_equal(sa->items[i], sb->items[i]);
            if (!eq) return 0;
        }
        return 1;
    }

    if (ta == &MenaiList_Type) {
        MenaiList_Object *la = (MenaiList_Object *)a;
        MenaiList_Object *lb = (MenaiList_Object *)b;
        if (la->length != lb->length) return 0;
        for (Py_ssize_t i = 0; i < la->length; i++) {
            int eq = menai_value_equal(la->elements[i], lb->elements[i]);
            if (!eq) return 0;
        }
        return 1;
    }

    if (ta == &MenaiDict_Type) {
        MenaiDict_Object *da = (MenaiDict_Object *)a;
        MenaiDict_Object *db = (MenaiDict_Object *)b;
        if (da->length != db->length) return 0;
        for (Py_ssize_t i = 0; i < da->length; i++) {
            if (da->hashes[i] != db->hashes[i]) return 0;
            int keq = menai_value_equal(da->keys[i], db->keys[i]);
            if (!keq) return 0;
            int veq = menai_value_equal(da->values[i], db->values[i]);
            if (!veq) return 0;
        }
        return 1;
    }

    if (ta == &MenaiSet_Type) {
        MenaiSet_Object *sa = (MenaiSet_Object *)a;
        MenaiSet_Object *sb = (MenaiSet_Object *)b;
        if (sa->length != sb->length) return 0;
        for (Py_ssize_t i = 0; i < sa->length; i++) {
            Py_ssize_t idx = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
            if (idx == -1) return 0;
        }
        return 1;
    }

    /* Unhashable types — fall back to pointer equality only */
    return 0;
}

/* ---------------------------------------------------------------------------
 * menai_value_describe
 * ------------------------------------------------------------------------- */

PyObject *
menai_value_describe(PyObject *val)
{
    PyTypeObject *t = Py_TYPE(val);

    if (t == &MenaiNone_Type)       return MenaiNone_describe(val, NULL);
    if (t == &MenaiBoolean_Type)    return MenaiBoolean_describe(val, NULL);
    if (t == &MenaiInteger_Type)    return MenaiInteger_describe(val, NULL);
    if (t == &MenaiFloat_Type)      return MenaiFloat_describe(val, NULL);
    if (t == &MenaiComplex_Type)    return MenaiComplex_describe(val, NULL);
    if (t == &MenaiString_Type)     return MenaiString_describe(val, NULL);
    if (t == &MenaiSymbol_Type)     return MenaiSymbol_describe(val, NULL);
    if (t == &MenaiStructType_Type) return MenaiStructType_describe(val, NULL);
    if (t == &MenaiStruct_Type)     return MenaiStruct_describe(val, NULL);
    if (t == &MenaiList_Type)       return MenaiList_describe(val, NULL);
    if (t == &MenaiDict_Type)       return MenaiDict_describe(val, NULL);
    if (t == &MenaiSet_Type)        return MenaiSet_describe(val, NULL);
    if (t == &MenaiFunction_Type)   return MenaiFunction_describe(val, NULL);

    PyErr_Format(PyExc_TypeError, "menai_value_describe: unknown type %s",
                 t->tp_name);
    return NULL;
}

/* ---------------------------------------------------------------------------
 * menai_value_to_python
 * ------------------------------------------------------------------------- */

PyObject *
menai_value_to_python(PyObject *val)
{
    PyTypeObject *t = Py_TYPE(val);

    if (t == &MenaiNone_Type)       return MenaiNone_to_python(val, NULL);
    if (t == &MenaiBoolean_Type)    return MenaiBoolean_to_python(val, NULL);
    if (t == &MenaiInteger_Type)    return MenaiInteger_to_python(val, NULL);
    if (t == &MenaiFloat_Type)      return MenaiFloat_to_python(val, NULL);
    if (t == &MenaiComplex_Type)    return MenaiComplex_to_python(val, NULL);
    if (t == &MenaiString_Type)     return MenaiString_to_python(val, NULL);
    if (t == &MenaiSymbol_Type)     return MenaiSymbol_to_python(val, NULL);
    if (t == &MenaiStructType_Type) return MenaiStructType_to_python(val, NULL);
    if (t == &MenaiStruct_Type)     return MenaiStruct_to_python(val, NULL);
    if (t == &MenaiList_Type)       return MenaiList_to_python(val, NULL);
    if (t == &MenaiDict_Type)       return MenaiDict_to_python(val, NULL);
    if (t == &MenaiSet_Type)        return MenaiSet_to_python(val, NULL);
    if (t == &MenaiFunction_Type)   return MenaiFunction_to_python(val, NULL);

    PyErr_Format(PyExc_TypeError, "menai_value_to_python: unknown type %s",
                 t->tp_name);
    return NULL;
}

/* ---------------------------------------------------------------------------
 * MenaiHashTable
 * ------------------------------------------------------------------------- */

/*
 * _ht_slot_count — smallest power of 2 such that
 *   slot_count * MENAI_HT_MAX_LOAD_NUM / MENAI_HT_MAX_LOAD_DEN >= n.
 * Returns 0 for n == 0.
 */
static Py_ssize_t
_ht_slot_count(Py_ssize_t n)
{
    if (n == 0) return 0;
    /* Minimum slots needed: ceil(n * DEN / NUM) */
    Py_ssize_t min_slots =
        (n * MENAI_HT_MAX_LOAD_DEN + MENAI_HT_MAX_LOAD_NUM - 1)
        / MENAI_HT_MAX_LOAD_NUM;
    Py_ssize_t sc = 4;
    while (sc < min_slots) sc <<= 1;
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

    ht->slots = (MenaiHashSlot *)PyMem_Malloc(sc * sizeof(MenaiHashSlot));
    if (!ht->slots) {
        PyErr_NoMemory();
        return -1;
    }

    memset(ht->slots, 0, sc * sizeof(MenaiHashSlot));
    ht->slot_count = sc;
    ht->used = 0;
    return 0;
}

void
menai_ht_free(MenaiHashTable *ht)
{
    PyMem_Free(ht->slots);
    ht->slots = NULL;
    ht->slot_count = 0;
    ht->used = 0;
}

Py_ssize_t
menai_ht_lookup(const MenaiHashTable *ht, PyObject *key, Py_hash_t hash)
{
    if (ht->slot_count == 0) return -1;

    Py_ssize_t mask = ht->slot_count - 1;
    Py_uhash_t perturb = (Py_uhash_t)hash;
    Py_ssize_t slot = (Py_ssize_t)(perturb & (Py_uhash_t)mask);

    for (;;) {
        MenaiHashSlot *s = &ht->slots[slot];
        if (s->key == NULL)
            return -1;  /* empty slot — key not present */
        if (s->hash == hash) {
            int eq = menai_value_equal(s->key, key);
            if (eq) return s->index;
        }
        /* Probe: same sequence CPython uses */
        perturb >>= 5;
        slot = (Py_ssize_t)((5 * (Py_uhash_t)slot + 1 + perturb) & (Py_uhash_t)mask);
    }
}

void
menai_ht_insert(MenaiHashTable *ht, PyObject *key, Py_hash_t hash, Py_ssize_t index)
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
menai_ht_build(MenaiHashTable *ht, PyObject **keys, const Py_hash_t *hashes, Py_ssize_t n)
{
    if (menai_ht_init(ht, n) < 0) return -1;
    for (Py_ssize_t i = 0; i < n; i++) menai_ht_insert(ht, keys[i], hashes[i], i);
    return 0;
}
