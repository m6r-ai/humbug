/*
 * menai_vm_hashtable.c — pure-C hash table and value hash/equality.
 *
 * menai_value_hash and menai_value_equal operate directly on MenaiValue C
 * structs without allocating any Python objects, removing the Python dict and
 * frozenset dependencies from MenaiDict and MenaiSet operations.
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

    /* MenaiInteger — delegate to PyLong hash (unavoidable: arbitrary precision) */
    if (t == &MenaiInteger_Type) {
        Py_hash_t h = PyObject_Hash(((MenaiInteger_Object *)val)->value);
        return h;
    }

    /* MenaiFloat — use _Py_HashDouble directly to avoid boxing */
    if (t == &MenaiFloat_Type) {
        Py_hash_t h = _Py_HashDouble(val, ((MenaiFloat_Object *)val)->value);
        return h;
    }

    /* MenaiComplex — combine real and imag hashes */
    if (t == &MenaiComplex_Type) {
        MenaiComplex_Object *c = (MenaiComplex_Object *)val;
        Py_hash_t hr = _Py_HashDouble(val, c->real);
        if (hr == -1) return -1;
        Py_hash_t hi = _Py_HashDouble(val, c->imag);
        if (hi == -1) return -1;
        Py_uhash_t acc = 0x636f6d70UL;  /* "comp" */
        acc = _hash_combine(acc, hr);
        acc = _hash_combine(acc, hi);
        return _hash_finalise(acc, 2);
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
        /* PyObject_RichCompareBool is unavoidable for arbitrary precision */
        return PyObject_RichCompareBool(((MenaiInteger_Object *)a)->value, ((MenaiInteger_Object *)b)->value, Py_EQ);
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
            if (eq <= 0) return eq;
        }
        return 1;
    }

    if (ta == &MenaiList_Type) {
        MenaiList_Object *la = (MenaiList_Object *)a;
        MenaiList_Object *lb = (MenaiList_Object *)b;
        if (la->length != lb->length) return 0;
        for (Py_ssize_t i = 0; i < la->length; i++) {
            int eq = menai_value_equal(la->elements[i], lb->elements[i]);
            if (eq <= 0) return eq;
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
            if (keq <= 0) return keq;
            int veq = menai_value_equal(da->values[i], db->values[i]);
            if (veq <= 0) return veq;
        }
        return 1;
    }

    if (ta == &MenaiSet_Type) {
        MenaiSet_Object *sa = (MenaiSet_Object *)a;
        MenaiSet_Object *sb = (MenaiSet_Object *)b;
        if (sa->length != sb->length) return 0;
        for (Py_ssize_t i = 0; i < sa->length; i++) {
            Py_ssize_t idx = menai_ht_lookup(&sb->ht, sa->elements[i], sa->hashes[i]);
            if (idx == -2) return -1;
            if (idx == -1) return 0;
        }
        return 1;
    }

    /* Unhashable types — fall back to pointer equality only */
    return 0;
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
            if (eq < 0) return -2;  /* error */
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
