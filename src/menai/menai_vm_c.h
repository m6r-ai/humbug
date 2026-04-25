/*
 * menai_vm_c.h
 */
#ifndef MENAI_VM_C_H
#define MENAI_VM_C_H

#include <sys/types.h>
#include <stddef.h>
#include <stdint.h>
#include <assert.h>

#ifdef _MSC_VER
typedef ptrdiff_t ssize_t;
#define SSIZE_MAX PTRDIFF_MAX
#endif

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#if defined(__GNUC__) || defined(__clang__)
#define MENAI_LIKELY(x) __builtin_expect(!!(x), 1)
#define MENAI_UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define MENAI_LIKELY(x) (x)
#define MENAI_UNLIKELY(x) (x)
#endif

typedef struct MenaiValue_s MenaiValue;

/*
 * MenaiType — the type tag for a Menai value.  uint16_t is sufficient for
 * the 13 current types and leaves room for future additions.  The values are
 * chosen to be distinct and non-zero so that ob_type == 0 reliably detects
 * use-after-free (the allocator poisons freed blocks with ob_type = 0).
 */
typedef uint16_t MenaiType;

#define MENAITYPE_NONE 0x0001
#define MENAITYPE_BOOLEAN 0x0002
#define MENAITYPE_FUNCTION 0x0003
#define MENAITYPE_SYMBOL 0x0004
#define MENAITYPE_STRING 0x0005
#define MENAITYPE_INTEGER 0x0006
#define MENAITYPE_FLOAT 0x0007
#define MENAITYPE_COMPLEX 0x0008
#define MENAITYPE_LIST 0x0009
#define MENAITYPE_DICT 0x000a
#define MENAITYPE_SET 0x000b
#define MENAITYPE_STRUCT 0x000c
#define MENAITYPE_STRUCTTYPE 0x000d

/*
 * MenaiValue_HEAD — common prefix for every Menai value struct.
 *
 * ob_refcnt    — reference count.
 * ob_type      — type tag (MenaiType, uint16_t).
 * ob_alloc     — pool bucket number if this object was served from the
 *                pool allocator, or -1 if it was allocated directly via malloc.
 *                Written by menai_alloc; read by menai_free to determine how
 *                to return the block.
 */
#define MenaiValue_HEAD              \
    uint32_t ob_refcnt;              \
    MenaiType ob_type;               \
    int16_t ob_alloc_bucket;

/*
 * MenaiValue — the minimal struct that every MenaiValue pointer can be safely cast to
 */
struct MenaiValue_s {
    MenaiValue_HEAD
};

const char *menai_short_type_name(MenaiType t);
void menai_value_dealloc(MenaiValue *v);

/*
 * menai_retain — claim an interest in val.
 */
static inline void
menai_retain(MenaiValue *val)
{
    assert(val->ob_type != 0);
    val->ob_refcnt++;
}

/*
 * menai_release — relinquish an interest in val.
 *
 * val must not be NULL.  When ob_refcnt reaches zero, we call the registered destructor.
 */
static inline void
menai_release(MenaiValue *val)
{
    assert(val->ob_type != 0);
    if (--val->ob_refcnt == 0) {
        menai_value_dealloc(val);
    }
}

/*
 * menai_xrelease — relinquish an interest in val if val is non-NULL.
 */
static inline void
menai_xrelease(MenaiValue *val)
{
    if (val != NULL) {
        menai_release(val);
    }
}

/*
 * menai_is_unique — return non-zero if val has exactly one live reference.
 */
static inline int
menai_is_unique(MenaiValue *val)
{
    return val->ob_refcnt == 1;
}

typedef ssize_t hash_t;
typedef size_t uhash_t;

/*
 * menai_hash_double — hash a C double without any Python API calls.
 *
 * Reinterprets the IEEE 754 bit pattern as a uint64_t via memcpy (safe
 * under strict aliasing rules) then applies a finalisation mix so that
 * nearby values produce well-distributed hashes.  NaN is normalised to a
 * fixed bit pattern before mixing so all NaN values hash identically.
 * The result is mapped away from -1 (the CPython "error" sentinel).
 *
 * This is a Menai-internal hash — it does not need to match Python's
 * float hash, because Menai floats and integers are never equal and are
 * never mixed in the same dict or set.
 */
static inline hash_t
menai_hash_double(double v)
{
    uint64_t bits;
    if (v != v) {
        bits = 0x7FF8000000000000ULL;
    } else {
        memcpy(&bits, &v, sizeof(bits));
    }

    /* Finalisation mix from SplitMix64 */
    bits ^= bits >> 30;
    bits *= 0xbf58476d1ce4e5b9ULL;
    bits ^= bits >> 27;
    bits *= 0x94d049bb133111ebULL;
    bits ^= bits >> 31;
    hash_t h = (hash_t)(bits & (uint64_t)PTRDIFF_MAX);
    return h == -1 ? -2 : h;
}

/*
 * menai_name_str_hash — FNV-1a hash of a UTF-8 C string.
 *
 * Used to precompute hashes for global name strings stored in
 * MenaiCodeObject::name_hashes, and to hash entries when building
 * GlobalsTable slots.  Returns a value in [0, PY_SSIZE_T_MAX]; never -1.
 */
static inline hash_t
menai_name_str_hash(const char *s)
{
    uhash_t h = 14695981039346656037ULL;  /* FNV offset basis */
    const unsigned char *p = (const unsigned char *)s;
    while (*p) {
        h ^= (uhash_t)*p++;
        h *= 1099511628211ULL;              /* FNV prime */
    }

    hash_t r = (hash_t)(h & (uhash_t)PTRDIFF_MAX);
    return r == -1 ? -2 : r;
}

hash_t menai_value_hash(MenaiValue *val);
int menai_value_equal(MenaiValue *a, MenaiValue *b);

/* ---------------------------------------------------------------------------
 * MenaiHashTable — open-addressing hash table
 *
 * Maps MenaiValue *keys to ssize_t indices.  Used as the internal
 * acceleration structure for MenaiDict (key -> entry index) and MenaiSet
 * (element -> entry index, for membership testing).
 *
 * Invariants:
 *   - slot_count is always a power of 2 (or 0 for an empty table).
 *   - used <= slot_count * MENAI_HT_MAX_LOAD.
 *   - A slot is empty when its key pointer is NULL.
 *   - Deleted slots are not used (tables are immutable after construction).
 * ------------------------------------------------------------------------- */

#define MENAI_HT_MAX_LOAD_NUM 2   /* load factor numerator   */
#define MENAI_HT_MAX_LOAD_DEN 3   /* load factor denominator */

typedef struct {
    MenaiValue *key;     /* borrowed ref to MenaiValue *; NULL = empty slot */
    hash_t hash;         /* cached hash of key */
    ssize_t index;       /* index into the owning dict/set's element arrays */
} MenaiHashSlot;

typedef struct {
    MenaiHashSlot *slots;
    ssize_t slot_count;  /* power of 2; 0 means uninitialised */
    ssize_t used;
} MenaiHashTable;

int menai_ht_init(MenaiHashTable *ht, ssize_t n);
void menai_ht_free(MenaiHashTable *ht);
ssize_t menai_ht_lookup(const MenaiHashTable *ht, MenaiValue *key, hash_t hash);
void menai_ht_insert(MenaiHashTable *ht, MenaiValue *key, hash_t hash, ssize_t index);
int menai_ht_build(MenaiHashTable *ht, MenaiValue **keys, const hash_t *hashes, ssize_t n);

void *menai_alloc(size_t size);
void menai_free(void *ptr);

typedef struct MenaiCodeObject_s {
    size_t ob_refcnt;

    uint64_t *instrs;                    /* packed instruction words */
    int code_len;                        /* number of instructions */

    MenaiValue **constants;              /* fast constant pool */
    ssize_t nconst;

    const char **names;                  /* global name strings for OP_LOAD_NAME */
    hash_t *name_hashes;                 /* precomputed FNV-1a hash of each name */
    ssize_t nnames;

    struct MenaiCodeObject_s **children; /* child code objects, one per closure */
    ssize_t nchildren;

    int param_count;
    int local_count;
    int outgoing_arg_slots;
    int is_variadic;
    ssize_t ncap;                        /* number of free variables (capture slots) */

    char **param_names;                  /* parameter name strings, parallel to param_count */
    ssize_t nparam_names;                /* number of elements in param_names */

    char *name;                          /* function name for error messages, or NULL */
} MenaiCodeObject;

/*
 * menai_code_object_retain — increment the reference count.
 */
static inline void
menai_code_object_retain(MenaiCodeObject *co)
{
    co->ob_refcnt++;
}

/*
 * menai_code_object_release — decrement the reference count and free if zero.
 */
void menai_code_object_release(MenaiCodeObject *co);

/*
 * menai_code_object_from_python — build a MenaiCodeObject tree from a Python
 * CodeObject.  All constants are converted to fast MenaiValues.  Returns a
 * new reference (ob_refcnt == 1), or NULL on error with a Python exception set.
 */
MenaiCodeObject *menai_code_object_from_python(PyObject *py_code);

/*
 * menai_code_object_max_locals — return the maximum (local_count +
 * outgoing_arg_slots) across the entire subtree rooted at co.
 */
int menai_code_object_max_locals(const MenaiCodeObject *co);

/*
 * menai_reg_set_own — store an owned reference into a register slot.
 *
 * val is an already-owned reference (e.g. freshly allocated, or returned from
 * a constructor).  The old slot value is released.  The slot must not be NULL.
 */
static inline void
menai_reg_set_own(MenaiValue **regs, int slot, MenaiValue *val)
{
    MenaiValue *old = regs[slot];
    regs[slot] = val;
    menai_release(old);
}

/*
 * menai_reg_set_borrow — store a borrowed reference into a register slot.
 *
 * val is a borrowed reference (e.g. read from another register, a constant
 * table, or a container element).  A retain is taken on val, then the old
 * slot value is released.  The slot must not be NULL.
 */
static inline void
menai_reg_set_borrow(MenaiValue **regs, int slot, MenaiValue *val)
{
    MenaiValue *old = regs[slot];
    menai_retain(val);
    regs[slot] = val;
    menai_release(old);
}

/*
 * menai_reg_init — write an owned reference into a slot that is known to hold
 * Menai_NONE (i.e. freshly allocated or reset to the default).
 *
 * Used when populating a callee's register window with arguments or captures
 * before a call.  The old slot value (Menai_NONE) is released.
 */
static inline void
menai_reg_init(MenaiValue **regs, int slot, MenaiValue *val)
{
    MenaiValue *old = regs[slot];
    regs[slot] = val;
    menai_release(old);
}

MenaiValue **menai_regs_alloc(size_t n, MenaiValue *none_val);
void menai_regs_free(MenaiValue **regs, size_t n);

/* Sign-magnitude arbitrary-precision integer. */
typedef struct {
    uint32_t *digits;  /* little-endian base-2^32 magnitude; NULL when zero */
    ssize_t length;    /* number of valid digits; 0 when zero */
    int sign;          /* -1, 0, or 1 */
} MenaiBigInt;

/* Initialise a MenaiBigInt to zero. Must be called before first use as output. */
#define menai_bigint_init(x) (memset((x), 0, sizeof(MenaiBigInt)))

void menai_bigint_free(MenaiBigInt *a);
int menai_bigint_copy(const MenaiBigInt *src, MenaiBigInt *dst);
int menai_bigint_from_long(long v, MenaiBigInt *a);
int menai_bigint_from_pylong(PyObject *obj, MenaiBigInt *a);
int menai_bigint_from_string(const char *s, int base, MenaiBigInt *a);
int menai_bigint_from_codepoints(const uint32_t *data, ssize_t len, int base, MenaiBigInt *a);
int menai_bigint_from_double(double v, MenaiBigInt *a);
int menai_bigint_fits_long(const MenaiBigInt *a);
int menai_bigint_to_long(const MenaiBigInt *a, long *out);
int menai_bigint_to_double(const MenaiBigInt *a, double *out);
PyObject *menai_bigint_to_pylong(const MenaiBigInt *a);
int menai_bigint_to_string(const MenaiBigInt *a, int base, char **out);
Py_hash_t menai_bigint_hash(const MenaiBigInt *a);
int menai_bigint_add(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_sub(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_mul(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_floordiv(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_mod(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_divmod(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *quotient, MenaiBigInt *remainder);
int menai_bigint_neg(const MenaiBigInt *a, MenaiBigInt *result);
int menai_bigint_abs(const MenaiBigInt *a, MenaiBigInt *result);
int menai_bigint_pow(const MenaiBigInt *a, const MenaiBigInt *exp, MenaiBigInt *result);
int menai_bigint_and(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_or(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_xor(const MenaiBigInt *a, const MenaiBigInt *b, MenaiBigInt *result);
int menai_bigint_not(const MenaiBigInt *a, MenaiBigInt *result);
int menai_bigint_shift_left(const MenaiBigInt *a, ssize_t shift, MenaiBigInt *result);
int menai_bigint_shift_right(const MenaiBigInt *a, ssize_t shift, MenaiBigInt *result);
int menai_bigint_eq(const MenaiBigInt *a, const MenaiBigInt *b);
int menai_bigint_ne(const MenaiBigInt *a, const MenaiBigInt *b);
int menai_bigint_lt(const MenaiBigInt *a, const MenaiBigInt *b);
int menai_bigint_gt(const MenaiBigInt *a, const MenaiBigInt *b);
int menai_bigint_le(const MenaiBigInt *a, const MenaiBigInt *b);
int menai_bigint_ge(const MenaiBigInt *a, const MenaiBigInt *b);

typedef struct {
    MenaiValue_HEAD
} MenaiNone;

MenaiValue *menai_none_singleton(void);
void menai_vm_none_init(void);

static inline void
menai_none_dealloc(MenaiValue *self)
{
    /*
     * The singleton is never freed — its refcount should never reach zero.
     */
    (void)self;
}

typedef struct {
    MenaiValue_HEAD
    int value;          /* 0 or 1 */
} MenaiBoolean;

MenaiValue *menai_boolean_true(void);
MenaiValue *menai_boolean_false(void);
void menai_vm_boolean_init(void);

static inline void
menai_boolean_dealloc(MenaiValue *self)
{
    /*
     * Singletons are never freed.
     */
    (void)self;
}

typedef struct {
    MenaiValue_HEAD
    double value;
} MenaiFloat;

MenaiValue *menai_float_alloc(double value);

static inline void
menai_float_dealloc(MenaiValue *self)
{
    menai_free(self);
}

typedef struct {
    MenaiValue_HEAD
    double real;
    double imag;
} MenaiComplex;

MenaiValue *menai_complex_alloc(double real, double imag);

static inline void
menai_complex_dealloc(MenaiValue *self)
{
    menai_free(self);
}

typedef struct {
    MenaiValue_HEAD
    ssize_t ncap;                  /* number of captured values */
    MenaiCodeObject *bytecode;     /* retained — owns all frame metadata */

    /* Inline capture array — ncap elements follow immediately. */
    MenaiValue *captures[1];       /* flexible array member (C99 [1] for MSVC compat) */
} MenaiFunction;

MenaiValue *menai_function_alloc(MenaiCodeObject *co, MenaiValue *none_val);

static inline void
menai_function_dealloc(MenaiValue *self)
{
    MenaiFunction *f = (MenaiFunction *)self;
    menai_code_object_release(f->bytecode);
    ssize_t ncap = f->ncap;
    for (ssize_t i = 0; i < ncap; i++) {
        menai_xrelease(f->captures[i]);
    }

    menai_free(self);
}

typedef struct {
    MenaiValue_HEAD
    ssize_t length;             /* codepoint count */
    hash_t hash;                /* cached hash; -1 = not yet computed */
    uint32_t data[];            /* UTF-32 codepoints, flexible array */
} MenaiString;

static inline ssize_t
menai_string_length(MenaiValue *s)
{
    return ((MenaiString *)s)->length;
}

static inline const uint32_t *
menai_string_data(MenaiValue *s)
{
    return ((MenaiString *)s)->data;
}

static inline uint32_t
menai_string_get(MenaiValue *s, ssize_t i)
{
    return ((MenaiString *)s)->data[i];
}

MenaiValue *menai_string_from_utf8(const char *utf8, ssize_t nbytes);
MenaiValue *menai_string_from_codepoints(const uint32_t *cp, ssize_t len);
MenaiValue *menai_string_from_codepoint(uint32_t cp);
MenaiValue *menai_string_from_pyunicode(PyObject *pystr);
PyObject *menai_string_to_pyunicode(MenaiValue *s);
int menai_string_compare(MenaiValue *a, MenaiValue *b);
int menai_string_equal(MenaiValue *a, MenaiValue *b);
hash_t menai_string_hash(MenaiValue *s);
MenaiValue *menai_string_concat(MenaiValue *a, MenaiValue *b);
MenaiValue *menai_string_ref(MenaiValue *s, ssize_t i);
MenaiValue *menai_string_slice(MenaiValue *s, ssize_t start, ssize_t end);
MenaiValue *menai_string_upcase(MenaiValue *s);
MenaiValue *menai_string_downcase(MenaiValue *s);
MenaiValue *menai_string_trim(MenaiValue *s);
MenaiValue *menai_string_trim_left(MenaiValue *s);
MenaiValue *menai_string_trim_right(MenaiValue *s);
ssize_t menai_string_find(MenaiValue *haystack, MenaiValue *needle);
int menai_string_has_prefix(MenaiValue *s, MenaiValue *prefix);
int menai_string_has_suffix(MenaiValue *s, MenaiValue *suffix);
MenaiValue *menai_string_replace(MenaiValue *s, MenaiValue *from, MenaiValue *to);

static inline void
menai_string_dealloc(MenaiValue *self)
{
    menai_free(self);
}

typedef struct {
    MenaiValue_HEAD
    MenaiValue *name;    /* owned MenaiString * */
} MenaiSymbol;

MenaiValue *menai_symbol_alloc(MenaiValue *name);

static inline void
menai_symbol_dealloc(MenaiValue *self)
{
    menai_xrelease(((MenaiSymbol *)self)->name);
    menai_free(self);
}

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

MenaiValue *menai_struct_alloc(MenaiValue *struct_type, MenaiValue **field_values, ssize_t nfields);
MenaiValue *menai_struct_type_new_from_args(PyObject *args);

static inline void
menai_struct_type_dealloc(MenaiValue *self)
{
    MenaiStructType *s = (MenaiStructType *)self;
    menai_ht_free(&s->field_ht);
    menai_xrelease(s->name);
    int n = s->nfields;
    for (int i = 0; i < n; i++) {
        menai_xrelease(s->fields[i].name);
    }

    menai_free(self);
}

static inline void
menai_struct_dealloc(MenaiValue *self)
{
    MenaiStruct *s = (MenaiStruct *)self;
    menai_xrelease(s->struct_type);
    int n = s->nfields;
    for (int i = 0; i < n; i++) {
        menai_xrelease(s->items[i]);
    }

    menai_free(self);
}

/*
 * Three-tier integer representation:
 *
 *   is_big == 0: value is stored inline as a C long in the small field.
 *                For values in [MENAI_INT_CACHE_MIN, MENAI_INT_CACHE_MAX]
 *                the object is a pre-allocated singleton and must never
 *                be freed.
 *
 *   is_big == 1: value is stored as a MenaiBigInt bignum in the big field.
 *                The MenaiBigInt owns its digit array.
 *
 * The ob_type is always &MenaiInteger_Type.
 */
typedef struct {
    MenaiValue_HEAD
    int is_big;
    long small;     /* valid when is_big == 0 */
    MenaiBigInt big;   /* valid when is_big == 1 */
} MenaiInteger;

/*
 * Small integer cache — covers [MENAI_INT_CACHE_MIN, MENAI_INT_CACHE_MAX].
 * menai_integer_from_long() returns a retained reference, hitting the
 * cache for in-range values.
 */
#define MENAI_INT_CACHE_MIN (-5)
#define MENAI_INT_CACHE_MAX 256
#define MENAI_INT_CACHE_SIZE (MENAI_INT_CACHE_MAX - MENAI_INT_CACHE_MIN + 1)

MenaiValue *menai_integer_from_long(long n);
MenaiValue *menai_integer_from_bigint(MenaiBigInt src);

/*
 * menai_integer_bigint — return a pointer to the MenaiBigInt for a big integer.
 * The caller must ensure is_big == 1 before calling.
 */
static inline const MenaiBigInt *
menai_integer_bigint(MenaiValue *o)
{
    return &((MenaiInteger *)o)->big;
}

/*
 * menai_integer_small — return the small value for a non-big integer.
 * The caller must ensure is_big == 0 before calling.
 */
static inline long
menai_integer_small(MenaiValue *o)
{
    return ((MenaiInteger *)o)->small;
}

int menai_vm_integer_init(void);

static inline void
menai_integer_dealloc(MenaiValue *self)
{
    MenaiInteger *obj = (MenaiInteger *)self;
    if (!obj->is_big) {
        long v = obj->small;
        if (v >= MENAI_INT_CACHE_MIN && v <= MENAI_INT_CACHE_MAX) {
            /*
             * Cached singleton — must never be freed.  Restore refcount so
             * the object remains live.
             */
            obj->ob_refcnt = 1;
            return;
        }
    } else {
        menai_bigint_free(&obj->big);
    }

    menai_free(self);
}

typedef struct {
    MenaiValue_HEAD
    MenaiValue **keys;       /* C array of owned MenaiValue *s */
    MenaiValue **values;     /* C array of owned MenaiValue *s */
    hash_t *hashes;          /* C array of menai_value_hash(keys[i]) */
    MenaiHashTable ht;       /* pure-C hash table for O(1) key lookup */
    ssize_t length;
} MenaiDict;

MenaiValue *menai_dict_new_empty(void);
MenaiValue *menai_dict_from_arrays_steal(MenaiValue **keys, MenaiValue **values, hash_t *hashes, ssize_t n);
MenaiValue *menai_dict_new_empty(void);

/*
 * _dict_free_arrays — release n owned references in keys and values, then
 * free all three arrays.  NULL pointers are safely ignored.
 */
static inline void
_dict_free_arrays(MenaiValue **keys, MenaiValue **values, hash_t *hashes, ssize_t n)
{
    if (keys) {
        for (ssize_t i = 0; i < n; i++) {
            menai_release(keys[i]);
        }

        free(keys);
    }

    if (values) {
        for (ssize_t i = 0; i < n; i++) {
            menai_release(values[i]);
        }

        free(values);
    }

    free(hashes);
}

static inline void
menai_dict_dealloc(MenaiValue *self)
{
    MenaiDict *d = (MenaiDict *)self;
    _dict_free_arrays(d->keys, d->values, d->hashes, d->length);
    menai_ht_free(&d->ht);
    menai_free(self);
}

typedef struct {
    MenaiValue_HEAD
    MenaiValue **elements; /* points to inline_elements for owners, into owner for views */
    ssize_t length;        /* number of live elements */
    /*
     * owner is non-NULL when this list is a slice view into another list's
     * inline_elements array.  In that case elements points into owner's storage
     * and must not be freed; only menai_release(owner) is needed on dealloc.
     * owner always points to a list with owner == NULL (never a chain).
     */
    MenaiValue *owner;
    MenaiValue *inline_elements[]; /* FAM — storage for owning lists */
} MenaiList;

MenaiValue *menai_list_alloc(ssize_t n);
MenaiValue *menai_list_new_empty(void);
MenaiValue *menai_list_rest(MenaiValue *lst);
MenaiValue *menai_list_slice(MenaiValue *lst, ssize_t start, ssize_t end);

static inline MenaiValue *
menai_list_get(MenaiList *list, ssize_t i)
{
    return list->elements[i];
}

static inline MenaiValue **
menai_list_elements(MenaiValue *list_obj)
{
    return ((MenaiList *)list_obj)->elements;
}

static inline ssize_t
menai_list_length(MenaiValue *list_obj)
{
    return ((MenaiList *)list_obj)->length;
}

static inline void
menai_list_dealloc(MenaiValue *self)
{
    MenaiList *lst = (MenaiList *)self;
    if (lst->owner != NULL) {
        /* View — release the backing list; do not touch the element array. */
        menai_release(lst->owner);
        menai_free(lst);
        return;
    }

    /* Owner — release all elements then free the combined block. */
    ssize_t n = lst->length;
    MenaiValue **arr = lst->elements;
    for (ssize_t i = 0; i < n; i++) {
        menai_release(*arr++);
    }

    menai_free(lst);
}

typedef struct {
    MenaiValue_HEAD
    MenaiValue **elements;   /* points into inline_data[0..length-1] */
    hash_t *hashes;          /* points into inline_data past the elements */
    MenaiHashTable ht;       /* pure-C hash table for O(1) membership; separate allocation */
    ssize_t length;          /* number of live elements */
    MenaiValue *inline_data[]; /* FAM: elements[0..cap-1] then hashes[0..cap-1] */
} MenaiSet;

MenaiValue *menai_set_alloc(ssize_t cap);
MenaiValue *menai_set_new_empty(void);

static inline void
menai_set_dealloc(MenaiValue *self)
{
    MenaiSet *s = (MenaiSet *)self;
    ssize_t n = s->length;
    for (ssize_t i = 0; i < n; i++) {
        menai_release(s->elements[i]);
    }

    menai_ht_free(&s->ht);
    menai_free(self);
}

MenaiValue *menai_convert_value(PyObject *src);
PyObject *menai_value_to_slow_value(MenaiValue *raw);
int menai_vm_bridge_init(void);

MenaiValue *menai_format_float(double v);
MenaiValue *menai_format_complex(double real, double imag);

#endif /* MENAI_VM_C_H */
