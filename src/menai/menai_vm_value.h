/*
 * menai_vm_value.h — base object model for the Menai VM.
 *
 * Defines the MenaiValue pointer type, the MenaiValue_HEAD macro, and the
 * MenaiType typedef.
 */
#ifndef MENAI_VM_OBJECT_H
#define MENAI_VM_OBJECT_H

#include <assert.h>

typedef struct MenaiValue_s MenaiValue;

/*
 * MenaiType — the type tag for a Menai value.  uint16_t is sufficient for
 * the 13 current types and leaves room for future additions.  The values are
 * chosen to be distinct and non-zero so that ob_type == 0 reliably detects
 * use-after-free (the allocator poisons freed blocks with ob_type = 0).
 */
typedef uint16_t MenaiType;

#define MENAITYPE_NONE 0x8271
#define MENAITYPE_BOOLEAN 0x9a3f
#define MENAITYPE_FUNCTION 0x18ab
#define MENAITYPE_SYMBOL 0xa4c7
#define MENAITYPE_STRING 0x89b2
#define MENAITYPE_INTEGER 0x79ae
#define MENAITYPE_FLOAT 0x87fb
#define MENAITYPE_COMPLEX 0x362b
#define MENAITYPE_LIST 0x9aa8
#define MENAITYPE_DICT 0xd087
#define MENAITYPE_SET 0x8954
#define MENAITYPE_STRUCT 0x76dd
#define MENAITYPE_STRUCTTYPE 0x6acd

/*
 * MenaiValue_HEAD — common prefix for every Menai value struct.
 *
 * ob_refcnt    — reference count.
 * ob_type      — type tag (MenaiType, uint16_t).
 * ob_alloc     — pool block size in bytes if this object was served from the
 *                pool allocator, or 0 if it was allocated directly via malloc.
 *                Written by menai_alloc; read by menai_free to determine how
 *                to return the block.  Stored as uint16_t; pool sizes fit
 *                within [32, 4096].
 * ob_destructor — called when ob_refcnt reaches zero.
 */
typedef void (*menai_destructor)(MenaiValue *);

#define MenaiValue_HEAD              \
    uint32_t ob_refcnt;              \
    MenaiType ob_type;               \
    uint16_t ob_alloc;               \
    menai_destructor ob_destructor;

/*
 * MenaiValue — the minimal struct that every MenaiValue pointer can be
 * safely cast to in order to read ob_refcnt, ob_type, and ob_alloc.
 */
struct MenaiValue_s {
    MenaiValue_HEAD
};

const char *menai_short_type_name(MenaiType t);

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
        val->ob_destructor(val);
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

#endif /* MENAI_VM_OBJECT_H */
