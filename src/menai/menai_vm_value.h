/*
 * menai_vm_value.h — base object model for the Menai VM.
 *
 * Defines the MenaiValue pointer type, the MenaiValue_HEAD macro, and the
 * MenaiType typedef.
 */
#ifndef MENAI_VM_OBJECT_H
#define MENAI_VM_OBJECT_H

#include <stddef.h>
#include <stdint.h>
#include <assert.h>

#ifdef _MSC_VER
typedef ptrdiff_t ssize_t;
#define SSIZE_MAX PTRDIFF_MAX
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
//    assert(val->ob_type != 0);
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
//    assert(val->ob_type != 0);
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

#endif /* MENAI_VM_OBJECT_H */
