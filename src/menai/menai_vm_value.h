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
 * MenaiType — the type descriptor for a Menai value type.
 */
typedef uint32_t MenaiType;

#define MENAITYPE_NONE 0x892a8271
#define MENAITYPE_BOOLEAN 0x186fce9a
#define MENAITYPE_FUNCTION 0xf00018ab
#define MENAITYPE_SYMBOL 0xebac98a4
#define MENAITYPE_STRING 0x2397a89b
#define MENAITYPE_INTEGER 0x752879ae
#define MENAITYPE_FLOAT 0x339a87fb
#define MENAITYPE_COMPLEX 0xcc92362b
#define MENAITYPE_LIST 0x12879aa8
#define MENAITYPE_DICT 0xd0c0d087
#define MENAITYPE_SET 0x5e188954
#define MENAITYPE_STRUCT 0x518976dd
#define MENAITYPE_STRUCTTYPE 0x89986acd

/*
 * MenaiValue_HEAD — common prefix for every Menai value struct.
 */
typedef void (*menai_destructor)(MenaiValue *);

#define MenaiValue_HEAD              \
    uint32_t ob_refcnt;              \
    MenaiType ob_type;               \
    menai_destructor ob_destructor;

/*
 * MenaiValue — the minimal struct that every MenaiValue pointer can be
 * safely cast to in order to read ob_refcnt and ob_type.
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
