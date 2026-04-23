/*
 * menai_vm_memory.h — register file operations for the Menai VM.
 *
 * Provides the register-level memory operations used by the dispatch loop.
 * The retain/release/xrelease/is_unique primitives live in menai_vm_value.h;
 * this header builds on those to provide the higher-level register file API.
 *
 * Naming convention:
 *   menai_reg_*           — register file operations
 *   menai_regs_alloc/_free — register array lifetime
 */

#ifndef MENAI_VM_MEMORY_H
#define MENAI_VM_MEMORY_H

#include <stddef.h>

#include "menai_vm_value.h"

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

#endif /* MENAI_VM_MEMORY_H */
