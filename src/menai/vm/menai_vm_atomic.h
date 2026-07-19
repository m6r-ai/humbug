/*
 * menai_vm_atomic.h - portable atomics for the cancellation flag.
 *
 * We prefer C11 <stdatomic.h>, but older MSVC versions that compile with
 * /std:c11 do not ship it.  In that case we fall back to the Win32
 * Interlocked API (available on every MSVC).
 */
#if defined(_MSC_VER) && !defined(__clang__)

/*
 * MSVC fallback using compiler intrinsics.
 *
 * We deliberately avoid <windows.h> because it defines a 'small' macro
 * that collides with the MenaiInteger/MenaiBigInt 'small' struct field
 * used throughout this file.  The Interlocked* functions are compiler intrinsics,
 * so we only need their declarations.
 */
long _InterlockedCompareExchange(long volatile *Destination, long Exchange, long Comparand);
#pragma intrinsic(_InterlockedCompareExchange)
long _InterlockedExchange(long volatile *Target, long Value);
#pragma intrinsic(_InterlockedExchange)

typedef volatile long _menai_atomic_int;
static inline int _menai_atomic_load(_menai_atomic_int *p) {
    return (int)_InterlockedCompareExchange(p, 0, 0);
}
static inline void _menai_atomic_store(_menai_atomic_int *p, int val) {
    _InterlockedExchange(p, (long)val);
}

#else

/* C11 stdatomic */
#include <stdatomic.h>
typedef atomic_int _menai_atomic_int;
static inline int _menai_atomic_load(_menai_atomic_int *p) {
    return atomic_load(p);
}
static inline void _menai_atomic_store(_menai_atomic_int *p, int val) {
    atomic_store(p, val);
}

#endif

