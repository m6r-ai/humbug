/*
 * menai_vm_compiler.h — compiler portability macros for the Menai VM.
 *
 * Provides branch prediction hints that map to the appropriate compiler
 * intrinsic on GCC (Linux), Clang (macOS), and MSVC (Windows).
 *
 * Usage:
 *   if (MENAI_LIKELY(ob_type == MENAITYPE_INTEGER)) { ... }
 *   if (MENAI_UNLIKELY(result == NULL)) { goto error; }
 */
#ifndef MENAI_VM_COMPILER_H
#define MENAI_VM_COMPILER_H

#if defined(__GNUC__) || defined(__clang__)
#define MENAI_LIKELY(x) __builtin_expect(!!(x), 1)
#define MENAI_UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define MENAI_LIKELY(x) (x)
#define MENAI_UNLIKELY(x) (x)
#endif

#endif /* MENAI_VM_COMPILER_H */
