/*
 * menai_vm_alloc.h — power-of-2 pool allocator for the Menai VM.
 *
 * Provides menai_alloc(size) and menai_free(ptr, size) as replacements for
 * malloc/free throughout the VM.  Allocations in the range [32, 4096] bytes
 * are served from per-bucket free-lists (one bucket per power of 2).
 * Allocations outside that range fall through to malloc/free directly.
 *
 * The caller is responsible for passing the same size to menai_free that was
 * passed to menai_alloc.  Each object type knows its own allocation size, so
 * this is always available at the dealloc call site.
 *
 * The pool is not thread-safe.  The VM holds the GIL throughout execution so
 * no locking is required.
 */

#ifndef MENAI_VM_ALLOC_H
#define MENAI_VM_ALLOC_H

#include <stddef.h>

/*
 * menai_alloc — allocate size bytes from the pool.
 *
 * Returns a pointer to uninitialized memory, or NULL on allocation failure.
 * Equivalent to malloc(size) but faster for sizes in [32, 4096].
 */
void *menai_alloc(size_t size);

/*
 * menai_free — return size bytes at ptr to the pool.
 *
 * ptr must have been returned by menai_alloc(size) with the same size value.
 * Equivalent to free(ptr) but faster for sizes in [32, 4096].
 */
void menai_free(void *ptr, size_t size);

#endif /* MENAI_VM_ALLOC_H */
