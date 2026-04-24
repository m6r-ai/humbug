/*
 * menai_vm_alloc.h — power-of-2 pool allocator for the Menai VM.
 *
 * Provides menai_alloc(size) and menai_free(ptr) as replacements for
 * malloc/free throughout the VM.  Allocations in the range [32, 4096] bytes
 * are served from per-bucket free-lists (one bucket per power of 2).
 * Allocations outside that range fall through to malloc/free directly.
 *
 * menai_alloc writes the pool bucket into the ob_alloc_bucket field of the
 * returned MenaiValue header (0 for out-of-pool allocations).  menai_free
 * reads ob_alloc_bucket to determine how to return the block, so no size argument
 * is required.
 *
 * The pool is not thread-safe.  The VM holds the GIL throughout execution so
 * no locking is required.
 */
#ifndef MENAI_VM_ALLOC_H
#define MENAI_VM_ALLOC_H

void *menai_alloc(size_t size);
void menai_free(void *ptr);

#endif /* MENAI_VM_ALLOC_H */
