/*
 * menai_vm_alloc.h — power-of-2 pool allocator for the Menai VM.
 *
 * Provides menai_alloc(size) and menai_free(ptr) as replacements for
 * malloc/free throughout the VM.  Allocations in the range [32, 4096] bytes
 * are served from per-bucket free-lists (one bucket per power of 2).
 * Allocations outside that range fall through to malloc/free directly.
 *
 * menai_alloc writes the pool block size into the ob_alloc field of the
 * returned MenaiValue header (0 for out-of-pool allocations).  menai_free
 * reads ob_alloc to determine how to return the block, so no size argument
 * is required.
 *
 * The pool is not thread-safe.  The VM holds the GIL throughout execution so
 * no locking is required.
 */
#ifndef MENAI_VM_ALLOC_H
#define MENAI_VM_ALLOC_H

/*
 * menai_alloc — allocate size bytes from the pool.
 *
 * Returns a pointer to memory with ob_alloc initialised and all other fields
 * uninitialised, or NULL on allocation failure.
 */
void *menai_alloc(size_t size);

/*
 * menai_free — return the block at ptr to the pool or to malloc.
 *
 * Reads ptr->ob_alloc to determine the block size.  ptr must have been
 * returned by menai_alloc.
 */
void menai_free(void *ptr);

#endif /* MENAI_VM_ALLOC_H */
