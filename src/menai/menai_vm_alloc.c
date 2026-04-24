/*
 * menai_vm_alloc.c — power-of-2 pool allocator for the Menai VM.
 *
 * Sizes 32–4096 bytes are handled by 8 buckets (one per power of 2: 32, 64,
 * 128, 256, 512, 1024, 2048, 4096).  Each bucket is a singly-linked free-list
 * threaded through the first sizeof(void *) bytes of the free block.  A
 * per-bucket depth cap of 256 entries prevents unbounded memory retention.
 *
 * menai_alloc writes the pool block size into ob_alloc in the returned header
 * (0 for out-of-pool allocations).  menai_free reads ob_alloc to route the
 * block back to the correct bucket or to free().
 *
 * Sizes outside [32, 4096] fall through to malloc/free.
 */
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

#include "menai_vm_value.h"

#include "menai_vm_alloc.h"

#define MENAI_POOL_MIN_SIZE 32
#define MENAI_POOL_MAX_SIZE 4096
#define MENAI_POOL_NUM_BUCKETS 8
#define MENAI_POOL_MAX_DEPTH 256

/*
 * Free-list head for each bucket.  Each free block stores the next pointer
 * in its first sizeof(void *) bytes.  All pooled blocks are at least 32 bytes
 * so this is always safe.
 */
static void *_pool_heads[MENAI_POOL_NUM_BUCKETS];
static int _pool_depths[MENAI_POOL_NUM_BUCKETS];

/*
 * _bucket_for — return the bucket index for a given size, and the rounded-up
 * block size that will be allocated from that bucket.
 *
 * size must be in [MENAI_POOL_MIN_SIZE, MENAI_POOL_MAX_SIZE].
 */
static inline int
_bucket_for(size_t size, size_t *block_size_out)
{
    size_t block = MENAI_POOL_MIN_SIZE;
    int bucket = 0;
    while (block < size) {
        block <<= 1;
        bucket++;
    }

    *block_size_out = block;
    return bucket;
}

void *
menai_alloc(size_t size)
{
    if (size < MENAI_POOL_MIN_SIZE || size > MENAI_POOL_MAX_SIZE) {
        void *ptr = malloc(size);
        if (ptr) {
            ((MenaiValue *)ptr)->ob_alloc = 0;
        }

        return ptr;
    }

    size_t block_size;
    int bucket = _bucket_for(size, &block_size);

    void *ptr;
    if (_pool_heads[bucket] != NULL) {
        ptr = _pool_heads[bucket];
        _pool_heads[bucket] = *(void **)ptr;
        _pool_depths[bucket]--;
        assert(((MenaiValue *)ptr)->ob_type == 0);
    } else {
        ptr = malloc(block_size);
        if (!ptr) {
            return NULL;
        }
    }

    ((MenaiValue *)ptr)->ob_alloc = (uint16_t)block_size;
    return ptr;
}

void
menai_free(void *ptr)
{
    if (ptr == NULL) {
        return;
    }

    uint16_t block_size = ((MenaiValue *)ptr)->ob_alloc;

    if (block_size == 0) {
        /* Out-of-pool allocation — return directly to malloc. */
        free(ptr);
        return;
    }

    size_t sz = (size_t)block_size;
    size_t dummy;
    int bucket = _bucket_for(sz, &dummy);

    if (_pool_depths[bucket] < MENAI_POOL_MAX_DEPTH) {
        assert(((MenaiValue *)ptr)->ob_type != 0);
        ((MenaiValue *)ptr)->ob_type = 0;
        *(void **)ptr = _pool_heads[bucket];
        _pool_heads[bucket] = ptr;
        _pool_depths[bucket]++;
        return;
    }

    free(ptr);
}
