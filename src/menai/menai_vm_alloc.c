/*
 * menai_vm_alloc.c — power-of-2 pool allocator for the Menai VM.
 *
 * Sizes 32–4096 bytes are handled by 7 buckets (one per power of 2: 32, 64,
 * 128, 256, 512, 1024, 2048, 4096).  Each bucket is a singly-linked free-list
 * threaded through the first sizeof(void *) bytes of the free block.  A
 * per-bucket depth cap of 256 entries prevents unbounded memory retention.
 *
 * Sizes outside [32, 4096] fall through to malloc/free.
 */

#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

#include "menai_vm_value.h"
#include "menai_vm_alloc.h"

#define MENAI_POOL_MIN_SIZE  32
#define MENAI_POOL_MAX_SIZE  4096
#define MENAI_POOL_NUM_BUCKETS 7
#define MENAI_POOL_MAX_DEPTH 256

/*
 * Bucket i covers allocations whose rounded-up power-of-2 size equals
 * MENAI_POOL_MIN_SIZE << i.
 *
 * Bucket 0: 32 bytes
 * Bucket 1: 64 bytes
 * Bucket 2: 128 bytes
 * Bucket 3: 256 bytes
 * Bucket 4: 512 bytes
 * Bucket 5: 1024 bytes
 * Bucket 6: 2048 bytes
 * Bucket 7: 4096 bytes  (MENAI_POOL_NUM_BUCKETS - 1 = 7, but 7+1=8 buckets)
 */

/*
 * We need 8 buckets to cover 32..4096 (32*2^7 = 4096).
 */
#undef MENAI_POOL_NUM_BUCKETS
#define MENAI_POOL_NUM_BUCKETS 8

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
    /*
     * Find the smallest power of 2 >= size, starting from MENAI_POOL_MIN_SIZE.
     * We compute the bucket index as ceil(log2(size)) - log2(MENAI_POOL_MIN_SIZE).
     */
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
        return malloc(size);
    }

    size_t block_size;
    int bucket = _bucket_for(size, &block_size);

    if (_pool_heads[bucket] != NULL) {
        void *ptr = _pool_heads[bucket];
        _pool_heads[bucket] = *(void **)ptr;
        _pool_depths[bucket]--;
        /* The block must have been poisoned when it was freed. */
        assert(((MenaiValue *)ptr)->ob_type == NULL);
        return ptr;
    }

    return malloc(block_size);
}

void
menai_free(void *ptr, size_t size)
{
    if (ptr == NULL) {
        return;
    }

    if (size < MENAI_POOL_MIN_SIZE || size > MENAI_POOL_MAX_SIZE) {
        free(ptr);
        return;
    }

    size_t block_size;
    int bucket = _bucket_for(size, &block_size);

    if (_pool_depths[bucket] < MENAI_POOL_MAX_DEPTH) {
        /* Poison ob_type so use-after-free is detectable. */
        assert(((MenaiValue *)ptr)->ob_type != NULL);
        ((MenaiValue *)ptr)->ob_type = NULL;
        *(void **)ptr = _pool_heads[bucket];
        _pool_heads[bucket] = ptr;
        _pool_depths[bucket]++;
        return;
    }

    free(ptr);
}
