/*
 * menai_vm_alloc.c — power-of-2 pool allocator for the Menai VM.
 *
 * Sizes 32–4096 bytes are handled by 8 buckets (one per power of 2: 32, 64,
 * 128, 256, 512, 1024, 2048, 4096).  Each bucket is a singly-linked free-list
 * threaded through the first sizeof(void *) bytes of the free block.  A
 * per-bucket depth cap of 256 entries prevents unbounded memory retention.
 *
 * menai_alloc writes the pool block into ob_alloc_bucket in the returned header
 * (0 for out-of-pool allocations).  menai_free reads ob_alloc_bucket to route the
 * block back to the correct bucket or to free().
 */
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>

#include "menai_vm_value.h"

#include "menai_vm_alloc.h"

#define MENAI_POOL_LOG_MIN_SIZE 5
#define MENAI_POOL_MIN_SIZE (1 << MENAI_POOL_LOG_MIN_SIZE)
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
#if defined(__GNUC__) || defined(__clang__)
    return size <= MENAI_POOL_MIN_SIZE ? 0 : 31 - __builtin_clz(size - 1) + 1 - MENAI_POOL_LOG_MIN_SIZE;
#else
    size_t block = MENAI_POOL_MIN_SIZE;
    int bucket = 0;
    while (block < size) {
        block <<= 1;
        bucket++;
    }

    *block_size_out = block;
    return bucket;
#endif
}

void *
menai_alloc(size_t size)
{
    if (size > MENAI_POOL_MAX_SIZE) {
        void *ptr = malloc(size);
        if (ptr) {
            ((MenaiValue *)ptr)->ob_alloc_bucket = -1;
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
        ptr = malloc(1 << (bucket + MENAI_POOL_LOG_MIN_SIZE));
        if (!ptr) {
            return NULL;
        }
    }

    ((MenaiValue *)ptr)->ob_alloc_bucket = (int16_t)bucket;
    return ptr;
}

void
menai_free(void *ptr)
{
    int16_t bucket = ((MenaiValue *)ptr)->ob_alloc_bucket;
    if (bucket == -1) {
        /* Out-of-pool allocation — return directly to malloc. */
        free(ptr);
        return;
    }

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
