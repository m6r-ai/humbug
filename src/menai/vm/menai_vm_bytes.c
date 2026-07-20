/*
 * menai_vm_bytes.c — MenaiBytes type implementation.
 *
 * MenaiBytes stores its data inline in the same allocation as the struct,
 * using a C99 flexible array member.  A single menai_alloc call covers both
 * the header and the data array for owning bytes.  Slice views allocate
 * only the header (sizeof(MenaiBytes)) and point their data pointer into
 * the owner's inline storage, exactly mirroring MenaiList's pattern.
 */
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "menai_vm_c.h"

/*
 * _menai_bytes_alloc — allocate an owning MenaiBytes with room for n bytes.
 * length is set to n; hash is set to -1; data is uninitialised.
 * Returns a new reference, or NULL on allocation failure.
 */
static MenaiBytes *
_menai_bytes_alloc(ssize_t n)
{
    size_t sz = sizeof(MenaiBytes) + (size_t)n;
    MenaiBytes *obj = (MenaiBytes *)menai_alloc(sz);
    if (obj == NULL) {
        return NULL;
    }

    obj->ob_refcnt = 1;
    obj->ob_type = MENAITYPE_BYTES;
    obj->length = n;
    obj->hash = -1;
    obj->owner = NULL;
    obj->data = obj->inline_data;

    return obj;
}

MenaiValue *
menai_bytes_alloc(ssize_t n)
{
    MenaiBytes *obj = _menai_bytes_alloc(n);
    if (!obj) {
        return NULL;
    }

    return (MenaiValue *)obj;
}

MenaiValue *
menai_bytes_new_empty(void)
{
    return menai_bytes_alloc(0);
}

MenaiValue *
menai_bytes_from_raw(const uint8_t *src, ssize_t n)
{
    MenaiBytes *obj = _menai_bytes_alloc(n);
    if (!obj) {
        return NULL;
    }

    if (n > 0) {
        memcpy(obj->inline_data, src, (size_t)n);
    }

    return (MenaiValue *)obj;
}

MenaiValue *
menai_bytes_slice(MenaiValue *b_val, ssize_t start, ssize_t end)
{
    MenaiBytes *b = (MenaiBytes *)b_val;

    /*
     * Resolve the owner: if b is itself a view, point at its owner so
     * all views are depth-1 from the root data owner.
     */
    MenaiValue *owner = (b->owner != NULL) ? b->owner : b_val;

    MenaiBytes *view = (MenaiBytes *)menai_alloc(sizeof(MenaiBytes));
    if (view == NULL) {
        return NULL;
    }

    view->ob_refcnt = 1;
    view->ob_type = MENAITYPE_BYTES;
    menai_retain(owner);
    view->owner = owner;
    view->data = b->data + start;
    view->length = end - start;
    view->hash = -1;

    return (MenaiValue *)view;
}

MenaiValue *
menai_bytes_concat(MenaiValue *a, MenaiValue *b)
{
    MenaiBytes *ma = (MenaiBytes *)a;
    MenaiBytes *mb = (MenaiBytes *)b;
    ssize_t la = ma->length;
    ssize_t lb = mb->length;
    MenaiBytes *obj = _menai_bytes_alloc(la + lb);
    if (!obj) {
        return NULL;
    }

    if (la > 0) {
        memcpy(obj->inline_data, ma->data, (size_t)la);
    }

    if (lb > 0) {
        memcpy(obj->inline_data + la, mb->data, (size_t)lb);
    }

    return (MenaiValue *)obj;
}

MenaiValue *
menai_bytes_ref(MenaiValue *b, ssize_t i)
{
    return menai_integer_from_long((long)((MenaiBytes *)b)->data[i]);
}

MenaiValue *
menai_bytes_append_u8(MenaiValue *b, uint8_t value)
{
    MenaiBytes *mb = (MenaiBytes *)b;
    ssize_t len = mb->length;
    MenaiBytes *obj = _menai_bytes_alloc(len + 1);
    if (!obj) {
        return NULL;
    }

    if (len > 0) {
        memcpy(obj->inline_data, mb->data, (size_t)len);
    }

    obj->inline_data[len] = value;

    return (MenaiValue *)obj;
}

/*
 * menai_bytes_append_multi — append N bytes encoded from an unsigned long
 * value in the specified endianness.  width must be 1–8.
 */
MenaiValue *
menai_bytes_append_multi(MenaiValue *b, unsigned long long value, int width, int le)
{
    MenaiBytes *mb = (MenaiBytes *)b;
    ssize_t len = mb->length;
    MenaiBytes *obj = _menai_bytes_alloc(len + width);
    if (!obj) {
        return NULL;
    }

    if (len > 0) {
        memcpy(obj->inline_data, mb->data, (size_t)len);
    }

    uint8_t *dest = obj->inline_data + len;
    if (le) {
        for (int i = 0; i < width; i++) {
            dest[i] = (uint8_t)((value >> (i * 8)) & 0xFF);
        }
    } else {
        for (int i = 0; i < width; i++) {
            dest[i] = (uint8_t)((value >> ((width - 1 - i) * 8)) & 0xFF);
        }
    }

    return (MenaiValue *)obj;
}

/*
 * menai_bytes_write_multi — return a copy of b with N bytes at the given
 * offset replaced by the encoded value.  width must be 1–8.
 */
MenaiValue *
menai_bytes_write_multi(MenaiValue *b, ssize_t offset,
                        unsigned long long value, int width, int le)
{
    MenaiBytes *mb = (MenaiBytes *)b;
    ssize_t len = mb->length;
    MenaiBytes *obj = _menai_bytes_alloc(len);
    if (!obj) {
        return NULL;
    }

    if (len > 0) {
        memcpy(obj->inline_data, mb->data, (size_t)len);
    }

    uint8_t *dest = obj->inline_data + offset;
    if (le) {
        for (int i = 0; i < width; i++) {
            dest[i] = (uint8_t)((value >> (i * 8)) & 0xFF);
        }
    } else {
        for (int i = 0; i < width; i++) {
            dest[i] = (uint8_t)((value >> ((width - 1 - i) * 8)) & 0xFF);
        }
    }

    return (MenaiValue *)obj;
}

int
menai_bytes_equal(MenaiValue *a, MenaiValue *b)
{
    MenaiBytes *ma = (MenaiBytes *)a;
    MenaiBytes *mb = (MenaiBytes *)b;
    ssize_t la = ma->length;
    if (la != mb->length) {
        return 0;
    }

    return memcmp(ma->data, mb->data, (size_t)la) == 0;
}

int
menai_bytes_compare(MenaiValue *a, MenaiValue *b)
{
    MenaiBytes *ma = (MenaiBytes *)a;
    MenaiBytes *mb = (MenaiBytes *)b;
    ssize_t la = ma->length, lb = mb->length;
    ssize_t min_len = la < lb ? la : lb;
    int cmp = memcmp(ma->data, mb->data, (size_t)min_len);
    if (cmp != 0) {
        return cmp < 0 ? -1 : 1;
    }

    if (la < lb) {
        return -1;
    }

    if (la > lb) {
        return 1;
    }

    return 0;
}

hash_t
menai_bytes_hash(MenaiValue *b)
{
    MenaiBytes *mb = (MenaiBytes *)b;
    if (mb->hash != -1) {
        return mb->hash;
    }

    /* FNV-1a over the raw bytes. */
    uint64_t h = 14695981039346656037ULL;
    const unsigned char *p = (const unsigned char *)mb->data;
    ssize_t nbytes = mb->length;
    for (ssize_t i = 0; i < nbytes; i++) {
        h ^= p[i];
        h *= 1099511628211ULL;
    }

    hash_t result = (hash_t)h;
    if (result == -1) {
        result = -2;
    }

    mb->hash = result;

    return result;
}