/*
 * menai_vm_string.c — MenaiString type implementation.
 *
 * Stores text as a UTF-32 codepoint array in a single allocation immediately
 * following the object header.  All string operations work directly on
 * uint32_t arrays.  The only Python string API used is PyUnicode at the
 * conversion boundary (menai_string_from_pyunicode / menai_string_to_pyunicode).
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "menai_vm_string.h"
#include "menai_vm_string_tables.h"

/* MenaiEvalError — fetched at init time by menai_vm_string_init(). */
static PyObject *_MenaiEvalError = NULL;

/*
 * Allocate a MenaiString_Object with room for len codepoints.
 * length is set to len; hash is set to -1; data is uninitialised.
 * Returns a new reference, or NULL on allocation failure.
 */
static MenaiString_Object *
_menai_string_alloc(Py_ssize_t len)
{
    MenaiString_Object *obj = (MenaiString_Object *)malloc(
        sizeof(MenaiString_Object) + (size_t)len * sizeof(uint32_t));
    if (obj == NULL)
        return NULL;

    obj->ob_refcnt = 1;
    obj->ob_type = &MenaiString_Type;
    obj->length = len;
    obj->hash = -1;

    return obj;
}

static void
MenaiString_dealloc(PyObject *self)
{
    /* Data is inline — one free covers the whole allocation. */
    free(self);
}

PyTypeObject MenaiString_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "menai.MenaiString",          /* tp_name */
    sizeof(MenaiString_Object),   /* tp_basicsize */
    0,                             /* tp_itemsize */
    MenaiString_dealloc,                  /* tp_dealloc */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    Py_TPFLAGS_DEFAULT,
};

/*
 * Decode a UTF-8 byte sequence into a freshly allocated uint32_t array.
 * Returns the codepoint count via *out_len.  The caller owns the array and
 * must free() it.  Returns NULL on allocation failure or encoding error.
 */
static uint32_t *
_utf8_decode(const char *utf8, Py_ssize_t nbytes, Py_ssize_t *out_len)
{
    /* First pass: count codepoints so we can allocate exactly. */
    Py_ssize_t n = 0;
    const unsigned char *p = (const unsigned char *)utf8;
    const unsigned char *end = p + nbytes;
    while (p < end) {
        unsigned char b = *p;
        if (b < 0x80) {
            p++;
        } else if ((b & 0xE0) == 0xC0) {
            if (p + 2 > end || (p[1] & 0xC0) != 0x80) goto bad_utf8;
            p += 2;
        } else if ((b & 0xF0) == 0xE0) {
            if (p + 3 > end || (p[1] & 0xC0) != 0x80 || (p[2] & 0xC0) != 0x80) goto bad_utf8;
            p += 3;
        } else if ((b & 0xF8) == 0xF0) {
            if (p + 4 > end || (p[1] & 0xC0) != 0x80 || (p[2] & 0xC0) != 0x80 || (p[3] & 0xC0) != 0x80) goto bad_utf8;
            p += 4;
        } else {
            goto bad_utf8;
        }
        n++;
    }

    uint32_t *buf = (uint32_t *)malloc((size_t)n * sizeof(uint32_t));
    if (!buf) {
        PyErr_NoMemory();
        return NULL;
    }

    /* Second pass: decode. */
    p = (const unsigned char *)utf8;
    for (Py_ssize_t i = 0; i < n; i++) {
        unsigned char b = *p;
        uint32_t cp;
        if (b < 0x80) {
            cp = b; p++;
        } else if ((b & 0xE0) == 0xC0) {
            cp = ((uint32_t)(b & 0x1F) << 6) | (p[1] & 0x3F);
            p += 2;
        } else if ((b & 0xF0) == 0xE0) {
            cp = ((uint32_t)(b & 0x0F) << 12) | ((uint32_t)(p[1] & 0x3F) << 6) | (p[2] & 0x3F);
            p += 3;
        } else {
            cp = ((uint32_t)(b & 0x07) << 18) | ((uint32_t)(p[1] & 0x3F) << 12) | ((uint32_t)(p[2] & 0x3F) << 6) | (p[3] & 0x3F);
            p += 4;
        }
        buf[i] = cp;
    }

    *out_len = n;
    return buf;

bad_utf8:
    PyErr_SetString(PyExc_ValueError, "invalid UTF-8 sequence");
    return NULL;
}

/*
 * Encode a uint32_t codepoint array into a newly allocated UTF-8 byte
 * buffer (null-terminated).  Returns the byte count (excluding the null
 * terminator) via *out_nbytes.  The caller owns the buffer and must free()
 * it.  Returns NULL on allocation failure.
 */
static char *
_utf8_encode(const uint32_t *data, Py_ssize_t len, Py_ssize_t *out_nbytes)
{
    /* First pass: compute byte count. */
    Py_ssize_t nbytes = 0;
    for (Py_ssize_t i = 0; i < len; i++) {
        uint32_t cp = data[i];
        if (cp < 0x80) nbytes += 1;
        else if (cp < 0x800) nbytes += 2;
        else if (cp < 0x10000) nbytes += 3;
        else nbytes += 4;
    }

    char *buf = (char *)malloc((size_t)(nbytes + 1));
    if (!buf) {
        PyErr_NoMemory();
        return NULL;
    }

    /* Second pass: encode. */
    unsigned char *q = (unsigned char *)buf;
    for (Py_ssize_t i = 0; i < len; i++) {
        uint32_t cp = data[i];
        if (cp < 0x80) {
            *q++ = (unsigned char)cp;
        } else if (cp < 0x800) {
            *q++ = (unsigned char)(0xC0 | (cp >> 6));
            *q++ = (unsigned char)(0x80 | (cp & 0x3F));
        } else if (cp < 0x10000) {
            *q++ = (unsigned char)(0xE0 | (cp >> 12));
            *q++ = (unsigned char)(0x80 | ((cp >> 6) & 0x3F));
            *q++ = (unsigned char)(0x80 | (cp & 0x3F));
        } else {
            *q++ = (unsigned char)(0xF0 | (cp >> 18));
            *q++ = (unsigned char)(0x80 | ((cp >> 12) & 0x3F));
            *q++ = (unsigned char)(0x80 | ((cp >> 6) & 0x3F));
            *q++ = (unsigned char)(0x80 | (cp & 0x3F));
        }
    }
    *q = '\0';

    *out_nbytes = nbytes;
    return buf;
}

MenaiValue
menai_string_from_utf8(const char *utf8, Py_ssize_t nbytes)
{
    Py_ssize_t len;
    uint32_t *buf = _utf8_decode(utf8, nbytes, &len);
    if (!buf) return NULL;

    MenaiString_Object *obj = _menai_string_alloc(len);
    if (!obj) {
        free(buf);
        return NULL;
    }
    memcpy(obj->data, buf, (size_t)len * sizeof(uint32_t));
    free(buf);

    return (MenaiValue)obj;
}

MenaiValue
menai_string_from_codepoints(const uint32_t *cp, Py_ssize_t len)
{
    MenaiString_Object *obj = _menai_string_alloc(len);
    if (!obj) return NULL;
    if (len > 0) memcpy(obj->data, cp, (size_t)len * sizeof(uint32_t));

    return (MenaiValue)obj;
}

MenaiValue
menai_string_from_codepoint(uint32_t cp)
{
    MenaiString_Object *obj = _menai_string_alloc(1);
    if (!obj) return NULL;
    obj->data[0] = cp;

    return (MenaiValue)obj;
}

MenaiValue
menai_string_from_pyunicode(PyObject *pystr)
{
    Py_ssize_t nbytes;
    const char *utf8 = PyUnicode_AsUTF8AndSize(pystr, &nbytes);
    if (!utf8) return NULL;

    return menai_string_from_utf8(utf8, nbytes);
}

PyObject *
menai_string_to_pyunicode(MenaiValue s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t nbytes;
    char *utf8 = _utf8_encode(ms->data, ms->length, &nbytes);
    if (!utf8) return NULL;

    PyObject *result = PyUnicode_FromStringAndSize(utf8, nbytes);
    free(utf8);

    return result;
}

int
menai_string_compare(MenaiValue a, MenaiValue b)
{
    MenaiString_Object *ma = (MenaiString_Object *)a;
    MenaiString_Object *mb = (MenaiString_Object *)b;
    Py_ssize_t la = ma->length, lb = mb->length;
    Py_ssize_t min_len = la < lb ? la : lb;
    for (Py_ssize_t i = 0; i < min_len; i++) {
        if (ma->data[i] < mb->data[i]) return -1;
        if (ma->data[i] > mb->data[i]) return 1;
    }
    if (la < lb) return -1;
    if (la > lb) return 1;
    return 0;
}

int
menai_string_equal(MenaiValue a, MenaiValue b)
{
    MenaiString_Object *ma = (MenaiString_Object *)a;
    MenaiString_Object *mb = (MenaiString_Object *)b;
    Py_ssize_t la = ma->length;
    if (la != mb->length) return 0;
    return memcmp(ma->data, mb->data, (size_t)la * sizeof(uint32_t)) == 0;
}

Py_hash_t
menai_string_hash(MenaiValue s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    if (ms->hash != -1) return ms->hash;

    /* FNV-1a over the codepoint bytes. */
    Py_ssize_t len = ms->length;
    uint64_t h = 14695981039346656037ULL;
    const unsigned char *p = (const unsigned char *)ms->data;
    Py_ssize_t nbytes = len * (Py_ssize_t)sizeof(uint32_t);
    for (Py_ssize_t i = 0; i < nbytes; i++) {
        h ^= p[i];
        h *= 1099511628211ULL;
    }

    Py_hash_t result = (Py_hash_t)h;
    if (result == -1) result = -2;  /* -1 is reserved for "not cached" */
    ms->hash = result;

    return result;
}

MenaiValue
menai_string_concat(MenaiValue a, MenaiValue b)
{
    MenaiString_Object *ma = (MenaiString_Object *)a;
    MenaiString_Object *mb = (MenaiString_Object *)b;
    Py_ssize_t la = ma->length, lb = mb->length;
    MenaiString_Object *obj = _menai_string_alloc(la + lb);
    if (!obj) return NULL;
    if (la > 0) memcpy(obj->data, ma->data, (size_t)la * sizeof(uint32_t));
    if (lb > 0) memcpy(obj->data + la, mb->data, (size_t)lb * sizeof(uint32_t));

    return (MenaiValue)obj;
}

MenaiValue
menai_string_ref(MenaiValue s, Py_ssize_t i)
{
    return menai_string_from_codepoint(((MenaiString_Object *)s)->data[i]);
}

MenaiValue
menai_string_slice(MenaiValue s, Py_ssize_t start, Py_ssize_t end)
{
    return menai_string_from_codepoints(((MenaiString_Object *)s)->data + start, end - start);
}

MenaiValue
menai_string_upcase(MenaiValue s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t len = ms->length;

    /* First pass: compute output length (expansions may add codepoints). */
    Py_ssize_t out_len = 0;
    for (Py_ssize_t i = 0; i < len; i++) {
        const MenaiUpcaseExpansion *exp = unicode_upcase_expansion(ms->data[i]);
        if (exp) {
            for (int j = 0; j < 3 && exp->expansion[j]; j++) out_len++;
        } else {
            out_len++;
        }
    }

    MenaiString_Object *obj = _menai_string_alloc(out_len);
    if (!obj) return NULL;

    /* Second pass: fill. */
    Py_ssize_t k = 0;
    for (Py_ssize_t i = 0; i < len; i++) {
        const MenaiUpcaseExpansion *exp = unicode_upcase_expansion(ms->data[i]);
        if (exp) {
            for (int j = 0; j < 3 && exp->expansion[j]; j++) obj->data[k++] = exp->expansion[j];
        } else {
            obj->data[k++] = unicode_simple_upcase(ms->data[i]);
        }
    }

    return (MenaiValue)obj;
}

MenaiValue
menai_string_downcase(MenaiValue s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t len = ms->length;
    MenaiString_Object *obj = _menai_string_alloc(len);
    if (!obj) return NULL;
    for (Py_ssize_t i = 0; i < len; i++) obj->data[i] = unicode_simple_downcase(ms->data[i]);

    return (MenaiValue)obj;
}

MenaiValue
menai_string_trim_left(MenaiValue s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t len = ms->length;
    Py_ssize_t start = 0;
    while (start < len && unicode_is_whitespace(ms->data[start])) start++;

    return menai_string_from_codepoints(ms->data + start, len - start);
}

MenaiValue
menai_string_trim_right(MenaiValue s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t end = ms->length;
    while (end > 0 && unicode_is_whitespace(ms->data[end - 1])) end--;

    return menai_string_from_codepoints(ms->data, end);
}

MenaiValue
menai_string_trim(MenaiValue s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t len = ms->length;
    Py_ssize_t start = 0, end = len;
    while (start < end && unicode_is_whitespace(ms->data[start])) start++;
    while (end > start && unicode_is_whitespace(ms->data[end - 1])) end--;

    return menai_string_from_codepoints(ms->data + start, end - start);
}

Py_ssize_t
menai_string_find(MenaiValue haystack, MenaiValue needle)
{
    MenaiString_Object *mh = (MenaiString_Object *)haystack;
    MenaiString_Object *mn = (MenaiString_Object *)needle;
    Py_ssize_t hlen = mh->length, nlen = mn->length;

    if (nlen == 0) return 0;
    if (nlen > hlen) return -1;

    Py_ssize_t limit = hlen - nlen;
    for (Py_ssize_t i = 0; i <= limit; i++) {
        if (memcmp(mh->data + i, mn->data, (size_t)nlen * sizeof(uint32_t)) == 0) return i;
    }

    return -1;
}

int
menai_string_has_prefix(MenaiValue s, MenaiValue prefix)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    MenaiString_Object *mp = (MenaiString_Object *)prefix;
    Py_ssize_t plen = mp->length;
    if (plen > ms->length) return 0;

    return memcmp(ms->data, mp->data, (size_t)plen * sizeof(uint32_t)) == 0;
}

int
menai_string_has_suffix(MenaiValue s, MenaiValue suffix)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    MenaiString_Object *msu = (MenaiString_Object *)suffix;
    Py_ssize_t slen = ms->length, sulen = msu->length;
    if (sulen > slen) return 0;

    return memcmp(ms->data + (slen - sulen), msu->data, (size_t)sulen * sizeof(uint32_t)) == 0;
}

MenaiValue
menai_string_replace(MenaiValue s, MenaiValue from, MenaiValue to)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    MenaiString_Object *mfr = (MenaiString_Object *)from;
    MenaiString_Object *mto = (MenaiString_Object *)to;
    Py_ssize_t slen = ms->length;
    Py_ssize_t frlen = mfr->length;
    Py_ssize_t tolen = mto->length;

    if (frlen == 0) {
        /*
         * Empty pattern: insert `to` before every codepoint and after the
         * last.  "hello".replace("", "X") -> "XhXeXlXlXoX"
         */
        Py_ssize_t out_len = slen + (slen + 1) * tolen;
        MenaiString_Object *obj = _menai_string_alloc(out_len);
        if (!obj) return NULL;
        Py_ssize_t dst = 0;
        for (Py_ssize_t i = 0; i <= slen; i++) {
            if (tolen > 0) {
                memcpy(obj->data + dst, mto->data, (size_t)tolen * sizeof(uint32_t));
                dst += tolen;
            }
            if (i < slen) obj->data[dst++] = ms->data[i];
        }

        return (MenaiValue)obj;
    }

    if (slen == 0) {
        menai_retain(s);
        return s;
    }

    /* First pass: count occurrences to compute output length. */
    Py_ssize_t count = 0;
    for (Py_ssize_t i = 0; i <= slen - frlen; ) {
        if (memcmp(ms->data + i, mfr->data, (size_t)frlen * sizeof(uint32_t)) == 0) {
            count++;
            i += frlen;
        } else {
            i++;
        }
    }

    if (count == 0) {
        menai_retain(s);
        return s;
    }

    Py_ssize_t out_len = slen + count * (tolen - frlen);
    MenaiString_Object *obj = _menai_string_alloc(out_len);
    if (!obj) return NULL;

    /* Second pass: fill. */
    Py_ssize_t src = 0, dst = 0;
    while (src <= slen - frlen) {
        if (memcmp(ms->data + src, mfr->data, (size_t)frlen * sizeof(uint32_t)) == 0) {
            if (tolen > 0) memcpy(obj->data + dst, mto->data, (size_t)tolen * sizeof(uint32_t));
            dst += tolen;
            src += frlen;
        } else {
            obj->data[dst++] = ms->data[src++];
        }
    }
    while (src < slen) obj->data[dst++] = ms->data[src++];

    return (MenaiValue)obj;
}

int
menai_vm_string_init(PyObject *eval_error_type)
{
    _MenaiEvalError = eval_error_type;
    Py_INCREF(_MenaiEvalError);

    return 0;
}
