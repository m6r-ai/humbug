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
#include <string.h>

#include "menai_vm_string.h"
#include "menai_vm_memory.h"
#include "menai_vm_string_tables.h"

/* MenaiEvalError — fetched at init time by menai_vm_string_init(). */
static PyObject *_MenaiEvalError = NULL;

/*
 * Allocate a MenaiString_Object with room for len codepoints.
 * ob_size is set to len; hash is set to -1; data is uninitialised.
 * Returns a new reference, or NULL on MemoryError.
 */
static MenaiString_Object *
_menai_string_alloc(Py_ssize_t len)
{
    /*
     * tp_basicsize covers the header fields; tp_itemsize = sizeof(uint32_t)
     * covers each codepoint slot.  PyObject_NewVar uses these to compute the
     * total allocation size: basicsize + len * itemsize.
     */
    MenaiString_Object *obj = PyObject_NewVar(MenaiString_Object, &MenaiString_Type, len);
    if (obj == NULL) return NULL;

    obj->hash = -1;
    return obj;
}

/*
 * Decode a UTF-8 byte sequence into a freshly allocated uint32_t array.
 * Returns the codepoint count via *out_len.  The caller owns the array and
 * must PyMem_Free it.  Returns NULL on MemoryError or encoding error.
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

    uint32_t *buf = (uint32_t *)PyMem_Malloc(n * sizeof(uint32_t));
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
 * terminator) via *out_nbytes.  The caller owns the buffer and must
 * PyMem_Free it.  Returns NULL on MemoryError.
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

    char *buf = (char *)PyMem_Malloc(nbytes + 1);
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

PyObject *
menai_string_from_utf8(const char *utf8, Py_ssize_t nbytes)
{
    Py_ssize_t len;
    uint32_t *buf = _utf8_decode(utf8, nbytes, &len);
    if (!buf) return NULL;

    MenaiString_Object *obj = _menai_string_alloc(len);
    if (!obj) {
        PyMem_Free(buf);
        return NULL;
    }
    memcpy(obj->data, buf, len * sizeof(uint32_t));
    PyMem_Free(buf);
    return (PyObject *)obj;
}

PyObject *
menai_string_from_codepoints(const uint32_t *cp, Py_ssize_t len)
{
    MenaiString_Object *obj = _menai_string_alloc(len);
    if (!obj) return NULL;
    if (len > 0) memcpy(obj->data, cp, len * sizeof(uint32_t));
    return (PyObject *)obj;
}

PyObject *
menai_string_from_codepoint(uint32_t cp)
{
    MenaiString_Object *obj = _menai_string_alloc(1);
    if (!obj) return NULL;
    obj->data[0] = cp;
    return (PyObject *)obj;
}

PyObject *
menai_string_from_pyunicode(PyObject *pystr)
{
    Py_ssize_t nbytes;
    const char *utf8 = PyUnicode_AsUTF8AndSize(pystr, &nbytes);
    if (!utf8) return NULL;
    return menai_string_from_utf8(utf8, nbytes);
}

PyObject *
menai_string_to_pyunicode(PyObject *s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t nbytes;
    char *utf8 = _utf8_encode(ms->data, Py_SIZE(ms), &nbytes);
    if (!utf8) return NULL;
    PyObject *result = PyUnicode_FromStringAndSize(utf8, nbytes);
    PyMem_Free(utf8);
    return result;
}

int
menai_string_compare(PyObject *a, PyObject *b)
{
    MenaiString_Object *ma = (MenaiString_Object *)a;
    MenaiString_Object *mb = (MenaiString_Object *)b;
    Py_ssize_t la = Py_SIZE(ma), lb = Py_SIZE(mb);
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
menai_string_equal(PyObject *a, PyObject *b)
{
    MenaiString_Object *ma = (MenaiString_Object *)a;
    MenaiString_Object *mb = (MenaiString_Object *)b;
    Py_ssize_t la = Py_SIZE(ma);
    if (la != Py_SIZE(mb)) return 0;
    return memcmp(ma->data, mb->data, (size_t)la * sizeof(uint32_t)) == 0;
}

Py_hash_t
menai_string_hash(PyObject *s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    if (ms->hash != -1) return ms->hash;

    /* FNV-1a over the codepoint bytes. */
    Py_ssize_t len = Py_SIZE(ms);
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

PyObject *
menai_string_concat(PyObject *a, PyObject *b)
{
    Py_ssize_t la = Py_SIZE(a);
    Py_ssize_t lb = Py_SIZE(b);
    MenaiString_Object *obj = _menai_string_alloc(la + lb);
    if (!obj) return NULL;
    if (la > 0) memcpy(obj->data, ((MenaiString_Object *)a)->data, (size_t)la * sizeof(uint32_t));
    if (lb > 0) memcpy(obj->data + la, ((MenaiString_Object *)b)->data, (size_t)lb * sizeof(uint32_t));
    return (PyObject *)obj;
}

PyObject *
menai_string_ref(PyObject *s, Py_ssize_t i)
{
    return menai_string_from_codepoint(((MenaiString_Object *)s)->data[i]);
}

PyObject *
menai_string_slice(PyObject *s, Py_ssize_t start, Py_ssize_t end)
{
    return menai_string_from_codepoints(((MenaiString_Object *)s)->data + start, end - start);
}

PyObject *
menai_string_upcase(PyObject *s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t len = Py_SIZE(ms);

    /* First pass: compute output length (expansions may add codepoints). */
    Py_ssize_t out_len = 0;
    for (Py_ssize_t i = 0; i < len; i++) {
        const MenaiUpcaseExpansion *exp = unicode_upcase_expansion(ms->data[i]);
        if (exp) {
            /* Count non-zero slots in the expansion. */
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
    return (PyObject *)obj;
}

PyObject *
menai_string_downcase(PyObject *s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t len = Py_SIZE(ms);
    MenaiString_Object *obj = _menai_string_alloc(len);
    if (!obj) return NULL;
    for (Py_ssize_t i = 0; i < len; i++) obj->data[i] = unicode_simple_downcase(ms->data[i]);
    return (PyObject *)obj;
}

PyObject *
menai_string_trim_left(PyObject *s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t len = Py_SIZE(ms);
    Py_ssize_t start = 0;
    while (start < len && unicode_is_whitespace(ms->data[start])) start++;
    return menai_string_from_codepoints(ms->data + start, len - start);
}

PyObject *
menai_string_trim_right(PyObject *s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t end = Py_SIZE(ms);
    while (end > 0 && unicode_is_whitespace(ms->data[end - 1])) end--;
    return menai_string_from_codepoints(ms->data, end);
}

PyObject *
menai_string_trim(PyObject *s)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    Py_ssize_t len = Py_SIZE(ms);
    Py_ssize_t start = 0, end = len;
    while (start < end && unicode_is_whitespace(ms->data[start])) start++;
    while (end > start && unicode_is_whitespace(ms->data[end - 1])) end--;
    return menai_string_from_codepoints(ms->data + start, end - start);
}

Py_ssize_t
menai_string_find(PyObject *haystack, PyObject *needle)
{
    MenaiString_Object *mh = (MenaiString_Object *)haystack;
    MenaiString_Object *mn = (MenaiString_Object *)needle;
    Py_ssize_t hlen = Py_SIZE(mh), nlen = Py_SIZE(mn);

    if (nlen == 0) return 0;
    if (nlen > hlen) return -1;

    /* Naive search — adequate for typical Menai string sizes. */
    Py_ssize_t limit = hlen - nlen;
    for (Py_ssize_t i = 0; i <= limit; i++) {
        if (memcmp(mh->data + i, mn->data, (size_t)nlen * sizeof(uint32_t)) == 0) return i;
    }
    return -1;
}

int
menai_string_has_prefix(PyObject *s, PyObject *prefix)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    MenaiString_Object *mp = (MenaiString_Object *)prefix;
    Py_ssize_t plen = Py_SIZE(mp);
    if (plen > Py_SIZE(ms)) return 0;

    return memcmp(ms->data, mp->data, (size_t)plen * sizeof(uint32_t)) == 0;
}

int
menai_string_has_suffix(PyObject *s, PyObject *suffix)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    MenaiString_Object *msu = (MenaiString_Object *)suffix;
    Py_ssize_t slen = Py_SIZE(ms), sulen = Py_SIZE(msu);
    if (sulen > slen) return 0;

    return memcmp(ms->data + (slen - sulen), msu->data, (size_t)sulen * sizeof(uint32_t)) == 0;
}

PyObject *
menai_string_replace(PyObject *s, PyObject *from, PyObject *to)
{
    MenaiString_Object *ms = (MenaiString_Object *)s;
    MenaiString_Object *mfr = (MenaiString_Object *)from;
    MenaiString_Object *mto = (MenaiString_Object *)to;
    Py_ssize_t slen = Py_SIZE(ms);
    Py_ssize_t frlen = Py_SIZE(mfr);
    Py_ssize_t tolen = Py_SIZE(mto);

    if (frlen == 0) {
        /*
         * Empty pattern: insert `to` before every codepoint and after the last.
         * "hello".replace("", "X") -> "XhXeXlXlXoX"  (slen+1 insertions)
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

        return (PyObject *)obj;
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

    /* Copy any remaining tail. */
    while (src < slen) obj->data[dst++] = ms->data[src++];

    return (PyObject *)obj;
}

static void
MenaiString_dealloc(PyObject *self)
{
    /* No separate allocation to free — data is inline.
     * tp_free handles the variable-size object. */
    Py_TYPE(self)->tp_free(self);
}

static PyObject *
MenaiString_richcompare(PyObject *self, PyObject *other, int op)
{
    if (Py_TYPE(other) != &MenaiString_Type) {
        if (op == Py_EQ) Py_RETURN_FALSE;
        if (op == Py_NE) Py_RETURN_TRUE;
        Py_RETURN_NOTIMPLEMENTED;
    }
    int cmp = menai_string_compare(self, other);
    switch (op) {
        case Py_EQ: return PyBool_FromLong(cmp == 0);
        case Py_NE: return PyBool_FromLong(cmp != 0);
        case Py_LT: return PyBool_FromLong(cmp < 0);
        case Py_LE: return PyBool_FromLong(cmp <= 0);
        case Py_GT: return PyBool_FromLong(cmp > 0);
        case Py_GE: return PyBool_FromLong(cmp >= 0);
        default: Py_RETURN_NOTIMPLEMENTED;
    }
}

static Py_hash_t
MenaiString_hash(PyObject *self)
{
    return menai_string_hash(self);
}

static PyObject *
MenaiString_type_name(PyObject *self, PyObject *args)
{
    (void)self; (void)args;
    return PyUnicode_FromString("string");
}

PyObject *
MenaiString_describe(PyObject *self, PyObject *args)
{
    (void)args;
    /* Produce a quoted, escaped representation matching menai_value.py. */
    MenaiString_Object *ms = (MenaiString_Object *)self;
    Py_ssize_t len = Py_SIZE(ms);

    /* Build the escaped content into a temporary buffer.
     * Worst case: every codepoint becomes \uXXXX (6 bytes) plus 2 quotes. */
    Py_ssize_t buf_cap = len * 6 + 3;
    char *buf = (char *)PyMem_Malloc(buf_cap);
    if (!buf) return PyErr_NoMemory();

    char *p = buf;
    *p++ = '"';
    for (Py_ssize_t i = 0; i < len; i++) {
        uint32_t cp = ms->data[i];
        switch (cp) {
        case '"':
            *p++ = '\\';
            *p++ = '"';
            break;

        case '\\':
            *p++ = '\\';
            *p++ = '\\';
            break;

        case '\n':
            *p++ = '\\';
            *p++ = 'n';
            break;

        case '\r':
            *p++ = '\\';
            *p++ = 'r';
            break;

        case '\t':
            *p++ = '\\';
            *p++ = 't';
            break;

        default:
            if (cp < 0x20) {
                p += sprintf(p, "\\u%04X", (unsigned)cp);
            } else {
                /* Encode as UTF-8 inline. */
                if (cp < 0x80) {
                    *p++ = (char)cp;
                } else if (cp < 0x800) {
                    *p++ = (char)(0xC0 | (cp >> 6));
                    *p++ = (char)(0x80 | (cp & 0x3F));
                } else if (cp < 0x10000) {
                    *p++ = (char)(0xE0 | (cp >> 12));
                    *p++ = (char)(0x80 | ((cp >> 6) & 0x3F));
                    *p++ = (char)(0x80 | (cp & 0x3F));
                } else {
                    *p++ = (char)(0xF0 | (cp >> 18));
                    *p++ = (char)(0x80 | ((cp >> 12) & 0x3F));
                    *p++ = (char)(0x80 | ((cp >> 6) & 0x3F));
                    *p++ = (char)(0x80 | (cp & 0x3F));
                }
            }
            break;
        }
    }
    *p++ = '"';
    *p   = '\0';

    PyObject *result = PyUnicode_FromStringAndSize(buf, p - buf);
    PyMem_Free(buf);
    return result;
}

static PyObject *
MenaiString_get_value(PyObject *self, void *closure)
{
    (void)closure;
    return menai_string_to_pyunicode(self);
}

PyObject *
MenaiString_to_python(PyObject *self, PyObject *args)
{
    (void)args;
    return menai_string_to_pyunicode(self);
}

static PyGetSetDef MenaiString_getset[] = {
    {"value", MenaiString_get_value, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static PyMethodDef MenaiString_methods[] = {
    {"type_name", MenaiString_type_name, METH_NOARGS, NULL},
    {"describe", MenaiString_describe, METH_NOARGS, NULL},
    {"to_python", MenaiString_to_python, METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

PyTypeObject MenaiString_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name = "menai.menai_vm_c.MenaiString",
    .tp_basicsize = sizeof(MenaiString_Object),
    .tp_itemsize = sizeof(uint32_t),
    .tp_flags = Py_TPFLAGS_DEFAULT,
    .tp_dealloc = MenaiString_dealloc,
    .tp_methods = MenaiString_methods,
    .tp_getset = MenaiString_getset,
    .tp_richcompare = MenaiString_richcompare,
    .tp_hash = MenaiString_hash,
};

int
menai_vm_string_init(PyObject *eval_error_type)
{
    _MenaiEvalError = eval_error_type;
    Py_INCREF(_MenaiEvalError);

    if (PyType_Ready(&MenaiString_Type) < 0)
        return -1;

    return 0;
}
