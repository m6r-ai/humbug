/*
 * menai_vm_string.h — MenaiString type and string operation API.
 *
 * MenaiString stores text as a UTF-32 codepoint array in a single allocation
 * immediately following the object header (flexible array member).  All string
 * operations work directly on uint32_t codepoint arrays with no dependence on
 * Python string types.
 *
 * The Python C API is used only for:
 *   - Error reporting (MenaiEvalError)
 *   - menai_string_from_pyunicode / menai_string_to_pyunicode (conversion boundary)
 */

#ifndef MENAI_VM_STRING_H
#define MENAI_VM_STRING_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>

#include "menai_vm_value.h"

/*
 * MenaiString
 *
 * length holds the codepoint count.  The codepoint data follows immediately
 * in memory via the flexible array member — one allocation per string.
 */

typedef struct {
    MenaiValue_HEAD
    Py_ssize_t length;          /* codepoint count */
    Py_hash_t hash;             /* cached hash; -1 = not yet computed */
    uint32_t data[];            /* UTF-32 codepoints, flexible array */
} MenaiString;

static inline Py_ssize_t
menai_string_length(MenaiValue *s)
{
    return ((MenaiString *)s)->length;
}

static inline const uint32_t *
menai_string_data(MenaiValue *s)
{
    return ((MenaiString *)s)->data;
}

static inline uint32_t
menai_string_get(MenaiValue *s, Py_ssize_t i)
{
    return ((MenaiString *)s)->data[i];
}

MenaiValue *menai_string_from_utf8(const char *utf8, Py_ssize_t nbytes);
MenaiValue *menai_string_from_codepoints(const uint32_t *cp, Py_ssize_t len);
MenaiValue *menai_string_from_codepoint(uint32_t cp);
MenaiValue *menai_string_from_pyunicode(PyObject *pystr);
PyObject *menai_string_to_pyunicode(MenaiValue *s);
int menai_string_compare(MenaiValue *a, MenaiValue *b);
int menai_string_equal(MenaiValue *a, MenaiValue *b);
Py_hash_t menai_string_hash(MenaiValue *s);
MenaiValue *menai_string_concat(MenaiValue *a, MenaiValue *b);
MenaiValue *menai_string_ref(MenaiValue *s, Py_ssize_t i);
MenaiValue *menai_string_slice(MenaiValue *s, Py_ssize_t start, Py_ssize_t end);
MenaiValue *menai_string_upcase(MenaiValue *s);
MenaiValue *menai_string_downcase(MenaiValue *s);
MenaiValue *menai_string_trim(MenaiValue *s);
MenaiValue *menai_string_trim_left(MenaiValue *s);
MenaiValue *menai_string_trim_right(MenaiValue *s);
Py_ssize_t menai_string_find(MenaiValue *haystack, MenaiValue *needle);
int menai_string_has_prefix(MenaiValue *s, MenaiValue *prefix);
int menai_string_has_suffix(MenaiValue *s, MenaiValue *suffix);
MenaiValue *menai_string_replace(MenaiValue *s, MenaiValue *from, MenaiValue *to);

#endif /* MENAI_VM_STRING_H */
