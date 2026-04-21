/*
 * menai_vm_string.h — MenaiString type and string operation API.
 *
 * MenaiString stores text as a UTF-32 codepoint array in a single allocation
 * immediately following the object header (flexible array member).  All string
 * operations work directly on uint32_t codepoint arrays with no dependence on
 * Python string types.
 *
 * The Python C API is used only for:
 *   - Object allocation / deallocation (tp_alloc, tp_free)
 *   - Error reporting (PyErr_SetString)
 *   - menai_string_from_pyunicode / menai_string_to_pyunicode (conversion boundary)
 */

#ifndef MENAI_VM_STRING_H
#define MENAI_VM_STRING_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdint.h>

/* ---------------------------------------------------------------------------
 * MenaiString_Object
 *
 * Uses PyObject_VAR_HEAD so that ob_size (from PyVarObject) holds the
 * codepoint count.  The codepoint data follows immediately in memory via
 * the flexible array member — one allocation per string.
 * ------------------------------------------------------------------------- */

typedef struct {
    PyObject_VAR_HEAD           /* ob_size = codepoint count (length) */
    Py_hash_t hash;             /* cached hash; -1 = not yet computed */
    uint32_t  data[];           /* UTF-32 codepoints, flexible array */
} MenaiString_Object;

/* The PyTypeObject for MenaiString — defined in menai_vm_string.c,
 * registered with the value module at init time. */
extern PyTypeObject MenaiString_Type;

static inline Py_ssize_t
menai_string_length(PyObject *s)
{
    return Py_SIZE(s);
}

static inline const uint32_t *
menai_string_data(PyObject *s)
{
    return ((MenaiString_Object *)s)->data;
}

static inline uint32_t
menai_string_get(PyObject *s, Py_ssize_t i)
{
    return ((MenaiString_Object *)s)->data[i];
}

/*
 * From a UTF-8 encoded buffer of nbytes bytes.  Returns a new reference,
 * or NULL on error (MenaiEvalError or MemoryError set).
 */
PyObject *menai_string_from_utf8(const char *utf8, Py_ssize_t nbytes);

/*
 * From an existing UTF-32 codepoint array of len codepoints.
 * Returns a new reference, or NULL on MemoryError.
 */
PyObject *menai_string_from_codepoints(const uint32_t *cp, Py_ssize_t len);

/* From a single codepoint.  Returns a new reference, or NULL on MemoryError. */
PyObject *menai_string_from_codepoint(uint32_t cp);

/*
 *From a Python unicode object (PyUnicode).  Used in convert_value.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_string_from_pyunicode(PyObject *pystr);

/*
 * Convert to a Python unicode object.  Used for error messages.
 * Returns a new reference, or NULL on error.
 */
PyObject *menai_string_to_pyunicode(PyObject *s);

PyObject *MenaiString_describe(PyObject *self, PyObject *args);
PyObject *MenaiString_to_python(PyObject *self, PyObject *args);

/* Lexicographic comparison.  Returns <0, 0, or >0. */
int menai_string_compare(PyObject *a, PyObject *b);

/* Returns 1 if equal, 0 if not. */
int menai_string_equal(PyObject *a, PyObject *b);

/* Returns the hash, computing and caching it on first call. */
Py_hash_t menai_string_hash(PyObject *s);

/* Concatenate two strings.  Returns a new reference. */
PyObject *menai_string_concat(PyObject *a, PyObject *b);

/*
 *Extract the codepoint at index i as a single-character string.
 * i must be in [0, length).  Returns a new reference.
 */
PyObject *menai_string_ref(PyObject *s, Py_ssize_t i);

/*
 * Extract the substring [start, end).  start and end must satisfy
 * 0 <= start <= end <= length.  Returns a new reference.
 */
PyObject *menai_string_slice(PyObject *s, Py_ssize_t start, Py_ssize_t end);

/*
 * Return a new string with all codepoints mapped to uppercase.
 * Handles multi-codepoint expansions (e.g. ß → SS).
 * Returns a new reference.
 */
PyObject *menai_string_upcase(PyObject *s);

/*
 * Return a new string with all codepoints mapped to lowercase.
 * Returns a new reference.
 */
PyObject *menai_string_downcase(PyObject *s);

/* Return a new string with leading and trailing whitespace removed. */
PyObject *menai_string_trim(PyObject *s);

/* Return a new string with leading whitespace removed. */
PyObject *menai_string_trim_left(PyObject *s);

/* Return a new string with trailing whitespace removed. */
PyObject *menai_string_trim_right(PyObject *s);

/*
 * Find the first occurrence of needle in haystack.
 * Returns the codepoint index, or -1 if not found, or -2 on error.
 */
Py_ssize_t menai_string_find(PyObject *haystack, PyObject *needle);

/* Returns 1 if s starts with prefix, 0 otherwise. */
int menai_string_has_prefix(PyObject *s, PyObject *prefix);

/* Returns 1 if s ends with suffix, 0 otherwise. */
int menai_string_has_suffix(PyObject *s, PyObject *suffix);

/* Return a new string with all occurrences of from replaced by to.
 * Returns a new reference. */
PyObject *menai_string_replace(PyObject *s, PyObject *from, PyObject *to);

/*
 * Module init — called once from menai_vm_value._menai_vm_value_init().
 * eval_error_type is a borrowed reference to MenaiEvalError.
 * Returns 0 on success, -1 on failure (Python exception set).
 */
int menai_vm_string_init(PyObject *eval_error_type);

#endif /* MENAI_VM_STRING_H */
