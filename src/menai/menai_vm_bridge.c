/*
 * menai_vm_bridge.c — Python boundary layer for all Menai runtime value types.
 *
 * Also defines the boundary describe/to_python functions forward-declared in
 * menai_vm_hashtable.c.
 *
 * Module name: menai.menai_vm_bridge
 * Exported singletons: Menai_NONE, Menai_BOOLEAN_TRUE, Menai_BOOLEAN_FALSE,
 *                      Menai_LIST_EMPTY, Menai_DICT_EMPTY, Menai_SET_EMPTY
 */
#define _POSIX_C_SOURCE 200809L
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#ifndef _MSC_VER
#include <unistd.h>
#else
#include <windows.h>
#endif

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "menai_vm_c.h"
#include "menai_vm_atomic.h"

static MenaiValue *slow_value_to_menai_value(PyObject *src);

/*
 * Module-level state fetched at init
 */
static PyObject *_VMRuntimeError_type = NULL;

/*
 * Singleton values fetched from menai_vm_bridge at init time.
 */
MenaiValue *Menai_NONE = NULL;
MenaiValue *Menai_TRUE = NULL;
MenaiValue *Menai_FALSE = NULL;
MenaiValue *Menai_EMPTY_LIST = NULL;
MenaiValue *Menai_EMPTY_DICT = NULL;
MenaiValue *Menai_EMPTY_SET = NULL;

/*
 * Cancellation — an atomic flag set by cancel() from any thread.
 *
 * The execute_loop checks this every CANCEL_CHECK_INTERVAL instructions.
 * It is reset to 0 at the start of each execute call so that a stale
 * cancellation from a previous call does not affect the next one.
 */
_menai_atomic_int _cancel_requested = 0;

/*
 * Conversion helpers — Python boundary only.
 * These are the sole Menai <-> Python conversion functions for types that
 * have both a native representation and a Python representation.
 */
static MenaiValue *
menai_string_from_pyunicode(PyObject *pystr)
{
    ssize_t nbytes;
    const char *utf8 = PyUnicode_AsUTF8AndSize(pystr, &nbytes);
    if (!utf8) {
        return NULL;
    }

    return menai_string_from_utf8(utf8, nbytes);
}

static PyObject *
menai_string_to_pyunicode(MenaiValue *s)
{
    ssize_t nbytes;
    char *utf8 = menai_string_to_utf8(s, &nbytes);
    if (!utf8) {
        return NULL;
    }

    PyObject *result = PyUnicode_FromStringAndSize(utf8, nbytes);
    free(utf8);
    return result;
}

static MenaiValue *
menai_bytes_from_pybytes(PyObject *pybytes)
{
    Py_ssize_t n;
    char *buf;
    if (PyBytes_AsStringAndSize(pybytes, &buf, &n) < 0) {
        return NULL;
    }

    return menai_bytes_from_raw((const uint8_t *)buf, (ssize_t)n);
}

static PyObject *
menai_bytes_to_pybytes(MenaiValue *b)
{
    MenaiBytes *mb = (MenaiBytes *)b;
    return PyBytes_FromStringAndSize((const char *)mb->data, (Py_ssize_t)mb->length);
}

static int
menai_bigint_from_pylong(PyObject *obj, MenaiBigInt *a)
{
    if (!PyLong_Check(obj)) {
        PyErr_SetString(PyExc_TypeError, "expected int");
        return -1;
    }

    int overflow = 0;
    long v = PyLong_AsLongAndOverflow(obj, &overflow);
    if (!overflow) {
        if (v == -1 && PyErr_Occurred()) {
            return -1;
        }

        return menai_bigint_from_long(v, a);
    }

    int sign = 0;
#if PY_VERSION_HEX >= 0x030E00A1
    PyLong_GetSign(obj, &sign);
#else
    sign = _PyLong_Sign(obj);
#endif

    int is_neg = (sign < 0);

    size_t nbits = (size_t)_PyLong_NumBits(obj);
    if (nbits == (size_t)-1 && PyErr_Occurred()) {
        return -1;
    }

    int needs_extra = (is_neg || (nbits % 8 == 0));
    size_t nbytes = (nbits + (needs_extra ? 8 : 7)) / 8;
    if (nbytes == 0) {
        nbytes = 1;
    }

    unsigned char *buf = (unsigned char *)malloc(nbytes);
    if (buf == NULL) {
        PyErr_NoMemory();
        return -1;
    }

#if PY_VERSION_HEX >= 0x030D0000
    int bytearray_ret = _PyLong_AsByteArray((PyLongObject *)obj, buf, nbytes, 1, 1, 1);
#else
    int bytearray_ret = _PyLong_AsByteArray((PyLongObject *)obj, buf, nbytes, 1, 1);
#endif
    if (bytearray_ret < 0) {
        free(buf);
        return -1;
    }

    if (is_neg) {
        int carry = 1;
        for (size_t i = 0; i < nbytes; i++) {
            int val = (~buf[i] & 0xFF) + carry;
            buf[i] = (unsigned char)(val & 0xFF);
            carry = val >> 8;
        }
    }

    ssize_t ndigits = (ssize_t)((nbytes + 3) / 4);
    uint32_t *digits = (uint32_t *)malloc((size_t)ndigits * sizeof(uint32_t));
    if (digits == NULL) {
        free(buf);
        PyErr_NoMemory();
        return -1;
    }

    for (ssize_t i = 0; i < ndigits; i++) {
        uint32_t d = 0;
        for (int b = 0; b < 4; b++) {
            size_t byte_idx = (size_t)(i * 4 + b);
            if (byte_idx < nbytes) {
                d |= ((uint32_t)buf[byte_idx]) << (b * 8);
            }
        }

        digits[i] = d;
    }

    free(buf);
    menai_bigint_free(a);
    a->digits = digits;
    a->length = ndigits;
    a->sign = is_neg ? -1 : 1;
    menai_bigint_normalize(a);
    return 0;
}

static PyObject *
menai_bigint_to_pylong(const MenaiBigInt *a)
{
    if (a->length == 0) {
        return PyLong_FromLong(0);
    }

    size_t nbytes = (size_t)a->length * 4;
    unsigned char *buf = (unsigned char *)malloc(nbytes);
    if (buf == NULL) {
        PyErr_NoMemory();
        return NULL;
    }

    for (ssize_t i = 0; i < a->length; i++) {
        uint32_t d = a->digits[i];
        buf[i * 4 + 0] = (unsigned char)(d & 0xFF);
        buf[i * 4 + 1] = (unsigned char)((d >> 8) & 0xFF);
        buf[i * 4 + 2] = (unsigned char)((d >> 16) & 0xFF);
        buf[i * 4 + 3] = (unsigned char)((d >> 24) & 0xFF);
    }

    PyObject *result = _PyLong_FromByteArray(buf, nbytes, 1, 0);
    free(buf);
    if (result == NULL) {
        return NULL;
    }

    if (a->sign == -1) {
        PyObject *neg = PyNumber_Negative(result);
        Py_DECREF(result);
        return neg;
    }

    return result;
}

/*
 * Slow-world type objects — fetched once at module init.
 * Used by slow_value_to_menai_value to identify slow objects by type.
 * Will be removed in Phase 2 when the compiler emits fast types directly.
 */
static PyTypeObject *Slow_NoneType = NULL;
static PyTypeObject *Slow_BooleanType = NULL;
static PyTypeObject *Slow_IntegerType = NULL;
static PyTypeObject *Slow_FloatType = NULL;
static PyTypeObject *Slow_ComplexType = NULL;
static PyTypeObject *Slow_StringType = NULL;
static PyTypeObject *Slow_SymbolType = NULL;
static PyTypeObject *Slow_ListType = NULL;
static PyTypeObject *Slow_DictType = NULL;
static PyTypeObject *Slow_SetType = NULL;
static PyTypeObject *Slow_FunctionType = NULL;
static PyTypeObject *Slow_StructTypeType = NULL;
static PyTypeObject *Slow_StructType = NULL;
static PyTypeObject *Slow_BytesType = NULL;

/*
 * _read_int — read a named integer attribute from a Python object.
 */
static int
_read_int(PyObject *obj, const char *attr, int *out)
{
    PyObject *v = PyObject_GetAttrString(obj, attr);
    if (!v) {
        return -1;
    }

    long val = PyLong_AsLong(v);
    Py_DECREF(v);
    if (val == -1 && PyErr_Occurred()) {
        return -1;
    }

    *out = (int)val;
    return 0;
}

/*
 * _read_bool — read a named boolean attribute from a Python object.
 */
static int
_read_bool(PyObject *obj, const char *attr, int *out)
{
    PyObject *v = PyObject_GetAttrString(obj, attr);
    if (!v) {
        return -1;
    }

    int r = PyObject_IsTrue(v);
    Py_DECREF(v);
    if (r < 0) {
        return -1;
    }

    *out = r;
    return 0;
}

/*
 * menai_code_object_from_python — build a MenaiCodeObject tree from a Python
 * CodeObject.  All constants are converted to fast MenaiValues.  Returns a
 * new reference (ob_refcnt == 1), or NULL on error with a Python exception set.
 */
static MenaiCodeObject *
menai_code_object_from_python(PyObject *py_code)
{
    MenaiCodeObject *co = (MenaiCodeObject *)calloc(1, sizeof(MenaiCodeObject));
    if (!co) {
        PyErr_NoMemory();
        return NULL;
    }

    co->ob_refcnt = 1;

    /* Scalar fields */
    if (_read_int(py_code, "param_count", &co->param_count) < 0) {
        goto fail;
    }

    if (_read_int(py_code, "local_count", &co->local_count) < 0) {
        goto fail;
    }

    if (_read_int(py_code, "outgoing_arg_slots", &co->outgoing_arg_slots) < 0) {
        goto fail;
    }

    if (_read_bool(py_code, "is_variadic", &co->is_variadic) < 0) {
        goto fail;
    }

    /* name — optional, used only for error messages */
    {
        PyObject *py_name = PyObject_GetAttrString(py_code, "name");
        if (py_name) {
            if (py_name != Py_None) {
                const char *s = PyUnicode_AsUTF8(py_name);
                if (s) {
                    co->name = strdup(s);
                }
            }

            Py_DECREF(py_name);
        } else {
            PyErr_Clear();
        }
    }

    /* ncap — length of free_vars list */
    {
        PyObject *fv = PyObject_GetAttrString(py_code, "free_vars");
        if (!fv) {
            goto fail;
        }

        co->ncap = PyList_GET_SIZE(fv);
        Py_DECREF(fv);
    }

    /* param_names — strdup each parameter name string */
    {
        PyObject *py_pnames = PyObject_GetAttrString(py_code, "param_names");
        if (!py_pnames) {
            goto fail;
        }

        co->nparam_names = PyList_GET_SIZE(py_pnames);
        if (co->nparam_names > 0) {
            co->param_names = (char **)calloc((size_t)co->nparam_names, sizeof(char *));
            if (!co->param_names) {
                Py_DECREF(py_pnames);
                PyErr_NoMemory();
                goto fail;
            }

            for (ssize_t i = 0; i < co->nparam_names; i++) {
                const char *s = PyUnicode_AsUTF8(PyList_GET_ITEM(py_pnames, i));
                if (!s) {
                    Py_DECREF(py_pnames);
                    goto fail;
                }

                co->param_names[i] = strdup(s);
                if (!co->param_names[i]) {
                    Py_DECREF(py_pnames);
                    PyErr_NoMemory();
                    goto fail;
                }
            }
        }

        Py_DECREF(py_pnames);
    }

    /* instructions — copy the packed array.array buffer */
    {
        PyObject *instrs_obj = PyObject_GetAttrString(py_code, "instructions");
        if (!instrs_obj) {
            goto fail;
        }

        Py_buffer view;
        if (PyObject_GetBuffer(instrs_obj, &view, PyBUF_SIMPLE) < 0) {
            Py_DECREF(instrs_obj);
            goto fail;
        }

        co->code_len = (int)(view.len / sizeof(uint64_t));
        if (co->code_len > 0) {
            co->instrs = (uint64_t *)malloc(view.len);
            if (!co->instrs) {
                PyBuffer_Release(&view);
                Py_DECREF(instrs_obj);
                PyErr_NoMemory();
                goto fail;
            }

            memcpy(co->instrs, view.buf, view.len);
        }

        PyBuffer_Release(&view);
        Py_DECREF(instrs_obj);
    }

    /* names — strdup each global name string */
    {
        PyObject *py_names = PyObject_GetAttrString(py_code, "names");
        if (!py_names) {
            goto fail;
        }

        co->nnames = PyList_GET_SIZE(py_names);
        if (co->nnames > 0) {
            co->names = (const char **)calloc((size_t)co->nnames, sizeof(char *));
            if (!co->names) {
                Py_DECREF(py_names);
                PyErr_NoMemory();
                goto fail;
            }

            for (ssize_t i = 0; i < co->nnames; i++) {
                const char *s = PyUnicode_AsUTF8(PyList_GET_ITEM(py_names, i));
                if (!s) {
                    Py_DECREF(py_names);
                    goto fail;
                }

                co->names[i] = strdup(s);
                if (!co->names[i]) {
                    Py_DECREF(py_names);
                    PyErr_NoMemory();
                    goto fail;
                }
            }
        }

        Py_DECREF(py_names);
    }

    /* name_hashes — precompute FNV-1a hash of each global name string */
    if (co->nnames > 0) {
        co->name_hashes = (hash_t *)malloc((size_t)co->nnames * sizeof(hash_t));
        if (!co->name_hashes) {
            PyErr_NoMemory();
            goto fail;
        }

        for (ssize_t i = 0; i < co->nnames; i++) {
            co->name_hashes[i] = menai_name_str_hash(co->names[i]);
        }
    }

    /*
     * children — recurse first so that when we convert constants that are
     * functions, their children already exist and can be referenced.
     */
    {
        PyObject *py_children = PyObject_GetAttrString(py_code, "code_objects");
        if (!py_children) {
            goto fail;
        }

        co->nchildren = PyList_GET_SIZE(py_children);
        if (co->nchildren > 0) {
            co->children = (MenaiCodeObject **)calloc(
                (size_t)co->nchildren, sizeof(MenaiCodeObject *));
            if (!co->children) {
                Py_DECREF(py_children);
                PyErr_NoMemory();
                goto fail;
            }

            for (ssize_t i = 0; i < co->nchildren; i++) {
                co->children[i] = menai_code_object_from_python(
                    PyList_GET_ITEM(py_children, i));
                if (!co->children[i]) {
                    Py_DECREF(py_children);
                    goto fail;
                }
            }
        }

        Py_DECREF(py_children);
    }

    /*
     * constants — convert each slow Python value to a fast MenaiValue *.
     */
    {
        PyObject *py_constants = PyObject_GetAttrString(py_code, "constants");
        if (!py_constants) {
            goto fail;
        }

        co->nconst = PyList_GET_SIZE(py_constants);
        if (co->nconst > 0) {
            co->constants = (MenaiValue **)calloc(
                (size_t)co->nconst, sizeof(MenaiValue *));
            if (!co->constants) {
                Py_DECREF(py_constants);
                PyErr_NoMemory();
                goto fail;
            }

            for (ssize_t i = 0; i < co->nconst; i++) {
                PyObject *orig = PyList_GET_ITEM(py_constants, i);
                MenaiValue *fast = slow_value_to_menai_value(orig);
                if (!fast) {
                    Py_DECREF(py_constants);
                    goto fail;
                }

                co->constants[i] = fast;
            }
        }

        Py_DECREF(py_constants);
    }

    return co;

fail:
    menai_code_object_release(co);
    return NULL;
}

/*
 * slow_value_to_menai_value — convert one slow menai_value.py object to a fast type.
 *
 * Returns a new reference.  src must be a slow menai_value.py object; passing
 * a fast C value is a programming error and will abort.  For MenaiFunction,
 * captured_values are NOT recursively converted here — call_setup in the VM
 * does that lazily at call time to avoid cycles in letrec closures.
 */
static MenaiValue *
slow_value_to_menai_value(PyObject *src)
{
    PyTypeObject *t = Py_TYPE(src);

    if (t == Slow_NoneType) {
        MenaiValue *s = menai_none_singleton();
        menai_retain(s);
        return s;
    }

    if (t == Slow_BooleanType) {
        PyObject *bv = PyObject_GetAttrString(src, "value");
        if (!bv) {
            return NULL;
        }

        int b = PyObject_IsTrue(bv);
        Py_DECREF(bv);
        if (b < 0) {
            return NULL;
        }

        MenaiValue *r = b ? menai_boolean_true() : menai_boolean_false();
        menai_retain(r);
        return r;
    }

    if (t == Slow_IntegerType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) {
            return NULL;
        }

        if (!PyLong_Check(v)) {
            Py_DECREF(v);
            PyErr_SetString(PyExc_TypeError, "MenaiInteger requires an int");
            return NULL;
        }

        int overflow = 0;
        long lv = PyLong_AsLongAndOverflow(v, &overflow);
        if (!overflow) {
            if (lv == -1 && PyErr_Occurred()) {
                Py_DECREF(v);
                return NULL;
            }

            Py_DECREF(v);
            return menai_integer_from_long(lv);
        }

        /* Bignum — convert via MenaiBigInt */
        MenaiBigInt big;
        menai_bigint_init(&big);
        if (menai_bigint_from_pylong(v, &big) < 0) {
            Py_DECREF(v);
            return NULL;
        }

        Py_DECREF(v);
        return menai_integer_from_bigint(big);
    }

    if (t == Slow_FloatType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) {
            return NULL;
        }

        double d = PyFloat_AsDouble(v);
        Py_DECREF(v);
        if (d == -1.0 && PyErr_Occurred()) {
            return NULL;
        }

        return menai_float_alloc(d);
    }

    if (t == Slow_ComplexType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) {
            return NULL;
        }

        double real = PyComplex_RealAsDouble(v);
        double imag = PyComplex_ImagAsDouble(v);
        Py_DECREF(v);
        return menai_complex_alloc(real, imag);
    }

    if (t == Slow_StringType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) {
            return NULL;
        }

        MenaiValue *r = menai_string_from_pyunicode(v);
        Py_DECREF(v);
        return r;
    }

    if (t == Slow_BytesType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) {
            return NULL;
        }

        MenaiValue *r = menai_bytes_from_pybytes(v);
        Py_DECREF(v);
        return r;
    }

    if (t == Slow_SymbolType) {
        PyObject *n = PyObject_GetAttrString(src, "name");
        if (!n) {
            return NULL;
        }

        MenaiValue *name_str = menai_string_from_pyunicode(n);
        Py_DECREF(n);
        if (!name_str) {
            return NULL;
        }

        MenaiValue *r = menai_symbol_alloc(name_str);
        menai_release(name_str);
        return r;
    }

    if (t == Slow_ListType) {
        PyObject *elems = PyObject_GetAttrString(src, "elements");
        if (!elems) {
            return NULL;
        }

        Py_ssize_t n = PyTuple_GET_SIZE(elems);
        MenaiValue *lst = menai_list_alloc(n);
        if (!lst) {
            Py_DECREF(elems);
            PyErr_NoMemory();
            return NULL;
        }

        MenaiValue **arr = menai_list_elements(lst);
        for (Py_ssize_t i = 0; i < n; i++) {
            arr[i] = slow_value_to_menai_value(PyTuple_GET_ITEM(elems, i));
            if (!arr[i]) {
                for (Py_ssize_t j = 0; j < i; j++) {
                    menai_release(arr[j]);
                }

                menai_release(lst);
                Py_DECREF(elems);
                return NULL;
            }
        }

        Py_DECREF(elems);
        return lst;
    }

    if (t == Slow_DictType) {
        PyObject *pairs = PyObject_GetAttrString(src, "pairs");
        if (!pairs) {
            return NULL;
        }

        Py_ssize_t n = PyTuple_GET_SIZE(pairs);
        MenaiValue **keys = n > 0 ? (MenaiValue **)malloc(n * sizeof(MenaiValue *)) : NULL;
        MenaiValue **values = n > 0 ? (MenaiValue **)malloc(n * sizeof(MenaiValue *)) : NULL;
        hash_t *hashes = n > 0 ? (hash_t *)malloc(n * sizeof(hash_t)) : NULL;
        if (n > 0 && (!keys || !values || !hashes)) {
            free(keys);
            free(values);
            free(hashes);
            Py_DECREF(pairs);
            PyErr_NoMemory();
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *pair = PyTuple_GET_ITEM(pairs, i);
            MenaiValue *fk = slow_value_to_menai_value(PyTuple_GET_ITEM(pair, 0));
            if (!fk) {
                for (Py_ssize_t j = 0; j < i; j++) {
                    menai_release(keys[j]);
                    menai_release(values[j]);
                }

                free(keys);
                free(values);
                free(hashes);
                Py_DECREF(pairs);
                return NULL;
            }

            MenaiValue *fv = slow_value_to_menai_value(PyTuple_GET_ITEM(pair, 1));
            if (!fv) {
                menai_release(fk);
                for (Py_ssize_t j = 0; j < i; j++) {
                    menai_release(keys[j]);
                    menai_release(values[j]);
                }

                free(keys);
                free(values);
                free(hashes);
                Py_DECREF(pairs);
                return NULL;
            }

            hash_t h = menai_value_hash(fk);
            if (h == -1) {
                menai_release(fk);
                menai_release(fv);
                for (Py_ssize_t j = 0; j < i; j++) {
                    menai_release(keys[j]);
                    menai_release(values[j]);
                }

                free(keys);
                free(values);
                free(hashes);
                Py_DECREF(pairs);
                return NULL;
            }

            keys[i] = fk;
            values[i] = fv;
            hashes[i] = h;
        }

        Py_DECREF(pairs);
        return menai_dict_from_arrays_steal(keys, values, hashes, n);
    }

    if (t == Slow_SetType) {
        PyObject *elems = PyObject_GetAttrString(src, "elements");
        if (!elems) {
            return NULL;
        }

        Py_ssize_t n = PyTuple_GET_SIZE(elems);
        MenaiValue *s = menai_set_alloc(n);
        if (!s) {
            Py_DECREF(elems);
            PyErr_NoMemory();
            return NULL;
        }

        MenaiValue **elements = ((MenaiSet *)s)->elements;
        hash_t *hashes = ((MenaiSet *)s)->hashes;
        for (Py_ssize_t i = 0; i < n; i++) {
            MenaiValue *fe = slow_value_to_menai_value(PyTuple_GET_ITEM(elems, i));
            if (!fe) {
                for (Py_ssize_t j = 0; j < i; j++) {
                    menai_release(elements[j]);
                }

                menai_release(s);
                Py_DECREF(elems);
                return NULL;
            }

            hash_t h = menai_value_hash(fe);
            if (h == -1) {
                menai_release(fe);
                for (Py_ssize_t j = 0; j < i; j++) {
                    menai_release(elements[j]);
                }

                menai_release(s);
                Py_DECREF(elems);
                return NULL;
            }

            elements[i] = fe;
            hashes[i] = h;
        }

        Py_DECREF(elems);
        ((MenaiSet *)s)->length = n;
        if (menai_ht_build(&((MenaiSet *)s)->ht, elements, hashes, n) < 0) {
            menai_release(s);
            return NULL;
        }

        return s;
    }

    if (t == Slow_StructTypeType) {
        PyObject *name = PyObject_GetAttrString(src, "name");
        PyObject *tag = PyObject_GetAttrString(src, "tag");
        PyObject *fn = PyObject_GetAttrString(src, "field_names");
        if (!name || !tag || !fn) {
            Py_XDECREF(name);
            Py_XDECREF(tag);
            Py_XDECREF(fn);
            return NULL;
        }

        MenaiValue *name_str = menai_string_from_pyunicode(name);
        Py_DECREF(name);
        if (!name_str) {
            Py_DECREF(tag);
            Py_DECREF(fn);
            return NULL;
        }

        int tag_val = (int)PyLong_AsLong(tag);
        Py_DECREF(tag);
        if (PyErr_Occurred()) {
            menai_release(name_str);
            Py_DECREF(fn);
            return NULL;
        }

        PyObject *fn_tup = PySequence_Tuple(fn);
        Py_DECREF(fn);
        if (!fn_tup) {
            menai_release(name_str);
            return NULL;
        }

        ssize_t nfields = PyTuple_GET_SIZE(fn_tup);
        MenaiValue **field_names_arr = NULL;
        if (nfields > 0) {
            field_names_arr = (MenaiValue **)calloc((size_t)nfields, sizeof(MenaiValue *));
            if (!field_names_arr) {
                menai_release(name_str);
                Py_DECREF(fn_tup);
                return NULL;
            }

            for (ssize_t i = 0; i < nfields; i++) {
                PyObject *fname = PyTuple_GET_ITEM(fn_tup, i);
                MenaiValue *fname_str = menai_string_from_pyunicode(fname);
                if (!fname_str) {
                    for (ssize_t j = 0; j < i; j++) {
                        menai_release(field_names_arr[j]);
                    }
                    free(field_names_arr);
                    menai_release(name_str);
                    Py_DECREF(fn_tup);
                    return NULL;
                }
                field_names_arr[i] = fname_str;
            }
        }

        MenaiValue *result = menai_struct_type_new(name_str, tag_val, field_names_arr, nfields);
        menai_release(name_str);
        for (ssize_t i = 0; i < nfields; i++) {
            menai_release(field_names_arr[i]);
        }
        free(field_names_arr);
        Py_DECREF(fn_tup);
        return result;
    }

    if (t == Slow_StructType) {
        PyObject *st = PyObject_GetAttrString(src, "struct_type");
        PyObject *fields = PyObject_GetAttrString(src, "fields");
        if (!st || !fields) {
            Py_XDECREF(st);
            Py_XDECREF(fields);
            return NULL;
        }

        MenaiValue *fast_st = slow_value_to_menai_value(st);
        Py_DECREF(st);
        if (!fast_st) {
            Py_DECREF(fields);
            return NULL;
        }

        Py_ssize_t n = PyTuple_GET_SIZE(fields);
        MenaiValue **fast_arr = n > 0
            ? (MenaiValue **)malloc(n * sizeof(MenaiValue *)) : NULL;
        if (n > 0 && !fast_arr) {
            menai_release(fast_st);
            Py_DECREF(fields);
            PyErr_NoMemory();
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            MenaiValue *ff = slow_value_to_menai_value(PyTuple_GET_ITEM(fields, i));
            if (!ff) {
                for (Py_ssize_t j = 0; j < i; j++) {
                    menai_release(fast_arr[j]);
                }

                free(fast_arr);
                menai_release(fast_st);
                Py_DECREF(fields);
                return NULL;
            }

            fast_arr[i] = ff;
        }

        Py_DECREF(fields);
        /*
         * menai_struct_alloc retains fast_st and each element of fast_arr
         * internally, so we release our references afterward.
         */
        MenaiValue *r = menai_struct_alloc(fast_st, fast_arr, n);
        for (Py_ssize_t i = 0; i < n; i++) {
            menai_release(fast_arr[i]);
        }

        free(fast_arr);
        menai_release(fast_st);
        return r;
    }

    if (t == Slow_FunctionType) {
        PyObject *cap = PyObject_GetAttrString(src, "captured_values");
        PyObject *bc = PyObject_GetAttrString(src, "bytecode");
        if (!cap || !bc) {
            Py_XDECREF(cap);
            Py_XDECREF(bc);
            return NULL;
        }

        MenaiCodeObject *co = menai_code_object_from_python(bc);
        Py_DECREF(bc);
        if (!co) {
            Py_DECREF(cap);
            return NULL;
        }

        MenaiValue *r = menai_function_alloc(co, menai_none_singleton());
        menai_code_object_release(co);
        if (!r) {
            Py_DECREF(cap);
            return NULL;
        }

        MenaiFunction *f = (MenaiFunction *)r;
        for (Py_ssize_t ci = 0; ci < f->bytecode->ncap; ci++) {
            MenaiValue *fast_cv = slow_value_to_menai_value(PyList_GET_ITEM(cap, ci));
            if (!fast_cv) {
                menai_release(r);
                Py_DECREF(cap);
                return NULL;
            }

            menai_release(f->captures[ci]);  /* release the None placeholder */
            f->captures[ci] = fast_cv;       /* owns the ref from slow_value_to_menai_value */
        }

        Py_DECREF(cap);
        return r;
    }

    PyErr_Format(PyExc_TypeError, "slow_value_to_menai_value: unexpected type %R", (PyObject *)t);
    return NULL;
}

PyObject *
menai_value_to_python_integer(MenaiValue *val)
{
    MenaiInteger *obj = (MenaiInteger *)val;
    if (!obj->is_big) {
        return PyLong_FromLong(obj->small);
    }

    return menai_bigint_to_pylong(&obj->big);
}

static int
fetch_slow_type(PyObject *mod, const char *name, PyTypeObject **dst)
{
    PyObject *obj = PyObject_GetAttrString(mod, name);
    if (!obj) {
        return -1;
    }

    *dst = (PyTypeObject *)obj;
    return 0;
}

/*
 * menai_value_to_slow_value — convert a fast MenaiValue * to its equivalent
 * slow menai_value.py Python object.
 *
 * This is the inverse of slow_value_to_menai_value.  It is used at the C VM execute
 * boundary to ensure all values returned to Python callers are proper Python
 * objects with the full MenaiValue interface (to_python, describe, etc.).
 *
 * For MenaiFunction, bytecode is set to None because these functions are
 * returned as values only, never executed on the Python side.
 * captured_values are recursively converted.
 *
 * Returns a new reference, or NULL on error with a Python exception set.
 */
static PyObject *
menai_value_to_slow_value(MenaiValue *val)
{
    MenaiType t = val->ob_type;

    if (t == MENAITYPE_NONE) {
        return PyObject_CallNoArgs((PyObject *)Slow_NoneType);
    }

    if (t == MENAITYPE_BOOLEAN) {
        int b = ((MenaiBoolean *)val)->value;
        return PyObject_CallOneArg((PyObject *)Slow_BooleanType, b ? Py_True : Py_False);
    }

    if (t == MENAITYPE_INTEGER) {
        PyObject *py_int = menai_value_to_python_integer(val);
        if (!py_int) {
            return NULL;
        }

        PyObject *result = PyObject_CallOneArg((PyObject *)Slow_IntegerType, py_int);
        Py_DECREF(py_int);
        return result;
    }

    if (t == MENAITYPE_FLOAT) {
        PyObject *py_float = PyFloat_FromDouble(((MenaiFloat *)val)->value);
        if (!py_float) {
            return NULL;
        }

        PyObject *result = PyObject_CallOneArg((PyObject *)Slow_FloatType, py_float);
        Py_DECREF(py_float);
        return result;
    }

    if (t == MENAITYPE_COMPLEX) {
        MenaiComplex *c = (MenaiComplex *)val;
        PyObject *py_complex = PyComplex_FromDoubles(c->real, c->imag);
        if (!py_complex) {
            return NULL;
        }

        PyObject *result = PyObject_CallOneArg((PyObject *)Slow_ComplexType, py_complex);
        Py_DECREF(py_complex);
        return result;
    }

    if (t == MENAITYPE_STRING) {
        PyObject *py_str = menai_string_to_pyunicode(val);
        if (!py_str) {
            return NULL;
        }

        PyObject *result = PyObject_CallOneArg((PyObject *)Slow_StringType, py_str);
        Py_DECREF(py_str);
        return result;
    }

    if (t == MENAITYPE_BYTES) {
        PyObject *py_bytes = menai_bytes_to_pybytes(val);
        if (!py_bytes) {
            return NULL;
        }

        PyObject *result = PyObject_CallOneArg((PyObject *)Slow_BytesType, py_bytes);
        Py_DECREF(py_bytes);
        return result;
    }

    if (t == MENAITYPE_SYMBOL) {
        PyObject *py_str = menai_string_to_pyunicode(((MenaiSymbol *)val)->name);
        if (!py_str) {
            return NULL;
        }

        PyObject *result = PyObject_CallOneArg((PyObject *)Slow_SymbolType, py_str);
        Py_DECREF(py_str);
        return result;
    }

    if (t == MENAITYPE_LIST) {
        MenaiList *lst = (MenaiList *)val;
        Py_ssize_t n = lst->length;
        PyObject *py_tuple = PyTuple_New(n);
        if (!py_tuple) {
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *elem = menai_value_to_slow_value(lst->elements[i]);
            if (!elem) {
                Py_DECREF(py_tuple);
                return NULL;
            }

            PyTuple_SET_ITEM(py_tuple, i, elem);
        }

        PyObject *result = PyObject_CallOneArg((PyObject *)Slow_ListType, py_tuple);
        Py_DECREF(py_tuple);
        return result;
    }

    if (t == MENAITYPE_DICT) {
        MenaiDict *d = (MenaiDict *)val;
        Py_ssize_t n = d->length;
        PyObject *py_pairs = PyTuple_New(n);
        if (!py_pairs) {
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *slow_key = menai_value_to_slow_value(d->keys[i]);
            if (!slow_key) {
                Py_DECREF(py_pairs);
                return NULL;
            }

            PyObject *slow_val = menai_value_to_slow_value(d->values[i]);
            if (!slow_val) {
                Py_DECREF(slow_key);
                Py_DECREF(py_pairs);
                return NULL;
            }

            PyObject *pair = PyTuple_Pack(2, slow_key, slow_val);
            Py_DECREF(slow_key);
            Py_DECREF(slow_val);
            if (!pair) {
                Py_DECREF(py_pairs);
                return NULL;
            }

            PyTuple_SET_ITEM(py_pairs, i, pair);
        }

        PyObject *result = PyObject_CallOneArg((PyObject *)Slow_DictType, py_pairs);
        Py_DECREF(py_pairs);
        return result;
    }

    if (t == MENAITYPE_SET) {
        MenaiSet *s = (MenaiSet *)val;
        Py_ssize_t n = s->length;
        PyObject *py_tuple = PyTuple_New(n);
        if (!py_tuple) {
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *elem = menai_value_to_slow_value(s->elements[i]);
            if (!elem) {
                Py_DECREF(py_tuple);
                return NULL;
            }

            PyTuple_SET_ITEM(py_tuple, i, elem);
        }

        PyObject *result = PyObject_CallOneArg((PyObject *)Slow_SetType, py_tuple);
        Py_DECREF(py_tuple);
        return result;
    }

    if (t == MENAITYPE_STRUCTTYPE) {
        MenaiStructType *st = (MenaiStructType *)val;
        PyObject *py_name = menai_string_to_pyunicode(st->name);
        if (!py_name) {
            return NULL;
        }

        PyObject *py_tag = PyLong_FromLong((long)st->tag);
        if (!py_tag) {
            Py_DECREF(py_name);
            return NULL;
        }

        PyObject *py_fields = PyTuple_New(st->nfields);
        if (!py_fields) {
            Py_DECREF(py_name);
            Py_DECREF(py_tag);
            return NULL;
        }

        for (int i = 0; i < st->nfields; i++) {
            PyObject *fname = menai_string_to_pyunicode(st->fields[i].name);
            if (!fname) {
                Py_DECREF(py_name);
                Py_DECREF(py_tag);
                Py_DECREF(py_fields);
                return NULL;
            }

            PyTuple_SET_ITEM(py_fields, i, fname);
        }

        PyObject *result = PyObject_CallFunctionObjArgs(
            (PyObject *)Slow_StructTypeType, py_name, py_tag, py_fields, NULL);
        Py_DECREF(py_name);
        Py_DECREF(py_tag);
        Py_DECREF(py_fields);
        return result;
    }

    if (t == MENAITYPE_STRUCT) {
        MenaiStruct *s = (MenaiStruct *)val;
        PyObject *slow_st = menai_value_to_slow_value(s->struct_type);
        if (!slow_st) {
            return NULL;
        }

        PyObject *py_fields = PyTuple_New(s->nfields);
        if (!py_fields) {
            Py_DECREF(slow_st);
            return NULL;
        }

        for (int i = 0; i < s->nfields; i++) {
            PyObject *fval = menai_value_to_slow_value(s->items[i]);
            if (!fval) {
                Py_DECREF(slow_st);
                Py_DECREF(py_fields);
                return NULL;
            }

            PyTuple_SET_ITEM(py_fields, i, fval);
        }

        PyObject *result = PyObject_CallFunctionObjArgs(
            (PyObject *)Slow_StructType, slow_st, py_fields, NULL);
        Py_DECREF(slow_st);
        Py_DECREF(py_fields);
        return result;
    }

    if (t == MENAITYPE_FUNCTION) {
        MenaiFunction *fn = (MenaiFunction *)val;
        MenaiCodeObject *co = fn->bytecode;

        PyObject *py_params = PyTuple_New(co->nparam_names);
        if (!py_params) {
            return NULL;
        }

        for (Py_ssize_t i = 0; i < co->nparam_names; i++) {
            PyObject *p = PyUnicode_FromString(co->param_names[i]);
            if (!p) {
                Py_DECREF(py_params);
                return NULL;
            }

            PyTuple_SET_ITEM(py_params, i, p);
        }

        PyObject *py_name = co->name ? PyUnicode_FromString(co->name) : (Py_INCREF(Py_None), Py_None);
        if (!py_name) {
            Py_DECREF(py_params);
            return NULL;
        }

        PyObject *py_caps = PyList_New(0);
        if (!py_caps) {
            Py_DECREF(py_params);
            Py_DECREF(py_name);
            return NULL;
        }

        PyObject *py_variadic = co->is_variadic ? Py_True : Py_False;
        PyObject *result = PyObject_CallFunctionObjArgs(
            (PyObject *)Slow_FunctionType,
            py_params,
            py_name,
            Py_None,   /* bytecode — not needed; functions are values only */
            py_caps,
            py_variadic,
            NULL);
        Py_DECREF(py_params);
        Py_DECREF(py_name);
        Py_DECREF(py_caps);
        return result;
    }

    PyErr_Format(PyExc_TypeError,
        "menai_value_to_slow_value: unknown type tag 0x%08x", (unsigned)t);
    return NULL;
}

/*
 * Cached globals GlobalsTable.
 *
 * The globals dict (prelude functions and constants) is built once at Menai
 * startup and never changes.  We cache the converted GlobalsTable keyed by
 * the identity of the Python dict/CodeObject.  On every subsequent execute()
 * call with the same key we reuse the cached table directly.
 */
static PyObject *_cached_globals_key = NULL;
static GlobalsTable _cached_globals_gt;
static int _cached_globals_gt_valid = 0;

/*
 * The CodeObject type from menai.menai_bytecode — used to identify prelude
 * CodeObjects in bridge_globals_get.  Fetched once during bridge init.
 */
static PyTypeObject *_py_code_object_type = NULL;

/*
 * bridge_translate_error - package a MenaiVMError from the native VM
 * into a _MenaiVMRuntimeError sentinel exception.
 *
 * Must only be called when no Python exception is already set.
 *
 * The C VM sets a granular MENAI_ERR_* code at each error site and
 * fills a MenaiVMError struct with diagnostic context (opcode, ip,
 * call_depth).  This function packages all of that into a
 * _MenaiVMRuntimeError Python exception object.  The Python wrapper
 * (MenaiVM.execute) catches this sentinel and translates it into the
 * final user-facing exception using the error table in
 * menai_vm_errors.py.
 *
 * For MENAI_ERR_USER_ERROR, the user-supplied message is carried in
 * err->user_message (a malloc'd C string).  This function frees it
 * after packaging it into the Python exception.
 */
static void
bridge_translate_error(const MenaiVMError *err)
{
    /*
     * Construct _MenaiVMRuntimeError(code, opcode, ip, call_depth,
     * user_message).
     */
    PyObject *py_user_msg;
    PyObject *args;
    PyObject *exc;

    if (err->user_message) {
        py_user_msg = PyUnicode_FromString(err->user_message);
        free((void *)err->user_message);
        if (!py_user_msg) {
            return;
        }
    } else {
        py_user_msg = Py_None;
        Py_INCREF(py_user_msg);
    }

    args = Py_BuildValue("(iiiiN)",
        err->code, err->opcode, err->ip, err->call_depth,
        py_user_msg);
    if (!args) {
        return;
    }

    exc = PyObject_CallObject(_VMRuntimeError_type, args);
    Py_DECREF(args);
    if (!exc) {
        return;
    }

    PyErr_SetObject((PyObject *)Py_TYPE(exc), exc);
    Py_DECREF(exc);
}

/*
 * bridge_globals_get
 * — return a pointer to the cached GlobalsTable, building
 * it the first time a given globals_key is seen.
 *
 * globals_key is either a Python dict of slow MenaiValue objects, or a Python
 * CodeObject representing the prelude.  When it is a CodeObject the prelude is
 * executed here once and the resulting dict is unpacked into the GlobalsTable;
 * subsequent calls with the same CodeObject identity reuse the cached table.
 * Returns NULL on error with a Python exception set.
 */
static const GlobalsTable *
bridge_globals_get(PyObject *globals_key)
{
    if (globals_key == _cached_globals_key && _cached_globals_gt_valid) {
        return &_cached_globals_gt;
    }

    if (_cached_globals_gt_valid) {
        globals_free(&_cached_globals_gt);
        _cached_globals_gt_valid = 0;
        Py_DECREF(_cached_globals_key);
        _cached_globals_key = NULL;
    }

    if (_py_code_object_type && Py_TYPE(globals_key) == _py_code_object_type) {
        /*
         * globals_key is a prelude CodeObject.  Execute it to obtain a
         * MenaiDict of fast values, then unpack directly into the
         * GlobalsTable without any slow round-trip.
         */
        MenaiCodeObject *prelude_co = menai_code_object_from_python(globals_key);
        if (!prelude_co) {
            return NULL;
        }

        MenaiVMError vm_err;
        MenaiValue *result = menai_vm_execute_native(prelude_co, NULL, NULL, &vm_err);
        menai_code_object_release(prelude_co);
        if (!result) {
            if (!PyErr_Occurred()) {
                bridge_translate_error(&vm_err);
            }

            return NULL;
        }

        if (!IS_MENAI_DICT(result)) {
            menai_release(result);
            PyErr_SetString(PyExc_TypeError, "Prelude must evaluate to a dict");
            return NULL;
        }

        if (globals_build_from_dict(&_cached_globals_gt, result) < 0) {
            menai_release(result);
            return NULL;
        }

        menai_release(result);
    } else {
        /*
         * globals_key is a Python dict of slow MenaiValue objects.
         * Convert each value and build the GlobalsTable from arrays.
         */
        Py_ssize_t n = PyDict_Size(globals_key);
        if (n > 0) {
            const char **names = (const char **)malloc(
                (size_t)n * sizeof(const char *));
            MenaiValue **values = (MenaiValue **)malloc(
                (size_t)n * sizeof(MenaiValue *));
            if (!names || !values) {
                free(names);
                free(values);
                PyErr_NoMemory();
                return NULL;
            }

            Py_ssize_t i = 0;
            PyObject *key, *val;
            Py_ssize_t pos = 0;
            while (PyDict_Next(globals_key, &pos, &key, &val)) {
                names[i] = PyUnicode_AsUTF8(key);
                if (!names[i]) {
                    for (Py_ssize_t j = 0; j < i; j++) {
                        menai_release(values[j]);
                    }
                    free(names);
                    free(values);
                    return NULL;
                }

                values[i] = slow_value_to_menai_value(val);
                if (!values[i]) {
                    for (Py_ssize_t j = 0; j < i; j++) {
                        menai_release(values[j]);
                    }
                    free(names);
                    free(values);
                    return NULL;
                }

                i++;
            }

            int rc = globals_build_from_arrays(&_cached_globals_gt,
                                                names, values, (ssize_t)n);
            for (Py_ssize_t j = 0; j < n; j++) {
                menai_release(values[j]);
            }
            free(names);
            free(values);
            if (rc < 0) {
                return NULL;
            }
        } else {
            _cached_globals_gt.slots = NULL;
            _cached_globals_gt.entries = NULL;
            _cached_globals_gt.slot_count = 0;
            _cached_globals_gt.count = 0;
            _cached_globals_gt.owns_names = 1;
        }
    }

    Py_INCREF(globals_key);
    _cached_globals_key = globals_key;
    _cached_globals_gt_valid = 1;
    return &_cached_globals_gt;
}

/*
 * menai_dict_from_pydict — convert a Python dict of (str, MenaiValue) pairs
 * to a native MenaiDict.  Keys are converted to MenaiString, values via
 * slow_value_to_menai_value.  Returns a new reference, or NULL on error.
 */
static MenaiValue *
menai_dict_from_pydict(PyObject *pydict)
{
    Py_ssize_t n = PyDict_Size(pydict);
    if (n == 0) {
        return menai_dict_new_empty();
    }

    MenaiValue **keys = (MenaiValue **)malloc((size_t)n * sizeof(MenaiValue *));
    MenaiValue **values = (MenaiValue **)malloc((size_t)n * sizeof(MenaiValue *));
    hash_t *hashes = (hash_t *)malloc((size_t)n * sizeof(hash_t));
    if (!keys || !values || !hashes) {
        free(keys);
        free(values);
        free(hashes);
        PyErr_NoMemory();
        return NULL;
    }

    Py_ssize_t i = 0;
    PyObject *key, *val;
    Py_ssize_t pos = 0;
    while (PyDict_Next(pydict, &pos, &key, &val)) {
        keys[i] = menai_string_from_pyunicode(key);
        if (!keys[i]) {
            goto fail;
        }

        values[i] = slow_value_to_menai_value(val);
        if (!values[i]) {
            menai_release(keys[i]);
            goto fail;
        }

        hashes[i] = menai_value_hash(keys[i]);
        if (hashes[i] == -1) {
            menai_release(keys[i]);
            menai_release(values[i]);
            goto fail;
        }

        i++;
    }

    return menai_dict_from_arrays_steal(keys, values, hashes, (ssize_t)n);

fail:
    for (Py_ssize_t j = 0; j < i; j++) {
        menai_release(keys[j]);
        menai_release(values[j]);
    }
    free(keys);
    free(values);
    free(hashes);
    return NULL;
}

/*
 * menai_vm_c_execute — the Python-callable entry point.
 *
 * Parses arguments, converts the code tree, builds the globals table, and
 * calls menai_vm_execute_native to run the VM.  The result is converted back
 * to a slow Python MenaiValue before returning.
 */
static PyObject *
menai_vm_c_execute(PyObject *self, PyObject *args)
{
    PyObject *code;
    PyObject *globals_dict;
    PyObject *extra_bindings = NULL;

    /* Clear any stale cancellation from a previous call. */
    menai_vm_clear_cancel();

    if (!PyArg_ParseTuple(args, "OO|O", &code, &globals_dict, &extra_bindings)) {
        return NULL;
    }

    MenaiCodeObject *native_code = menai_code_object_from_python(code);
    if (!native_code) {
        return NULL;
    }

    const GlobalsTable *globals_gt = NULL;
    if (globals_dict && globals_dict != Py_None) {
        globals_gt = bridge_globals_get(globals_dict);
        if (!globals_gt) {
            menai_code_object_release(native_code);
            return NULL;
        }
    }

    MenaiValue *native_extra = NULL;
    if (extra_bindings && extra_bindings != Py_None) {
        native_extra = menai_dict_from_pydict(extra_bindings);
        if (!native_extra) {
            menai_code_object_release(native_code);
            return NULL;
        }
    }

    MenaiVMError vm_err;
    MenaiValue *result;

    /*
     * Release the GIL for the duration of VM execution.  The execute loop is
     * pure C operating on Menai values — it does not touch any Python objects.
     * This allows other Python threads (e.g. the event loop requesting
     * cancellation) to run without contention.
     */
    Py_BEGIN_ALLOW_THREADS
    result = menai_vm_execute_native(native_code, globals_gt, native_extra, &vm_err);
    Py_END_ALLOW_THREADS

    menai_code_object_release(native_code);
    menai_xrelease(native_extra);

    if (result == NULL) {
        if (!PyErr_Occurred()) {
            bridge_translate_error(&vm_err);
        }

        return NULL;
    }

    PyObject *slow = menai_value_to_slow_value(result);
    menai_release(result);
    return slow;
}

/*
 * bridge_yield_fn — callback for the VM's periodic cancellation check.
 *
 * Checks the atomic cancellation flag.  The GIL is not held during VM
 * execution (see menai_vm_c_execute), so no GIL release is needed here.
 * The flag is set by cancel() from another thread via an atomic store.
 *
 * Returns 0 to continue execution, -1 to signal cancellation.
 */
static int
bridge_yield_fn(void)
{
    if (_menai_atomic_load(&_cancel_requested)) {
        return -1;
    }
    return 0;
}

int
menai_vm_bridge_init(void)
{
    /* Fetch slow-world types — needed by slow_value_to_menai_value. */
    PyObject *slow_mod = PyImport_ImportModule("menai.menai_value");
    if (!slow_mod) {
        return 0;
    }

    if (fetch_slow_type(slow_mod, "MenaiNone", &Slow_NoneType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiBoolean", &Slow_BooleanType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiInteger", &Slow_IntegerType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiFloat", &Slow_FloatType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiComplex", &Slow_ComplexType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiString", &Slow_StringType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiSymbol", &Slow_SymbolType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiList", &Slow_ListType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiDict", &Slow_DictType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiSet", &Slow_SetType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiFunction", &Slow_FunctionType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiStructType", &Slow_StructTypeType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiStruct", &Slow_StructType) < 0) {
        goto fail;
    }

    if (fetch_slow_type(slow_mod, "MenaiBytes", &Slow_BytesType) < 0) {
        goto fail;
    }

    Py_DECREF(slow_mod);
    slow_mod = NULL;

    /* Fetch the CodeObject type — used by bridge_globals_get to identify
     * prelude CodeObjects. */
    PyObject *bytecode_mod = PyImport_ImportModule("menai.menai_bytecode");
    if (!bytecode_mod) {
        return 0;
    }

    PyObject *co_type = PyObject_GetAttrString(bytecode_mod, "CodeObject");
    Py_DECREF(bytecode_mod);
    if (!co_type) {
        return 0;
    }

    _py_code_object_type = (PyTypeObject *)co_type;

    menai_vm_none_init();
    menai_vm_boolean_init();
    if (menai_vm_integer_init() < 0) {
        return 0;
    }
    return 1;

fail:
    Py_XDECREF(slow_mod);
    return 0;
}

/*
 * menai_vm_c_cancel — request cancellation of the currently running execute().
 *
 * Thread-safe: may be called from a different thread than the one in execute().
 * The flag is checked at the next cancellation check point in the execution loop.
 */
static PyObject *
menai_vm_c_cancel(PyObject *self, PyObject *args)
{
    _menai_atomic_store(&_cancel_requested, 1);
    Py_RETURN_NONE;
}

/*
 * Module definition
 */
static PyMethodDef menai_vm_c_methods[] = {
    {
        "execute",
        menai_vm_c_execute,
        METH_VARARGS,
        "Execute a Menai CodeObject and return the result."
    },
    {
        "cancel",
        menai_vm_c_cancel,
        METH_NOARGS,
        "Request cancellation of the currently running execute() call."
    },
    { NULL, NULL, 0, NULL }
};

static struct PyModuleDef menai_vm_c_module = {
    PyModuleDef_HEAD_INIT,
    "menai.menai_vm_c",
    NULL,
    -1,
    menai_vm_c_methods
};

static int
menai_vm_shim_init(void)
{
    if (!menai_vm_bridge_init()) {
        return -1;
    }

    Menai_NONE = menai_none_singleton();
    Menai_TRUE = menai_boolean_true();
    Menai_FALSE = menai_boolean_false();
    Menai_EMPTY_LIST = menai_list_new_empty();
    Menai_EMPTY_DICT = menai_dict_new_empty();
    Menai_EMPTY_SET = menai_set_new_empty();

    PyObject *err_mod = PyImport_ImportModule("menai.menai_vm_errors");
    if (err_mod == NULL) {
        return -1;
    }

    _VMRuntimeError_type = PyObject_GetAttrString(err_mod, "_MenaiVMRuntimeError");
    Py_DECREF(err_mod);
    if (_VMRuntimeError_type == NULL) {
        Py_XDECREF(_VMRuntimeError_type);
        return -1;
    }

    menai_vm_set_yield_fn(bridge_yield_fn);

    return 0;
}

PyMODINIT_FUNC
PyInit_menai_vm_c(void)
{
    PyObject *module = PyModule_Create(&menai_vm_c_module);
    if (module == NULL) {
        return NULL;
    }

    if (menai_vm_shim_init() < 0) {
        Py_DECREF(module);
        return NULL;
    }

    return module;
}
