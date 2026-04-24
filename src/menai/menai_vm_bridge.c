/*
 * menai_vm_bridge.c — Python boundary layer for all Menai runtime value types.
 *
 * Provides:
 *   menai_convert_value() — slow menai_value.py -> fast C type
 *
 * Also defines the boundary describe/to_python functions forward-declared in
 * menai_vm_hashtable.c.
 *
 * Module name: menai.menai_vm_bridge
 * Exported singletons: Menai_NONE, Menai_BOOLEAN_TRUE, Menai_BOOLEAN_FALSE,
 *                      Menai_LIST_EMPTY, Menai_DICT_EMPTY, Menai_SET_EMPTY
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "menai_vm_alloc.h"
#include "menai_vm_float.h"
#include "menai_vm_dict.h"
#include "menai_vm_function.h"
#include "menai_vm_code.h"
#include "menai_vm_list.h"
#include "menai_vm_set.h"
#include "menai_vm_struct.h"
#include "menai_vm_symbol.h"
#include "menai_vm_complex.h"
#include "menai_vm_integer.h"
#include "menai_vm_boolean.h"
#include "menai_vm_none.h"
#include "menai_vm_string.h"
#include "menai_vm_bridge.h"
#include "menai_vm_hashtable.h"

/*
 * Slow-world type objects — fetched once at module init.
 * Used by menai_convert_value to identify slow objects by type.
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

/* Error type */
PyObject *MenaiEvalError_type = NULL;

/*
 * menai_convert_value — convert one slow menai_value.py object to a fast type.
 *
 * Returns a new reference.  src must be a slow menai_value.py object; passing
 * a fast C value is a programming error and will abort.  For MenaiFunction,
 * captured_values are NOT recursively converted here — call_setup in the VM
 * does that lazily at call time to avoid cycles in letrec closures.
 */
MenaiValue *
menai_convert_value(PyObject *src)
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
        MenaiValue **arr = n > 0 ? (MenaiValue **)menai_alloc((size_t)n * sizeof(MenaiValue *)) : NULL;
        if (n > 0 && !arr) {
            Py_DECREF(elems);
            PyErr_NoMemory();
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            arr[i] = menai_convert_value(PyTuple_GET_ITEM(elems, i));
            if (!arr[i]) {
                for (Py_ssize_t j = 0; j < i; j++) {
                    menai_release(arr[j]);
                }

                menai_free(arr, (size_t)n * sizeof(MenaiValue *));
                Py_DECREF(elems);
                return NULL;
            }
        }

        Py_DECREF(elems);
        return menai_list_from_array_steal(arr, n);
    }

    if (t == Slow_DictType) {
        PyObject *pairs = PyObject_GetAttrString(src, "pairs");
        if (!pairs) {
            return NULL;
        }

        Py_ssize_t n = PyTuple_GET_SIZE(pairs);
        MenaiValue **keys = n > 0 ? (MenaiValue **)malloc(n * sizeof(MenaiValue *)) : NULL;
        MenaiValue **values = n > 0 ? (MenaiValue **)malloc(n * sizeof(MenaiValue *)) : NULL;
        Py_hash_t *hashes = n > 0 ? (Py_hash_t *)malloc(n * sizeof(Py_hash_t)) : NULL;
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
            MenaiValue *fk = menai_convert_value(PyTuple_GET_ITEM(pair, 0));
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

            MenaiValue *fv = menai_convert_value(PyTuple_GET_ITEM(pair, 1));
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

            Py_hash_t h = menai_value_hash(fk);
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
        MenaiValue **elements = n > 0 ? (MenaiValue **)malloc(n * sizeof(MenaiValue *)) : NULL;
        Py_hash_t *hashes = n > 0 ? (Py_hash_t *)malloc(n * sizeof(Py_hash_t)) : NULL;
        if (n > 0 && (!elements || !hashes)) {
            free(elements);
            free(hashes);
            Py_DECREF(elems);
            PyErr_NoMemory();
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            MenaiValue *fe = menai_convert_value(PyTuple_GET_ITEM(elems, i));
            if (!fe) {
                for (Py_ssize_t j = 0; j < i; j++) {
                    menai_release(elements[j]);
                }

                free(elements);
                free(hashes);
                Py_DECREF(elems);
                return NULL;
            }

            Py_hash_t h = menai_value_hash(fe);
            if (h == -1) {
                menai_release(fe);
                for (Py_ssize_t j = 0; j < i; j++) {
                    menai_release(elements[j]);
                }

                free(elements);
                free(hashes);
                Py_DECREF(elems);
                return NULL;
            }

            elements[i] = fe;
            hashes[i] = h;
        }

        Py_DECREF(elems);
        return menai_set_from_arrays_steal(elements, hashes, n);
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

        PyObject *args = PyTuple_Pack(3, name, tag, fn);
        Py_DECREF(name);
        Py_DECREF(tag);
        Py_DECREF(fn);
        if (!args) {
            return NULL;
        }

        MenaiValue *r = menai_struct_type_new_from_args(args);
        Py_DECREF(args);
        return r;
    }

    if (t == Slow_StructType) {
        PyObject *st = PyObject_GetAttrString(src, "struct_type");
        PyObject *fields = PyObject_GetAttrString(src, "fields");
        if (!st || !fields) {
            Py_XDECREF(st);
            Py_XDECREF(fields);
            return NULL;
        }

        MenaiValue *fast_st = menai_convert_value(st);
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
            MenaiValue *ff = menai_convert_value(PyTuple_GET_ITEM(fields, i));
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
            MenaiValue *fast_cv = menai_convert_value(PyList_GET_ITEM(cap, ci));
            if (!fast_cv) {
                menai_release(r);
                Py_DECREF(cap);
                return NULL;
            }

            menai_release(f->captures[ci]);  /* release the None placeholder */
            f->captures[ci] = fast_cv;       /* owns the ref from menai_convert_value */
        }

        Py_DECREF(cap);
        return r;
    }

    PyErr_Format(PyExc_TypeError, "menai_convert_value: unexpected type %R", (PyObject *)t);
    return NULL;
}

PyObject *
menai_value_describe_none(MenaiValue *val)
{
    (void)val;
    return PyUnicode_FromString("#none");
}

PyObject *
menai_value_describe_boolean(MenaiValue *val)
{
    int v = ((MenaiBoolean *)val)->value;
    return PyUnicode_FromString(v ? "#t" : "#f");
}

PyObject *
menai_value_describe_integer(MenaiValue *val)
{
    MenaiInteger *obj = (MenaiInteger *)val;
    if (!obj->is_big) {
        return PyUnicode_FromFormat("%ld", obj->small);
    }

    char *s = NULL;
    if (menai_bigint_to_string(&obj->big, 10, &s) < 0) {
        return NULL;
    }

    PyObject *r = PyUnicode_FromString(s);
    free(s);
    return r;
}

PyObject *
menai_value_describe_float(MenaiValue *val)
{
    double v = ((MenaiFloat *)val)->value;
    PyObject *pf = PyFloat_FromDouble(v);
    if (!pf) {
        return NULL;
    }

    PyObject *r = PyObject_Str(pf);
    Py_DECREF(pf);
    return r;
}

PyObject *
menai_value_describe_complex(MenaiValue *val)
{
    MenaiComplex *c = (MenaiComplex *)val;
    double r = c->real;
    double im = c->imag;

    /*
     * Replicate the Python-layer describe logic: use integer notation when
     * the component is an exact integer, otherwise use str(float).
     */
    PyObject *real_py = PyFloat_FromDouble(r);
    PyObject *imag_py = PyFloat_FromDouble(im);
    if (!real_py || !imag_py) {
        Py_XDECREF(real_py);
        Py_XDECREF(imag_py);
        return NULL;
    }

    PyObject *real_str;
    PyObject *imag_str;

    /* Format real component */
    if (r == (double)(long)r && r >= (double)LONG_MIN && r <= (double)LONG_MAX) {
        real_str = PyUnicode_FromFormat("%ld", (long)r);
    } else {
        real_str = PyObject_Str(real_py);
    }

    /* Format imaginary component */
    if (im == (double)(long)im && im >= (double)LONG_MIN && im <= (double)LONG_MAX) {
        imag_str = PyUnicode_FromFormat("%ld", (long)im);
    } else {
        imag_str = PyObject_Str(imag_py);
    }

    Py_DECREF(real_py);
    Py_DECREF(imag_py);

    if (!real_str || !imag_str) {
        Py_XDECREF(real_str);
        Py_XDECREF(imag_str);
        return NULL;
    }

    PyObject *result;
    if (r == 0.0 && im == 0.0) {
        result = PyUnicode_FromString("0+0j");
    } else if (r == 0.0) {
        result = PyUnicode_FromFormat("%Uj", imag_str);
    } else if (im >= 0.0) {
        result = PyUnicode_FromFormat("%U+%Uj", real_str, imag_str);
    } else {
        result = PyUnicode_FromFormat("%U%Uj", real_str, imag_str);
    }

    Py_DECREF(real_str);
    Py_DECREF(imag_str);
    return result;
}

PyObject *
menai_value_describe_string(MenaiValue *val)
{
    /*
     * Convert to Python unicode, then escape and wrap in double quotes,
     * matching the Python-layer MenaiString.describe() output.
     */
    PyObject *pystr = menai_string_to_pyunicode(val);
    if (!pystr) {
        return NULL;
    }

    Py_ssize_t len;
    const char *utf8 = PyUnicode_AsUTF8AndSize(pystr, &len);
    if (!utf8) {
        Py_DECREF(pystr);
        return NULL;
    }

    PyObject *parts = PyList_New(0);
    if (!parts) {
        Py_DECREF(pystr);
        return NULL;
    }

    /* Walk the UTF-8 bytes and build escaped representation */
    for (Py_ssize_t i = 0; i < len; ) {
        unsigned char ch = (unsigned char)utf8[i];
        PyObject *piece = NULL;

        if (ch == '"') {
            piece = PyUnicode_FromString("\\\"");
            i++;
        } else if (ch == '\\') {
            piece = PyUnicode_FromString("\\\\");
            i++;
        } else if (ch == '\n') {
            piece = PyUnicode_FromString("\\n");
            i++;
        } else if (ch == '\t') {
            piece = PyUnicode_FromString("\\t");
            i++;
        } else if (ch == '\r') {
            piece = PyUnicode_FromString("\\r");
            i++;
        } else if (ch < 32) {
            piece = PyUnicode_FromFormat("\\u%04x", (unsigned)ch);
            i++;
        } else {
            /* Find the end of the run of printable bytes */
            Py_ssize_t start = i;
            while (i < len) {
                unsigned char c2 = (unsigned char)utf8[i];
                if (c2 == '"' || c2 == '\\' || c2 == '\n' || c2 == '\t' || c2 == '\r' || c2 < 32) {
                    break;
                }

                i++;
            }

            piece = PyUnicode_DecodeUTF8(utf8 + start, i - start, NULL);
        }

        if (!piece) {
            Py_DECREF(parts);
            Py_DECREF(pystr);
            return NULL;
        }

        if (PyList_Append(parts, piece) < 0) {
            Py_DECREF(piece);
            Py_DECREF(parts);
            Py_DECREF(pystr);
            return NULL;
        }

        Py_DECREF(piece);
    }

    Py_DECREF(pystr);

    PyObject *empty = PyUnicode_FromString("");
    if (!empty) {
        Py_DECREF(parts);
        return NULL;
    }

    PyObject *joined = PyUnicode_Join(empty, parts);
    Py_DECREF(empty);
    Py_DECREF(parts);
    if (!joined) {
        return NULL;
    }

    PyObject *result = PyUnicode_FromFormat("\"%U\"", joined);
    Py_DECREF(joined);
    return result;
}

PyObject *
menai_value_describe_symbol(MenaiValue *val)
{
    return menai_string_to_pyunicode(((MenaiSymbol *)val)->name);
}

PyObject *
menai_value_describe_structtype(MenaiValue *val)
{
    MenaiStructType *st = (MenaiStructType *)val;
    int nf = st->nfields;

    PyObject *type_name = menai_string_to_pyunicode(st->name);
    if (!type_name) {
        return NULL;
    }

    if (nf == 0) {
        PyObject *result = PyUnicode_FromFormat("<struct-type %U ()>", type_name);
        Py_DECREF(type_name);
        return result;
    }

    PyObject *parts = PyList_New(nf);
    if (!parts) {
        Py_DECREF(type_name);
        return NULL;
    }

    for (int i = 0; i < nf; i++) {
        PyObject *fname = menai_string_to_pyunicode(st->fields[i].name);
        if (!fname) {
            Py_DECREF(parts);
            Py_DECREF(type_name);
            return NULL;
        }

        PyList_SET_ITEM(parts, i, fname);
    }

    PyObject *sep = PyUnicode_FromString(" ");
    PyObject *fields_str = sep ? PyUnicode_Join(sep, parts) : NULL;
    Py_XDECREF(sep);
    Py_DECREF(parts);
    if (!fields_str) {
        Py_DECREF(type_name);
        return NULL;
    }

    PyObject *result = PyUnicode_FromFormat("<struct-type %U (%U)>", type_name, fields_str);
    Py_DECREF(type_name);
    Py_DECREF(fields_str);
    return result;
}

PyObject *
menai_value_describe_struct(MenaiValue *val)
{
    MenaiStruct *s = (MenaiStruct *)val;
    MenaiStructType *st = (MenaiStructType *)s->struct_type;
    int nf = s->nfields;

    PyObject *type_name = menai_string_to_pyunicode(st->name);
    if (!type_name) {
        return NULL;
    }

    if (nf == 0) {
        PyObject *result = PyUnicode_FromFormat("(%U)", type_name);
        Py_DECREF(type_name);
        return result;
    }

    PyObject *parts = PyList_New(nf);
    if (!parts) {
        Py_DECREF(type_name);
        return NULL;
    }

    for (int i = 0; i < nf; i++) {
        PyObject *fd = menai_value_describe(s->items[i]);
        if (!fd) {
            Py_DECREF(parts);
            Py_DECREF(type_name);
            return NULL;
        }

        PyList_SET_ITEM(parts, i, fd);
    }

    PyObject *sep = PyUnicode_FromString(" ");
    if (!sep) {
        Py_DECREF(parts);
        return NULL;
    }

    PyObject *fields_str = PyUnicode_Join(sep, parts);
    Py_DECREF(sep);
    Py_DECREF(parts);
    if (!fields_str) {
        return NULL;
    }

    PyObject *result = PyUnicode_FromFormat("(%U %U)", type_name, fields_str);
    Py_DECREF(type_name);
    Py_DECREF(fields_str);
    return result;
}

PyObject *
menai_value_describe_list(MenaiValue *val)
{
    MenaiList *lst = (MenaiList *)val;
    Py_ssize_t n = lst->length;

    if (n == 0) {
        return PyUnicode_FromString("()");
    }

    PyObject *parts = PyList_New(n);
    if (!parts) {
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *ed = menai_value_describe(lst->elements[i]);
        if (!ed) {
            Py_DECREF(parts);
            return NULL;
        }

        PyList_SET_ITEM(parts, i, ed);
    }

    PyObject *sep = PyUnicode_FromString(" ");
    if (!sep) {
        Py_DECREF(parts);
        return NULL;
    }

    PyObject *joined = PyUnicode_Join(sep, parts);
    Py_DECREF(sep);
    Py_DECREF(parts);
    if (!joined) {
        return NULL;
    }

    PyObject *result = PyUnicode_FromFormat("(%U)", joined);
    Py_DECREF(joined);
    return result;
}

PyObject *
menai_value_describe_dict(MenaiValue *val)
{
    MenaiDict *d = (MenaiDict *)val;
    Py_ssize_t n = d->length;

    if (n == 0) {
        return PyUnicode_FromString("{}");
    }

    PyObject *pairs = PyList_New(n);
    if (!pairs) {
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *kd = menai_value_describe(d->keys[i]);
        if (!kd) {
            Py_DECREF(pairs);
            return NULL;
        }

        PyObject *vd = menai_value_describe(d->values[i]);
        if (!vd) {
            Py_DECREF(kd);
            Py_DECREF(pairs);
            return NULL;
        }

        PyObject *pair = PyUnicode_FromFormat("(%U %U)", kd, vd);
        Py_DECREF(kd);
        Py_DECREF(vd);
        if (!pair) {
            Py_DECREF(pairs);
            return NULL;
        }

        PyList_SET_ITEM(pairs, i, pair);
    }

    PyObject *sep = PyUnicode_FromString(" ");
    if (!sep) {
        Py_DECREF(pairs);
        return NULL;
    }

    PyObject *joined = PyUnicode_Join(sep, pairs);
    Py_DECREF(sep);
    Py_DECREF(pairs);
    if (!joined) {
        return NULL;
    }

    PyObject *result = PyUnicode_FromFormat("{%U}", joined);
    Py_DECREF(joined);
    return result;
}

PyObject *
menai_value_describe_set(MenaiValue *val)
{
    MenaiSet *s = (MenaiSet *)val;
    Py_ssize_t n = s->length;

    if (n == 0) {
        return PyUnicode_FromString("#{}");
    }

    PyObject *parts = PyList_New(n);
    if (!parts) {
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *ed = menai_value_describe(s->elements[i]);
        if (!ed) {
            Py_DECREF(parts);
            return NULL;
        }

        PyList_SET_ITEM(parts, i, ed);
    }

    PyObject *sep = PyUnicode_FromString(" ");
    if (!sep) {
        Py_DECREF(parts);
        return NULL;
    }

    PyObject *joined = PyUnicode_Join(sep, parts);
    Py_DECREF(sep);
    Py_DECREF(parts);
    if (!joined) {
        return NULL;
    }

    PyObject *result = PyUnicode_FromFormat("#{%U}", joined);
    Py_DECREF(joined);
    return result;
}

PyObject *
menai_value_describe_function(MenaiValue *val)
{
    MenaiFunction *fn = (MenaiFunction *)val;
    MenaiCodeObject *co = fn->bytecode;
    Py_ssize_t np = co->nparam_names;

    PyObject *param_str;
    if (np == 0) {
        param_str = PyUnicode_FromString("");
    } else if (co->is_variadic && np > 0) {
        if (np == 1) {
            param_str = PyUnicode_FromString(co->param_names[0]);
        } else {
            PyObject *regular = PyList_New(np - 1);
            if (!regular) {
                return NULL;
            }

            for (Py_ssize_t i = 0; i < np - 1; i++) {
                PyObject *p = PyUnicode_FromString(co->param_names[i]);
                if (!p) {
                    Py_DECREF(regular);
                    return NULL;
                }

                PyList_SET_ITEM(regular, i, p);
            }

            PyObject *sep = PyUnicode_FromString(" ");
            if (!sep) {
                Py_DECREF(regular);
                return NULL;
            }

            PyObject *reg_str = PyUnicode_Join(sep, regular);
            Py_DECREF(sep);
            Py_DECREF(regular);
            if (!reg_str) {
                return NULL;
            }

            PyObject *rest = PyUnicode_FromString(co->param_names[np - 1]);
            if (!rest) {
                Py_DECREF(reg_str);
                return NULL;
            }

            param_str = PyUnicode_FromFormat("%U . %U", reg_str, rest);
            Py_DECREF(reg_str);
            Py_DECREF(rest);
        }
    } else {
        PyObject *parts = PyList_New(np);
        if (!parts) {
            return NULL;
        }

        for (Py_ssize_t i = 0; i < np; i++) {
            PyObject *p = PyUnicode_FromString(co->param_names[i]);
            if (!p) {
                Py_DECREF(parts); return NULL;
            }

            PyList_SET_ITEM(parts, i, p);
        }

        PyObject *sep = PyUnicode_FromString(" ");
        if (!sep) {
            Py_DECREF(parts); return NULL;
        }

        param_str = PyUnicode_Join(sep, parts);
        Py_DECREF(sep);
        Py_DECREF(parts);
    }

    if (!param_str) {
        return NULL;
    }

    PyObject *result = PyUnicode_FromFormat("<lambda (%U)>", param_str);
    Py_DECREF(param_str);
    return result;
}

PyObject *
menai_value_describe(MenaiValue *val)
{
    MenaiType t = val->ob_type;

    switch (t) {
    case MENAITYPE_NONE:
        return menai_value_describe_none(val);

    case MENAITYPE_BOOLEAN:
        return menai_value_describe_boolean(val);

    case MENAITYPE_INTEGER:
        return menai_value_describe_integer(val);

    case MENAITYPE_FLOAT:
        return menai_value_describe_float(val);

    case MENAITYPE_COMPLEX:
        return menai_value_describe_complex(val);

    case MENAITYPE_STRING:
        return menai_value_describe_string(val);

    case MENAITYPE_SYMBOL:
        return menai_value_describe_symbol(val);

    case MENAITYPE_STRUCTTYPE:
        return menai_value_describe_structtype(val);

    case MENAITYPE_STRUCT:
        return menai_value_describe_struct(val);

    case MENAITYPE_LIST:
        return menai_value_describe_list(val);

    case MENAITYPE_DICT:
        return menai_value_describe_dict(val);

    case MENAITYPE_SET:
        return menai_value_describe_set(val);
 
    case MENAITYPE_FUNCTION:
        return menai_value_describe_function(val);
    }

    PyErr_Format(PyExc_TypeError, "menai_value_describe: unknown type %s", menai_short_type_name(t));
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
 * This is the inverse of menai_convert_value.  It is used at the C VM execute
 * boundary to ensure all values returned to Python callers are proper Python
 * objects with the full MenaiValue interface (to_python, describe, etc.).
 *
 * For MenaiFunction, bytecode is set to None because the slow Python VM will
 * never be asked to execute these functions — they are returned as values only.
 * captured_values are recursively converted to slow values.
 *
 * Returns a new reference, or NULL on error with a Python exception set.
 */
PyObject *
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
            Py_None,   /* bytecode — not needed; slow VM won't execute this */
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

int
menai_vm_bridge_init(void)
{
    /* Fetch slow-world types — needed by menai_convert_value. */
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

    Py_DECREF(slow_mod);
    slow_mod = NULL;

    /* Fetch MenaiEvalError */
    PyObject *err_mod = PyImport_ImportModule("menai.menai_error");
    if (!err_mod) {
        return 0;
    }

    MenaiEvalError_type = PyObject_GetAttrString(err_mod, "MenaiEvalError");
    Py_DECREF(err_mod);
    if (!MenaiEvalError_type) {
        return 0;
    }

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
