/*
 * menai_vm_value.c — Python boundary layer for all Menai runtime value types.
 *
 * Provides:
 *   menai_convert_value() — slow menai_value.py -> fast C type
 *
 * Also defines the boundary describe/to_python functions forward-declared in
 * menai_vm_hashtable.c.
 *
 * Module name: menai.menai_vm_value
 * Exported singletons: Menai_NONE, Menai_BOOLEAN_TRUE, Menai_BOOLEAN_FALSE,
 *                      Menai_LIST_EMPTY, Menai_DICT_EMPTY, Menai_SET_EMPTY
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

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
#include "menai_vm_value.h"
#include "menai_vm_hashtable.h"

/*
 * Module-level singletons
 */
static MenaiValue *_Menai_EMPTY_LIST = NULL;
static MenaiValue *_Menai_EMPTY_DICT = NULL;
static MenaiValue *_Menai_EMPTY_SET = NULL;

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
    assert(!( Py_TYPE(src) == (PyTypeObject *)&MenaiNone_Type      ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiBoolean_Type    ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiInteger_Type    ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiFloat_Type      ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiComplex_Type    ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiString_Type     ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiSymbol_Type     ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiList_Type       ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiDict_Type       ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiSet_Type        ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiFunction_Type   ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiStructType_Type ||
              Py_TYPE(src) == (PyTypeObject *)&MenaiStruct_Type ));
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

        /* Bignum — convert via MenaiInt */
        MenaiInt big;
        menai_int_init(&big);
        if (menai_int_from_pylong(v, &big) < 0) {
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
        MenaiValue **arr = n > 0 ? (MenaiValue **)malloc(n * sizeof(MenaiValue *)) : NULL;
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

                free(arr);
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

/* ---------------------------------------------------------------------------
 * Boundary describe functions — forward-declared in menai_vm_hashtable.c
 * ------------------------------------------------------------------------- */

PyObject *
menai_value_describe_none(MenaiValue *val)
{
    (void)val;
    return PyUnicode_FromString("#none");
}

PyObject *
menai_value_describe_boolean(MenaiValue *val)
{
    int v = ((MenaiBoolean_Object *)val)->value;
    return PyUnicode_FromString(v ? "#t" : "#f");
}

PyObject *
menai_value_describe_integer(MenaiValue *val)
{
    MenaiInteger_Object *obj = (MenaiInteger_Object *)val;
    if (!obj->is_big) {
        return PyUnicode_FromFormat("%ld", obj->small);
    }

    char *s = NULL;
    if (menai_int_to_string(&obj->big, 10, &s) < 0) {
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
    MenaiComplex_Object *c = (MenaiComplex_Object *)val;
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
    MenaiList_Object *lst = (MenaiList_Object *)val;
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
    MenaiDict_Object *d = (MenaiDict_Object *)val;
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
    MenaiSet_Object *s = (MenaiSet_Object *)val;
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

/* ---------------------------------------------------------------------------
 * Boundary to_python functions — forward-declared in menai_vm_hashtable.c
 * ------------------------------------------------------------------------- */

PyObject *
menai_value_to_python_none(MenaiValue *val)
{
    (void)val;
    Py_RETURN_NONE;
}

PyObject *
menai_value_to_python_boolean(MenaiValue *val)
{
    int v = ((MenaiBoolean_Object *)val)->value;
    return PyBool_FromLong(v);
}

PyObject *
menai_value_to_python_integer(MenaiValue *val)
{
    MenaiInteger_Object *obj = (MenaiInteger_Object *)val;
    if (!obj->is_big) {
        return PyLong_FromLong(obj->small);
    }

    return menai_int_to_pylong(&obj->big);
}

PyObject *
menai_value_to_python_float(MenaiValue *val)
{
    return PyFloat_FromDouble(((MenaiFloat *)val)->value);
}

PyObject *
menai_value_to_python_complex(MenaiValue *val)
{
    MenaiComplex_Object *c = (MenaiComplex_Object *)val;
    return PyComplex_FromDoubles(c->real, c->imag);
}

PyObject *
menai_value_to_python_string(MenaiValue *val)
{
    return menai_string_to_pyunicode(val);
}

PyObject *
menai_value_to_python_symbol(MenaiValue *val)
{
    return menai_string_to_pyunicode(((MenaiSymbol *)val)->name);
}

PyObject *
menai_value_to_python_structtype(MenaiValue *val)
{
    MenaiStructType *st = (MenaiStructType *)val;
    PyObject *name = menai_string_to_pyunicode(st->name);
    if (!name) {
        return NULL;
    }

    PyObject *result = PyUnicode_FromFormat("<struct-type %U>", name);
    Py_DECREF(name);
    return result;
}

PyObject *
menai_value_to_python_struct(MenaiValue *val)
{
    MenaiStruct *s = (MenaiStruct *)val;
    MenaiStructType *st = (MenaiStructType *)s->struct_type;
    int nf = s->nfields;

    PyObject *result = PyDict_New();
    if (!result) {
        return NULL;
    }

    for (int i = 0; i < nf; i++) {
        PyObject *fname = menai_string_to_pyunicode(st->fields[i].name);
        if (!fname) {
            Py_DECREF(result);
            return NULL;
        }

        PyObject *fval = menai_value_to_python(s->items[i]);
        if (!fval) {
            Py_DECREF(fname);
            Py_DECREF(result);
            return NULL;
        }

        int ok = PyDict_SetItem(result, fname, fval);
        Py_DECREF(fname);
        Py_DECREF(fval);
        if (ok < 0) {
            Py_DECREF(result);
            return NULL;
        }
    }

    return result;
}

PyObject *
menai_value_to_python_list(MenaiValue *val)
{
    MenaiList_Object *lst = (MenaiList_Object *)val;
    Py_ssize_t n = lst->length;

    PyObject *result = PyList_New(n);
    if (!result) {
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *item = menai_value_to_python(lst->elements[i]);
        if (!item) {
            Py_DECREF(result);
            return NULL;
        }

        PyList_SET_ITEM(result, i, item);
    }

    return result;
}

PyObject *
menai_value_to_python_dict(MenaiValue *val)
{
    MenaiDict_Object *d = (MenaiDict_Object *)val;
    Py_ssize_t n = d->length;

    PyObject *result = PyDict_New();
    if (!result) {
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        MenaiValue *k = d->keys[i];
        MenaiType *kt = k->ob_type;
        PyObject *py_key;

        /* Use string representation for Python dict keys, matching Python layer */
        if (kt == &MenaiString_Type) {
            py_key = menai_string_to_pyunicode(k);
        } else if (kt == &MenaiSymbol_Type) {
            py_key = menai_string_to_pyunicode(((MenaiSymbol *)k)->name);
        } else {
            /* Non-string/symbol keys are stringified, matching slow VM behaviour */
            PyObject *native = menai_value_to_python(k);
            if (!native) {
                Py_DECREF(result); return NULL;
            }

            py_key = PyObject_Str(native);
            Py_DECREF(native);
        }

        if (!py_key) {
            Py_DECREF(result);
            return NULL;
        }

        PyObject *py_val = menai_value_to_python(d->values[i]);
        if (!py_val) {
            Py_DECREF(py_key);
            Py_DECREF(result);
            return NULL;
        }

        int ok = PyDict_SetItem(result, py_key, py_val);
        Py_DECREF(py_key);
        Py_DECREF(py_val);
        if (ok < 0) {
            Py_DECREF(result);
            return NULL;
        }
    }

    return result;
}

PyObject *
menai_value_to_python_set(MenaiValue *val)
{
    MenaiSet_Object *s = (MenaiSet_Object *)val;
    Py_ssize_t n = s->length;

    PyObject *result = PySet_New(NULL);
    if (!result) {
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        MenaiValue *elem = s->elements[i];
        MenaiType *et = elem->ob_type;
        PyObject *py_elem;

        if (et == &MenaiString_Type) {
            py_elem = menai_string_to_pyunicode(elem);
        } else if (et == &MenaiSymbol_Type) {
            py_elem = menai_string_to_pyunicode(((MenaiSymbol *)elem)->name);
        } else {
            py_elem = menai_value_to_python(elem);
        }

        if (!py_elem) {
            Py_DECREF(result);
            return NULL;
        }

        int ok = PySet_Add(result, py_elem);
        Py_DECREF(py_elem);
        if (ok < 0) {
            Py_DECREF(result);
            return NULL;
        }
    }

    return result;
}

PyObject *
menai_value_to_python_function(MenaiValue *val)
{
    /* Functions return themselves as opaque Python objects */
    Py_INCREF((PyObject *)val);
    return (PyObject *)val;
}

/* ---------------------------------------------------------------------------
 * Module init
 * ------------------------------------------------------------------------- */

/* ---------------------------------------------------------------------------
 * Python-facing methods and getsets for all fast value types.
 *
 * These are patched onto each PyTypeObject before PyType_Ready() is called
 * in _menai_vm_value_init().  They provide the Python API expected by
 * menai.py and tests: type_name(), describe(), to_python(), and properties
 * such as .pairs, .value, .parameters, etc.
 * ------------------------------------------------------------------------- */

/* Shared method wrappers — dispatch via menai_value_describe/to_python */

static PyObject *
py_type_name(PyObject *self, PyObject *args)
{
    (void)args;
    MenaiType *t = (MenaiType *)Py_TYPE(self);
    if (t == &MenaiNone_Type) {
        return PyUnicode_FromString("none");
    }

    if (t == &MenaiBoolean_Type) {
        return PyUnicode_FromString("boolean");
    }

    if (t == &MenaiInteger_Type) {
        return PyUnicode_FromString("integer");
    }

    if (t == &MenaiFloat_Type) {
        return PyUnicode_FromString("float");
    }

    if (t == &MenaiComplex_Type) {
        return PyUnicode_FromString("complex");
    }

    if (t == &MenaiString_Type) {
        return PyUnicode_FromString("string");
    }

    if (t == &MenaiSymbol_Type) {
        return PyUnicode_FromString("symbol");
    }

    if (t == &MenaiList_Type) {
        return PyUnicode_FromString("list");
    }

    if (t == &MenaiDict_Type) {
        return PyUnicode_FromString("dict");
    }

    if (t == &MenaiSet_Type) {
        return PyUnicode_FromString("set");
    }

    if (t == &MenaiFunction_Type) {
        return PyUnicode_FromString("function");
    }

    if (t == &MenaiStructType_Type) {
        return PyUnicode_FromString("struct-type");
    }

    if (t == &MenaiStruct_Type) {
        return PyUnicode_FromString("struct");
    }

    return PyUnicode_FromString(t->tp_name);
}

static PyObject *
py_describe(PyObject *self, PyObject *args)
{
    (void)args;
    return menai_value_describe((MenaiValue *)self);
}

static PyObject *
py_to_python(PyObject *self, PyObject *args)
{
    (void)args;
    return menai_value_to_python((MenaiValue *)self);
}

static PyMethodDef _shared_methods[] = {
    {"type_name", py_type_name, METH_NOARGS, NULL},
    {"describe",  py_describe,  METH_NOARGS, NULL},
    {"to_python", py_to_python, METH_NOARGS, NULL},
    {NULL, NULL, 0, NULL}
};

/* MenaiDict.pairs — returns tuple of (key, value) 2-tuples */
static PyObject *
dict_get_pairs(PyObject *self, void *closure)
{
    (void)closure;
    MenaiDict_Object *d = (MenaiDict_Object *)self;
    Py_ssize_t n = d->length;
    PyObject *tup = PyTuple_New(n);
    if (!tup) {
        return NULL;
    }

    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *pair = PyTuple_Pack(2,
            (PyObject *)d->keys[i], (PyObject *)d->values[i]);
        if (!pair) {
            Py_DECREF(tup); return NULL;
        }

        PyTuple_SET_ITEM(tup, i, pair);
    }

    return tup;
}

static PyGetSetDef _dict_getsets[] = {
    {"pairs", dict_get_pairs, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

/* MenaiString.value — returns Python str */
static PyObject *
string_get_value(PyObject *self, void *closure)
{
    (void)closure;
    return menai_string_to_pyunicode((MenaiValue *)self);
}

static PyGetSetDef _string_getsets[] = {
    {"value", string_get_value, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

/* MenaiFloat.value */
static PyObject *
float_get_value(PyObject *self, void *closure)
{
    (void)closure;
    return PyFloat_FromDouble(((MenaiFloat *)self)->value);
}

static PyGetSetDef _float_getsets[] = {
    {"value", float_get_value, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

/* MenaiInteger.value */
static PyObject *
integer_get_value(PyObject *self, void *closure)
{
    (void)closure;
    MenaiInteger_Object *obj = (MenaiInteger_Object *)self;
    if (!obj->is_big) {
        return PyLong_FromLong(obj->small);
    }

    return menai_int_to_pylong(&obj->big);
}

static PyGetSetDef _integer_getsets[] = {
    {"value", integer_get_value, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

/* MenaiFunction getsets */
static PyObject *
func_get_parameters(PyObject *self, void *closure)
{
    (void)closure;
    MenaiCodeObject *co = ((MenaiFunction *)self)->bytecode;
    PyObject *lst = PyList_New(co->param_count);
    if (!lst) {
        return NULL;
    }

    /* param_names are not stored on MenaiCodeObject yet — return empty list. */
    return lst;
}

static PyObject *
func_get_name(PyObject *self, void *closure)
{
    (void)closure;
    MenaiCodeObject *co = ((MenaiFunction *)self)->bytecode;
    if (co->name) {
        return PyUnicode_FromString(co->name);
    }

    Py_RETURN_NONE;
}

static PyObject *
func_get_bytecode(PyObject *self, void *closure)
{
    (void)closure;
    /* The Python CodeObject is not retained — return None for now. */
    Py_RETURN_NONE;
}

static PyObject *
func_get_is_variadic(PyObject *self, void *closure)
{
    (void)closure;
    return PyBool_FromLong(((MenaiFunction *)self)->bytecode->is_variadic);
}

static PyObject *
func_get_param_count(PyObject *self, void *closure)
{
    (void)closure;
    return PyLong_FromLong(((MenaiFunction *)self)->bytecode->param_count);
}

static PyObject *
func_get_captured_values(PyObject *self, void *closure)
{
    (void)closure;
    MenaiFunction *f = (MenaiFunction *)self;
    Py_ssize_t ncap = f->ncap;
    PyObject *lst = PyList_New(ncap);
    if (!lst) {
        return NULL;
    }

    for (Py_ssize_t i = 0; i < ncap; i++) {
        PyObject *cv = f->captures[i] ? (PyObject *)f->captures[i] : Py_None;
        Py_INCREF(cv);
        PyList_SET_ITEM(lst, i, cv);
    }

    return lst;
}

static int
func_set_captured_values(PyObject *self, PyObject *value, void *closure)
{
    (void)closure;
    if (!PyList_Check(value)) {
        PyErr_SetString(PyExc_TypeError, "captured_values must be a list");
        return -1;
    }

    MenaiFunction *f = (MenaiFunction *)self;
    Py_ssize_t ncap = f->ncap;
    if (PyList_GET_SIZE(value) != ncap) {
        PyErr_SetString(PyExc_ValueError,
            "captured_values length does not match function capture count");
        return -1;
    }

    for (Py_ssize_t i = 0; i < ncap; i++) {
        MenaiValue *nv = (MenaiValue *)PyList_GET_ITEM(value, i);
        menai_retain(nv);
        menai_xrelease(f->captures[i]);
        f->captures[i] = nv;
    }

    return 0;
}

static PyGetSetDef _function_getsets[] = {
    {"parameters", func_get_parameters, NULL, NULL, NULL},
    {"name", func_get_name, NULL, NULL, NULL},
    {"bytecode", func_get_bytecode, NULL, NULL, NULL},
    {"is_variadic", func_get_is_variadic, NULL, NULL, NULL},
    {"param_count", func_get_param_count, NULL, NULL, NULL},
    {"captured_values", func_get_captured_values, func_set_captured_values,  NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

/* MenaiBoolean.value */
static PyObject *
boolean_get_value(PyObject *self, void *closure)
{
    (void)closure;
    return PyBool_FromLong(((MenaiBoolean_Object *)self)->value);
}

static PyGetSetDef _boolean_getsets[] = {
    {"value", boolean_get_value, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

/* MenaiSymbol.name */
static PyObject *
symbol_get_name(PyObject *self, void *closure)
{
    (void)closure;
    return menai_string_to_pyunicode(((MenaiSymbol *)self)->name);
}

static PyGetSetDef _symbol_getsets[] = {
    {"name", symbol_get_name, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

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

static struct PyModuleDef module_def = {
    PyModuleDef_HEAD_INIT,
    "menai.menai_vm_value",
    NULL,
    -1,
    NULL
};

PyObject *
_menai_vm_value_init(void)
{
    /* Fetch slow-world types — needed by menai_convert_value. */
    PyObject *slow_mod = PyImport_ImportModule("menai.menai_value");
    if (!slow_mod) {
        return NULL;
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
        return NULL;
    }

    MenaiEvalError_type = PyObject_GetAttrString(err_mod, "MenaiEvalError");
    Py_DECREF(err_mod);
    if (!MenaiEvalError_type) {
        return NULL;
    }

    /*
     * Patch tp_methods and tp_getset onto each type before PyType_Ready.
     * All types get the shared methods (type_name, describe, to_python).
     * Selected types also get type-specific getsets.
     */
    MenaiNone_Type.tp_methods = _shared_methods;
    MenaiBoolean_Type.tp_methods = _shared_methods;
    MenaiBoolean_Type.tp_getset = _boolean_getsets;
    MenaiInteger_Type.tp_methods = _shared_methods;
    MenaiInteger_Type.tp_getset = _integer_getsets;
    MenaiFloat_Type.tp_methods = _shared_methods;
    MenaiFloat_Type.tp_getset = _float_getsets;
    MenaiComplex_Type.tp_methods = _shared_methods;
    MenaiString_Type.tp_methods = _shared_methods;
    MenaiString_Type.tp_getset = _string_getsets;
    MenaiSymbol_Type.tp_methods = _shared_methods;
    MenaiSymbol_Type.tp_getset = _symbol_getsets;
    MenaiList_Type.tp_methods = _shared_methods;
    MenaiDict_Type.tp_methods = _shared_methods;
    MenaiDict_Type.tp_getset = _dict_getsets;
    MenaiSet_Type.tp_methods = _shared_methods;
    MenaiFunction_Type.tp_methods = _shared_methods;
    MenaiFunction_Type.tp_getset = _function_getsets;
    MenaiStructType_Type.tp_methods = _shared_methods;
    MenaiStruct_Type.tp_methods = _shared_methods;

    if (menai_vm_string_init(MenaiEvalError_type) < 0) {
        return NULL;
    }

    if (menai_vm_none_init() < 0) {
        return NULL;
    }

    if (menai_vm_boolean_init() < 0) {
        return NULL;
    }

    if (menai_vm_float_init() < 0) {
        return NULL;
    }

    if (menai_vm_integer_init() < 0) {
        return NULL;
    }

    if (menai_vm_complex_init() < 0) {
        return NULL;
    }

    if (menai_vm_function_init() < 0) {
        return NULL;
    }

    if (menai_vm_symbol_init() < 0) {
        return NULL;
    }

    if (menai_vm_list_init() < 0) {
        return NULL;
    }

    if (menai_vm_set_init() < 0) {
        return NULL;
    }

    if (menai_vm_struct_init() < 0) {
        return NULL;
    }

    if (menai_vm_dict_init() < 0) {
        return NULL;
    }

    /* Call PyType_Ready for types whose init functions don't do it. */
    if (PyType_Ready(&MenaiString_Type) < 0) {
        return NULL;
    }

    /* Create module */
    PyObject *module = PyModule_Create(&module_def);
    if (!module) {
        return NULL;
    }

    /* Register in sys.modules so Python code can import menai.menai_vm_value
     * after menai_vm_c has been loaded. */
    PyObject *sys_modules = PySys_GetObject("modules");
    if (sys_modules == NULL) {
        Py_DECREF(module);
        return NULL;
    }

    if (PyDict_SetItemString(sys_modules, "menai.menai_vm_value", module) < 0) {
        Py_DECREF(module);
        return NULL;
    }

    /* Create empty collection singletons */
    _Menai_EMPTY_LIST = menai_list_new_empty();
    if (!_Menai_EMPTY_LIST) {
        Py_DECREF(module);
        return NULL;
    }

    _Menai_EMPTY_DICT = menai_dict_new_empty();
    if (!_Menai_EMPTY_DICT) {
        Py_DECREF(module);
        return NULL;
    }

    _Menai_EMPTY_SET = menai_set_new_empty();
    if (!_Menai_EMPTY_SET) {
        Py_DECREF(module);
        return NULL;
    }

    /* Add singletons to module.  menai_retain increments ob_refcnt at offset 0,
     * which is the same field Py_INCREF would increment given the shared layout. */
    MenaiValue *none_val = menai_none_singleton();
    menai_retain(none_val);
    if (PyModule_AddObject(module, "Menai_NONE", (PyObject *)none_val) < 0) {
        menai_release(none_val);
        Py_DECREF(module);
        return NULL;
    }

    MenaiValue *bool_true = menai_boolean_true();
    menai_retain(bool_true);
    if (PyModule_AddObject(module, "Menai_BOOLEAN_TRUE", (PyObject *)bool_true) < 0) {
        menai_release(bool_true);
        Py_DECREF(module);
        return NULL;
    }

    MenaiValue *bool_false = menai_boolean_false();
    menai_retain(bool_false);
    if (PyModule_AddObject(module, "Menai_BOOLEAN_FALSE", (PyObject *)bool_false) < 0) {
        menai_release(bool_false);
        Py_DECREF(module);
        return NULL;
    }

    struct {
        const char *name;
        MenaiValue **obj;
    } singletons[] = {
        {"Menai_LIST_EMPTY", &_Menai_EMPTY_LIST},
        {"Menai_DICT_EMPTY", &_Menai_EMPTY_DICT},
        {"Menai_SET_EMPTY", &_Menai_EMPTY_SET},
    };
    for (int i = 0; i < (int)(sizeof(singletons)/sizeof(singletons[0])); i++) {
        menai_retain(*singletons[i].obj);
        if (PyModule_AddObject(module, singletons[i].name, (PyObject *)*singletons[i].obj) < 0) {
            menai_release(*singletons[i].obj);
            Py_DECREF(module);
            return NULL;
        }
    }

    /* Add type objects to the module so Python code can import them for
     * isinstance checks and type introspection. */
    struct {
        const char *name;
        PyTypeObject *type;
    } types[] = {
        {"MenaiNone", &MenaiNone_Type},
        {"MenaiBoolean", &MenaiBoolean_Type},
        {"MenaiInteger", &MenaiInteger_Type},
        {"MenaiFloat", &MenaiFloat_Type},
        {"MenaiComplex", &MenaiComplex_Type},
        {"MenaiString", &MenaiString_Type},
        {"MenaiSymbol", &MenaiSymbol_Type},
        {"MenaiList", &MenaiList_Type},
        {"MenaiDict", &MenaiDict_Type},
        {"MenaiSet", &MenaiSet_Type},
        {"MenaiFunction", &MenaiFunction_Type},
        {"MenaiStructType", &MenaiStructType_Type},
        {"MenaiStruct", &MenaiStruct_Type},
    };
    for (int i = 0; i < (int)(sizeof(types)/sizeof(types[0])); i++) {
        Py_INCREF(types[i].type);
        if (PyModule_AddObject(module, types[i].name, (PyObject *)types[i].type) < 0) {
            Py_DECREF(types[i].type);
            Py_DECREF(module);
            return NULL;
        }
    }

    return module;

fail:
    Py_XDECREF(slow_mod);
    return NULL;
}
