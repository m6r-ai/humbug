/*
 * menai_vm_value.c — Python boundary layer for all Menai runtime value types.
 *
 * Provides:
 *   menai_convert_value()          — slow menai_value.py -> fast C type
 *   menai_convert_code_constants() — walk CodeObject tree, convert constants in-place
 *   menai_build_closure_caches()   — walk CodeObject tree, build ClosureCache structs
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
static MenaiValue _Menai_EMPTY_LIST = NULL;
static MenaiValue _Menai_EMPTY_DICT = NULL;
static MenaiValue _Menai_EMPTY_SET = NULL;

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
 * _is_fast — return 1 if obj is already a fast C value type.
 *
 * MenaiObject_HEAD and PyObject_HEAD share the same layout: both begin with
 * a size_t refcount followed by a type pointer.  Py_TYPE() reads the type
 * pointer at the same offset regardless of which header was used, so
 * comparing the result against (PyTypeObject *)&MenaiXxx_Type is a valid
 * pointer identity test.
 */
static int
_is_fast(PyObject *obj)
{
    PyTypeObject *t = Py_TYPE(obj);
    return (t == (PyTypeObject *)&MenaiNone_Type     ||
            t == (PyTypeObject *)&MenaiBoolean_Type  ||
            t == (PyTypeObject *)&MenaiInteger_Type  ||
            t == (PyTypeObject *)&MenaiFloat_Type    ||
            t == (PyTypeObject *)&MenaiComplex_Type  ||
            t == (PyTypeObject *)&MenaiString_Type   ||
            t == (PyTypeObject *)&MenaiSymbol_Type   ||
            t == (PyTypeObject *)&MenaiList_Type     ||
            t == (PyTypeObject *)&MenaiDict_Type     ||
            t == (PyTypeObject *)&MenaiSet_Type      ||
            t == (PyTypeObject *)&MenaiFunction_Type ||
            t == (PyTypeObject *)&MenaiStructType_Type ||
            t == (PyTypeObject *)&MenaiStruct_Type);
}

/*
 * menai_convert_value — convert one slow menai_value.py object to a fast type.
 *
 * Returns a new reference.  If src is already a fast type, returns it with
 * an incremented refcount.  For MenaiFunction, captured_values are NOT
 * recursively converted here — call_setup in the VM does that lazily at call
 * time to avoid cycles in letrec closures.
 */
PyObject *
menai_convert_value(PyObject *src)
{
    if (_is_fast(src)) {
        Py_INCREF(src);
        return src;
    }

    PyTypeObject *t = Py_TYPE(src);

    if (t == Slow_NoneType) {
        MenaiValue s = menai_none_singleton();
        menai_retain(s);
        return (PyObject *)s;
    }

    if (t == Slow_BooleanType) {
        PyObject *bv = PyObject_GetAttrString(src, "value");
        if (!bv) return NULL;
        int b = PyObject_IsTrue(bv);
        Py_DECREF(bv);
        if (b < 0) return NULL;
        MenaiValue r = b ? menai_boolean_true() : menai_boolean_false();
        menai_retain(r);
        return (PyObject *)r;
    }

    if (t == Slow_IntegerType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
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
            return (PyObject *)menai_integer_from_long(lv);
        }
        /* Bignum — convert via MenaiInt */
        MenaiInt big;
        menai_int_init(&big);
        if (menai_int_from_pylong(v, &big) < 0) {
            Py_DECREF(v);
            return NULL;
        }
        Py_DECREF(v);
        return (PyObject *)menai_integer_from_bigint(big);
    }

    if (t == Slow_FloatType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
        double d = PyFloat_AsDouble(v);
        Py_DECREF(v);
        if (d == -1.0 && PyErr_Occurred()) return NULL;
        return (PyObject *)menai_float_alloc(d);
    }

    if (t == Slow_ComplexType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
        double real = PyComplex_RealAsDouble(v);
        double imag = PyComplex_ImagAsDouble(v);
        Py_DECREF(v);
        return (PyObject *)menai_complex_alloc(real, imag);
    }

    if (t == Slow_StringType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
        MenaiValue r = menai_string_from_pyunicode(v);
        Py_DECREF(v);
        return (PyObject *)r;
    }

    if (t == Slow_SymbolType) {
        PyObject *n = PyObject_GetAttrString(src, "name");
        if (!n) return NULL;
        MenaiValue r = menai_symbol_alloc(n);
        Py_DECREF(n);
        return (PyObject *)r;
    }

    if (t == Slow_ListType) {
        PyObject *elems = PyObject_GetAttrString(src, "elements");
        if (!elems) return NULL;

        Py_ssize_t n = PyTuple_GET_SIZE(elems);
        MenaiValue *arr = n > 0 ? (MenaiValue *)malloc(n * sizeof(MenaiValue)) : NULL;
        if (n > 0 && !arr) {
            Py_DECREF(elems);
            PyErr_NoMemory();
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            arr[i] = (MenaiValue)menai_convert_value(PyTuple_GET_ITEM(elems, i));
            if (!arr[i]) {
                for (Py_ssize_t j = 0; j < i; j++) menai_release(arr[j]);
                free(arr);
                Py_DECREF(elems);
                return NULL;
            }
        }

        Py_DECREF(elems);
        return (PyObject *)menai_list_from_array_steal(arr, n);
    }

    if (t == Slow_DictType) {
        PyObject *pairs = PyObject_GetAttrString(src, "pairs");
        if (!pairs) return NULL;
        Py_ssize_t n = PyTuple_GET_SIZE(pairs);
        MenaiValue *keys = n > 0
            ? (MenaiValue *)malloc(n * sizeof(MenaiValue)) : NULL;
        MenaiValue *values = n > 0
            ? (MenaiValue *)malloc(n * sizeof(MenaiValue)) : NULL;
        Py_hash_t *hashes = n > 0
            ? (Py_hash_t *)malloc(n * sizeof(Py_hash_t)) : NULL;
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
            MenaiValue fk = (MenaiValue)menai_convert_value(PyTuple_GET_ITEM(pair, 0));
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
            MenaiValue fv = (MenaiValue)menai_convert_value(PyTuple_GET_ITEM(pair, 1));
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
        return (PyObject *)menai_dict_from_arrays_steal(keys, values, hashes, n);
    }

    if (t == Slow_SetType) {
        PyObject *elems = PyObject_GetAttrString(src, "elements");
        if (!elems) return NULL;
        Py_ssize_t n = PyTuple_GET_SIZE(elems);
        MenaiValue *elements = n > 0
            ? (MenaiValue *)malloc(n * sizeof(MenaiValue)) : NULL;
        Py_hash_t *hashes = n > 0
            ? (Py_hash_t *)malloc(n * sizeof(Py_hash_t)) : NULL;
        if (n > 0 && (!elements || !hashes)) {
            free(elements);
            free(hashes);
            Py_DECREF(elems);
            PyErr_NoMemory();
            return NULL;
        }
        for (Py_ssize_t i = 0; i < n; i++) {
            MenaiValue fe = (MenaiValue)menai_convert_value(PyTuple_GET_ITEM(elems, i));
            if (!fe) {
                for (Py_ssize_t j = 0; j < i; j++) menai_release(elements[j]);
                free(elements);
                free(hashes);
                Py_DECREF(elems);
                return NULL;
            }
            Py_hash_t h = menai_value_hash(fe);
            if (h == -1) {
                menai_release(fe);
                for (Py_ssize_t j = 0; j < i; j++) menai_release(elements[j]);
                free(elements);
                free(hashes);
                Py_DECREF(elems);
                return NULL;
            }
            elements[i] = fe;
            hashes[i] = h;
        }
        Py_DECREF(elems);
        return (PyObject *)menai_set_from_arrays_steal(elements, hashes, n);
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
        if (!args) return NULL;

        MenaiValue r = menai_struct_type_new_from_args(args);
        Py_DECREF(args);
        return (PyObject *)r;
    }

    if (t == Slow_StructType) {
        PyObject *st = PyObject_GetAttrString(src, "struct_type");
        PyObject *fields = PyObject_GetAttrString(src, "fields");
        if (!st || !fields) {
            Py_XDECREF(st);
            Py_XDECREF(fields);
            return NULL;
        }

        MenaiValue fast_st = (MenaiValue)menai_convert_value(st);
        Py_DECREF(st);
        if (!fast_st) {
            Py_DECREF(fields);
            return NULL;
        }

        Py_ssize_t n = PyTuple_GET_SIZE(fields);
        MenaiValue *fast_arr = n > 0
            ? (MenaiValue *)malloc(n * sizeof(MenaiValue)) : NULL;
        if (n > 0 && !fast_arr) {
            menai_release(fast_st);
            Py_DECREF(fields);
            PyErr_NoMemory();
            return NULL;
        }

        for (Py_ssize_t i = 0; i < n; i++) {
            MenaiValue ff = (MenaiValue)menai_convert_value(PyTuple_GET_ITEM(fields, i));
            if (!ff) {
                for (Py_ssize_t j = 0; j < i; j++) menai_release(fast_arr[j]);
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
        MenaiValue r = menai_struct_alloc(fast_st, fast_arr, n);
        for (Py_ssize_t i = 0; i < n; i++) menai_release(fast_arr[i]);
        free(fast_arr);
        menai_release(fast_st);
        return (PyObject *)r;
    }

    if (t == Slow_FunctionType) {
        PyObject *bc = PyObject_GetAttrString(src, "bytecode");
        PyObject *cap = PyObject_GetAttrString(src, "captured_values");
        if (!bc || !cap) {
            Py_XDECREF(bc);
            Py_XDECREF(cap);
            return NULL;
        }

        /*
         * menai_build_closure_caches has already run, so bc carries a
         * _closure_cache capsule with all frame metadata pre-extracted.
         * Allocate the fast function from that cache (captures initialised
         * to None), then patch in the converted captured values.
         */
        PyObject *capsule = PyObject_GetAttrString(bc, "_closure_cache");
        Py_DECREF(bc);
        if (!capsule) {
            Py_DECREF(cap);
            return NULL;
        }
        const ClosureCache *cc = (const ClosureCache *)PyCapsule_GetPointer(
            capsule, CLOSURE_CACHE_CAPSULE_NAME);
        Py_DECREF(capsule);
        if (!cc) {
            Py_DECREF(cap);
            return NULL;
        }

        MenaiValue r = menai_function_alloc(cc, menai_none_singleton());
        if (!r) {
            Py_DECREF(cap);
            return NULL;
        }

        MenaiFunction_Object *f = (MenaiFunction_Object *)r;
        for (Py_ssize_t ci = 0; ci < cc->ncap; ci++) {
            MenaiValue fast_cv = (MenaiValue)menai_convert_value(PyList_GET_ITEM(cap, ci));
            if (!fast_cv) {
                menai_release(r);
                Py_DECREF(cap);
                return NULL;
            }
            menai_release(f->captures[ci]);  /* release the None placeholder */
            f->captures[ci] = fast_cv;       /* owns the ref from menai_convert_value */
        }
        Py_DECREF(cap);
        return (PyObject *)r;
    }

    PyErr_Format(PyExc_TypeError, "menai_convert_value: unexpected type %R", (PyObject *)t);
    return NULL;
}

static void
_closure_cache_capsule_destructor(PyObject *capsule)
{
    ClosureCache *cc = (ClosureCache *)PyCapsule_GetPointer(
        capsule, CLOSURE_CACHE_CAPSULE_NAME);
    if (!cc) return;
    Py_XDECREF(cc->parameters);
    Py_XDECREF(cc->name);
    Py_XDECREF(cc->instrs_obj);
    /* constants, names_list, and closure_caches are borrowed — not released here. */
    free(cc);
}

/*
 * menai_convert_code_constants — walk a CodeObject tree, converting all
 * constants lists in-place from slow to fast types.  Must be called before
 * menai_build_closure_caches so that any MenaiFunction constants have their
 * bytecode converted before their ClosureCache is built.
 *
 * Returns the code object (borrowed reference), or NULL on error.
 */
PyObject *
menai_convert_code_constants(PyObject *code)
{
    /*
     * Guard against processing the same CodeObject twice.  A named function's
     * CodeObject may appear both as a MenaiFunction constant and as a direct
     * child in code_objects; the _constants_converted flag prevents redundant
     * work and the double-conversion that would corrupt the constants list.
     */
    PyObject *_flag = PyObject_GetAttrString(code, "_constants_converted");
    int _already_done = (_flag == Py_True);
    Py_XDECREF(_flag);
    PyErr_Clear();
    if (_already_done) return code;

    /*
     * Convert code.constants list in-place.  For function constants, recurse
     * into their bytecode first so the nested constants are fast before the
     * outer MenaiFunction is converted.
     */
    PyObject *constants = PyObject_GetAttrString(code, "constants");
    if (!constants) return NULL;
    Py_ssize_t n = PyList_GET_SIZE(constants);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *orig = PyList_GET_ITEM(constants, i);
        /* If this constant is a function, recurse into its bytecode first. */
        PyObject *bc = PyObject_GetAttrString(orig, "bytecode");
        if (bc == NULL) {
            PyErr_Clear();  /* not a function — no bytecode attribute */
        } else if (bc != Py_None) {
            if (!menai_convert_code_constants(bc)) {
                Py_DECREF(bc);
                Py_DECREF(constants);
                return NULL;
            }
            Py_DECREF(bc);
        } else {
            Py_DECREF(bc);
        }
        PyObject *fast = menai_convert_value(orig);
        if (!fast) {
            Py_DECREF(constants);
            return NULL;
        }
        PyList_SET_ITEM(constants, i, fast);
        Py_DECREF(orig);
    }
    Py_DECREF(constants);

    /* Recurse into child code objects */
    PyObject *children = PyObject_GetAttrString(code, "code_objects");
    if (!children) return NULL;
    n = PyList_GET_SIZE(children);
    for (Py_ssize_t i = 0; i < n; i++) {
        if (!menai_convert_code_constants(PyList_GET_ITEM(children, i))) {
            Py_DECREF(children);
            return NULL;
        }
    }
    Py_DECREF(children);

    if (PyObject_SetAttrString(code, "_constants_converted", Py_True) < 0)
        return NULL;

    return code;
}

/*
 * menai_build_closure_caches — walk a CodeObject tree, building a
 * ClosureCache struct (wrapped in a PyCapsule) for each child code object.
 * OP_MAKE_CLOSURE unwraps the capsule directly — zero PyObject_GetAttrString
 * calls on the hot closure-creation path.  Must be called after
 * menai_convert_code_constants: the MenaiFunction constructor reads
 * _code_caches to initialise closure_caches on the fast function object, so
 * _code_caches must be set before any function constants are converted.
 *
 * Returns the code object (borrowed reference), or NULL on error.
 */
PyObject *
menai_build_closure_caches(PyObject *code)
{
    /*
     * Guard against processing the same CodeObject twice.  A named function's
     * CodeObject may appear both as a MenaiFunction constant and as a direct
     * child in code_objects.  The second call would rebuild _code_caches,
     * freeing the list that MenaiFunction_Object.closure_caches already
     * borrowed a pointer to, causing a use-after-free crash.
     */
    PyObject *_existing = PyObject_GetAttrString(code, "_code_caches");
    int _already_done = (_existing && PyList_Check(_existing));
    Py_XDECREF(_existing);
    PyErr_Clear();
    if (_already_done) return code;

    PyObject *children = PyObject_GetAttrString(code, "code_objects");
    if (!children) return NULL;

    /*
     * Recurse into the bytecode of any function constants, mirroring the
     * logic in menai_convert_code_constants.  A named function's CodeObject
     * may appear in constants[] but not in code_objects[], so we must visit
     * it here to ensure its _code_caches are built before it is called.
     */
    PyObject *constants = PyObject_GetAttrString(code, "constants");
    if (!constants) {
        Py_DECREF(children);
        return NULL;
    }
    Py_ssize_t nc = PyList_GET_SIZE(constants);
    for (Py_ssize_t i = 0; i < nc; i++) {
        PyObject *orig = PyList_GET_ITEM(constants, i);
        PyObject *bc = PyObject_GetAttrString(orig, "bytecode");
        if (bc == NULL) {
            PyErr_Clear();  /* not a function — no bytecode attribute */
        } else if (bc != Py_None) {
            if (!menai_build_closure_caches(bc)) {
                Py_DECREF(bc);
                Py_DECREF(constants);
                Py_DECREF(children);
                return NULL;
            }
            Py_DECREF(bc);
        } else {
            Py_DECREF(bc);
        }
    }
    Py_DECREF(constants);

    Py_ssize_t n = PyList_GET_SIZE(children);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *child = PyList_GET_ITEM(children, i);
        if (!menai_build_closure_caches(child)) {
            Py_DECREF(children);
            return NULL;
        }

        /*
         * Build a ClosureCache struct for this child and wrap it in a
         * PyCapsule.  OP_MAKE_CLOSURE unwraps the capsule and passes the
         * struct pointer directly to menai_function_alloc — zero
         * PyTuple_GET_ITEM or PyLong_AsLong calls on the hot closure-creation
         * path.
         */
        PyObject *param_names = PyObject_GetAttrString(child, "param_names");
        if (!param_names) {
            Py_DECREF(children);
            return NULL;
        }

        PyObject *param_names_tup = PySequence_Tuple(param_names);
        Py_DECREF(param_names);
        if (!param_names_tup) {
            Py_DECREF(children);
            return NULL;
        }

        PyObject *cname = PyObject_GetAttrString(child, "name");
        if (!cname) {
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }

        PyObject *is_var = PyObject_GetAttrString(child, "is_variadic");
        if (!is_var) {
            Py_DECREF(cname);
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }
        int is_variadic = PyObject_IsTrue(is_var);
        Py_DECREF(is_var);
        if (is_variadic < 0) {
            Py_DECREF(cname);
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }

        PyObject *free_vars = PyObject_GetAttrString(child, "free_vars");
        if (!free_vars) {
            Py_DECREF(cname);
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }
        Py_ssize_t ncap = PyList_GET_SIZE(free_vars);
        Py_DECREF(free_vars);

        PyObject *instrs_obj = PyObject_GetAttrString(child, "instructions");
        if (!instrs_obj) {
            Py_DECREF(cname);
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }

        PyObject *child_constants = PyObject_GetAttrString(child, "constants");
        if (!child_constants) {
            Py_DECREF(instrs_obj);
            Py_DECREF(cname);
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }

        /* child_constants is borrowed — kept alive by child (the CodeObject).
         * menai_convert_code_constants will have run before any MenaiFunction
         * is created from this cache, so ob_item will contain fast values. */

        PyObject *names_list = PyObject_GetAttrString(child, "names");
        if (!names_list) {
            Py_DECREF(child_constants);
            Py_DECREF(instrs_obj);
            Py_DECREF(cname);
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }

        PyObject *pc_obj = PyObject_GetAttrString(child, "param_count");
        if (!pc_obj) {
            Py_DECREF(names_list);
            Py_DECREF(child_constants);
            Py_DECREF(instrs_obj);
            Py_DECREF(cname);
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }

        PyObject *lc_obj = PyObject_GetAttrString(child, "local_count");
        if (!lc_obj) {
            Py_DECREF(pc_obj);
            Py_DECREF(names_list);
            Py_DECREF(child_constants);
            Py_DECREF(instrs_obj);
            Py_DECREF(cname);
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }

        /* Fetch child._code_caches — already populated by the recursive call above. */
        PyObject *child_cc = PyObject_GetAttrString(child, "_code_caches");
        PyObject *child_cc_list = (child_cc && PyList_Check(child_cc)) ? child_cc : NULL;
        if (!child_cc) PyErr_Clear();

        Py_buffer _view;
        uint64_t *instrs_ptr = NULL;
        int code_len = 0;
        if (PyObject_GetBuffer(instrs_obj, &_view, PyBUF_SIMPLE) == 0) {
            instrs_ptr = (uint64_t *)_view.buf;
            code_len = (int)(_view.len / sizeof(uint64_t));
            PyBuffer_Release(&_view);
        } else {
            Py_XDECREF(child_cc);
            Py_DECREF(lc_obj);
            Py_DECREF(pc_obj);
            Py_DECREF(names_list);
            Py_DECREF(child_constants);
            Py_DECREF(instrs_obj);
            Py_DECREF(param_names_tup);
            Py_DECREF(cname);
            Py_DECREF(children);
            return NULL;
        }

        ClosureCache *cc = (ClosureCache *)malloc(sizeof(ClosureCache));
        if (!cc) {
            Py_XDECREF(child_cc);
            Py_DECREF(lc_obj);
            Py_DECREF(pc_obj);
            Py_DECREF(names_list);
            Py_DECREF(child_constants);
            Py_DECREF(instrs_obj);
            Py_DECREF(param_names_tup);
            Py_DECREF(cname);
            Py_DECREF(children);
            PyErr_NoMemory();
            return NULL;
        }

        /*
         * The ClosureCache owns parameters, name, and instrs_obj.
         * constants, names_list, and closure_caches are borrowed — kept alive
         * by bytecode.  bytecode itself is not owned here — kept alive by the
         * parent's code_objects list for the duration of execution.
         */
        cc->parameters = param_names_tup;
        cc->name = cname;
        cc->bytecode = child;
        cc->instrs_obj = instrs_obj;
        cc->constants = child_constants;       /* borrowed */
        cc->names_list = names_list;           /* borrowed */
        cc->closure_caches = child_cc_list;    /* borrowed */
        cc->instrs = instrs_ptr;
        cc->param_count = (int)PyLong_AsLong(pc_obj);
        cc->local_count = (int)PyLong_AsLong(lc_obj);
        cc->is_variadic = is_variadic;
        cc->ncap = ncap;
        cc->code_len = code_len;
        cc->closure_caches_items = child_cc_list
            ? ((PyListObject *)child_cc_list)->ob_item : NULL;

        /* child_cc is borrowed by cc->closure_caches; drop our GetAttrString ref. */
        Py_XDECREF(child_cc);
        Py_DECREF(lc_obj);
        Py_DECREF(pc_obj);
        /* instrs_obj, param_names_tup, cname owned by cc — do not DECREF. */
        /* constants, names_list, closure_caches borrowed — do not DECREF. */

        PyObject *capsule = PyCapsule_New(cc, CLOSURE_CACHE_CAPSULE_NAME,
                                          _closure_cache_capsule_destructor);
        if (!capsule) {
            free(cc);
            Py_DECREF(children);
            return NULL;
        }

        int ok = PyObject_SetAttrString(child, "_closure_cache", capsule);
        Py_DECREF(capsule);
        if (ok < 0) {
            Py_DECREF(children);
            return NULL;
        }
    }

    /*
     * Build _code_caches — a list of each child's _closure_cache capsule,
     * indexed by position in code_objects.  Stored on the parent so
     * frame_setup can cache it once and OP_MAKE_CLOSURE uses PyList_GET_ITEM
     * with zero PyObject_GetAttrString calls in the hot loop.
     */
    n = PyList_GET_SIZE(children);
    PyObject *code_caches = PyList_New(n);
    if (!code_caches) {
        Py_DECREF(children);
        return NULL;
    }
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *cc = PyObject_GetAttrString(PyList_GET_ITEM(children, i), "_closure_cache");
        if (!cc) {
            Py_DECREF(code_caches);
            Py_DECREF(children);
            return NULL;
        }
        PyList_SET_ITEM(code_caches, i, cc);  /* steals ref */
    }
    Py_DECREF(children);
    int cc_ok = PyObject_SetAttrString(code, "_code_caches", code_caches);
    Py_DECREF(code_caches);
    if (cc_ok < 0) return NULL;

    return code;
}

/* ---------------------------------------------------------------------------
 * Boundary describe functions — forward-declared in menai_vm_hashtable.c
 * ------------------------------------------------------------------------- */

PyObject *
menai_value_describe_none(MenaiValue val)
{
    (void)val;
    return PyUnicode_FromString("#none");
}

PyObject *
menai_value_describe_boolean(MenaiValue val)
{
    int v = ((MenaiBoolean_Object *)val)->value;
    return PyUnicode_FromString(v ? "#t" : "#f");
}

PyObject *
menai_value_describe_integer(MenaiValue val)
{
    MenaiInteger_Object *obj = (MenaiInteger_Object *)val;
    if (!obj->is_big) {
        return PyUnicode_FromFormat("%ld", obj->small);
    }
    char *s = NULL;
    if (menai_int_to_string(&obj->big, 10, &s) < 0) return NULL;
    PyObject *r = PyUnicode_FromString(s);
    PyMem_Free(s);
    return r;
}

PyObject *
menai_value_describe_float(MenaiValue val)
{
    double v = ((MenaiFloat_Object *)val)->value;
    PyObject *pf = PyFloat_FromDouble(v);
    if (!pf) return NULL;
    PyObject *r = PyObject_Str(pf);
    Py_DECREF(pf);
    return r;
}

PyObject *
menai_value_describe_complex(MenaiValue val)
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
menai_value_describe_string(MenaiValue val)
{
    /*
     * Convert to Python unicode, then escape and wrap in double quotes,
     * matching the Python-layer MenaiString.describe() output.
     */
    PyObject *pystr = menai_string_to_pyunicode(val);
    if (!pystr) return NULL;

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
                if (c2 == '"' || c2 == '\\' || c2 == '\n' ||
                    c2 == '\t' || c2 == '\r' || c2 < 32) break;
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
    if (!joined) return NULL;

    PyObject *result = PyUnicode_FromFormat("\"%U\"", joined);
    Py_DECREF(joined);
    return result;
}

PyObject *
menai_value_describe_symbol(MenaiValue val)
{
    PyObject *name = ((MenaiSymbol_Object *)val)->name;
    Py_INCREF(name);
    return name;
}

PyObject *
menai_value_describe_structtype(MenaiValue val)
{
    MenaiStructType_Object *st = (MenaiStructType_Object *)val;
    PyObject *field_names = st->field_names;
    Py_ssize_t nf = PyTuple_GET_SIZE(field_names);

    if (nf == 0) {
        return PyUnicode_FromFormat("<struct-type %U ()>", st->name);
    }

    PyObject *sep = PyUnicode_FromString(" ");
    if (!sep) return NULL;
    PyObject *fields_str = PyUnicode_Join(sep, field_names);
    Py_DECREF(sep);
    if (!fields_str) return NULL;

    PyObject *result = PyUnicode_FromFormat("<struct-type %U (%U)>",
                                            st->name, fields_str);
    Py_DECREF(fields_str);
    return result;
}

PyObject *
menai_value_describe_struct(MenaiValue val)
{
    MenaiStruct_Object *s = (MenaiStruct_Object *)val;
    MenaiStructType_Object *st = (MenaiStructType_Object *)s->struct_type;
    int nf = s->nfields;

    if (nf == 0) {
        return PyUnicode_FromFormat("(%U)", st->name);
    }

    PyObject *parts = PyList_New(nf);
    if (!parts) return NULL;

    for (int i = 0; i < nf; i++) {
        PyObject *fd = menai_value_describe(s->items[i]);
        if (!fd) {
            Py_DECREF(parts);
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
    if (!fields_str) return NULL;

    PyObject *result = PyUnicode_FromFormat("(%U %U)", st->name, fields_str);
    Py_DECREF(fields_str);
    return result;
}

PyObject *
menai_value_describe_list(MenaiValue val)
{
    MenaiList_Object *lst = (MenaiList_Object *)val;
    Py_ssize_t n = lst->length;

    if (n == 0) {
        return PyUnicode_FromString("()");
    }

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;

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
    if (!joined) return NULL;

    PyObject *result = PyUnicode_FromFormat("(%U)", joined);
    Py_DECREF(joined);
    return result;
}

PyObject *
menai_value_describe_dict(MenaiValue val)
{
    MenaiDict_Object *d = (MenaiDict_Object *)val;
    Py_ssize_t n = d->length;

    if (n == 0) {
        return PyUnicode_FromString("{}");
    }

    PyObject *pairs = PyList_New(n);
    if (!pairs) return NULL;

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
    if (!joined) return NULL;

    PyObject *result = PyUnicode_FromFormat("{%U}", joined);
    Py_DECREF(joined);
    return result;
}

PyObject *
menai_value_describe_set(MenaiValue val)
{
    MenaiSet_Object *s = (MenaiSet_Object *)val;
    Py_ssize_t n = s->length;

    if (n == 0) {
        return PyUnicode_FromString("#{}");
    }

    PyObject *parts = PyList_New(n);
    if (!parts) return NULL;

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
    if (!joined) return NULL;

    PyObject *result = PyUnicode_FromFormat("#{%U}", joined);
    Py_DECREF(joined);
    return result;
}

PyObject *
menai_value_describe_function(MenaiValue val)
{
    MenaiFunction_Object *fn = (MenaiFunction_Object *)val;
    PyObject *params = fn->parameters;
    int is_variadic = fn->is_variadic;
    Py_ssize_t np = PyTuple_GET_SIZE(params);

    PyObject *param_str;
    if (np == 0) {
        param_str = PyUnicode_FromString("");
    } else if (is_variadic && np > 0) {
        /* Last parameter is the rest parameter */
        if (np == 1) {
            /* Single rest parameter — no dot prefix, matching slow VM behaviour */
            param_str = PyTuple_GET_ITEM(params, 0);
            Py_INCREF(param_str);
        } else {
            PyObject *regular = PyList_New(np - 1);
            if (!regular) return NULL;
            for (Py_ssize_t i = 0; i < np - 1; i++) {
                PyObject *p = PyTuple_GET_ITEM(params, i);
                Py_INCREF(p);
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
            if (!reg_str) return NULL;
            param_str = PyUnicode_FromFormat("%U . %S",
                reg_str, PyTuple_GET_ITEM(params, np - 1));
            Py_DECREF(reg_str);
        }
    } else {
        PyObject *sep = PyUnicode_FromString(" ");
        if (!sep) return NULL;
        param_str = PyUnicode_Join(sep, params);
        Py_DECREF(sep);
    }

    if (!param_str) return NULL;
    PyObject *result = PyUnicode_FromFormat("<lambda (%U)>", param_str);
    Py_DECREF(param_str);
    return result;
}

/* ---------------------------------------------------------------------------
 * Boundary to_python functions — forward-declared in menai_vm_hashtable.c
 * ------------------------------------------------------------------------- */

PyObject *
menai_value_to_python_none(MenaiValue val)
{
    (void)val;
    Py_RETURN_NONE;
}

PyObject *
menai_value_to_python_boolean(MenaiValue val)
{
    int v = ((MenaiBoolean_Object *)val)->value;
    return PyBool_FromLong(v);
}

PyObject *
menai_value_to_python_integer(MenaiValue val)
{
    MenaiInteger_Object *obj = (MenaiInteger_Object *)val;
    if (!obj->is_big) {
        return PyLong_FromLong(obj->small);
    }
    return menai_int_to_pylong(&obj->big);
}

PyObject *
menai_value_to_python_float(MenaiValue val)
{
    return PyFloat_FromDouble(((MenaiFloat_Object *)val)->value);
}

PyObject *
menai_value_to_python_complex(MenaiValue val)
{
    MenaiComplex_Object *c = (MenaiComplex_Object *)val;
    return PyComplex_FromDoubles(c->real, c->imag);
}

PyObject *
menai_value_to_python_string(MenaiValue val)
{
    return menai_string_to_pyunicode(val);
}

PyObject *
menai_value_to_python_symbol(MenaiValue val)
{
    PyObject *name = ((MenaiSymbol_Object *)val)->name;
    Py_INCREF(name);
    return name;
}

PyObject *
menai_value_to_python_structtype(MenaiValue val)
{
    MenaiStructType_Object *st = (MenaiStructType_Object *)val;
    return PyUnicode_FromFormat("<struct-type %U>", st->name);
}

PyObject *
menai_value_to_python_struct(MenaiValue val)
{
    MenaiStruct_Object *s = (MenaiStruct_Object *)val;
    MenaiStructType_Object *st = (MenaiStructType_Object *)s->struct_type;
    int nf = s->nfields;
    PyObject *field_names = st->field_names;

    PyObject *result = PyDict_New();
    if (!result) return NULL;

    for (int i = 0; i < nf; i++) {
        PyObject *fname = PyTuple_GET_ITEM(field_names, i);
        PyObject *fval = menai_value_to_python(s->items[i]);
        if (!fval) {
            Py_DECREF(result);
            return NULL;
        }
        int ok = PyDict_SetItem(result, fname, fval);
        Py_DECREF(fval);
        if (ok < 0) {
            Py_DECREF(result);
            return NULL;
        }
    }

    return result;
}

PyObject *
menai_value_to_python_list(MenaiValue val)
{
    MenaiList_Object *lst = (MenaiList_Object *)val;
    Py_ssize_t n = lst->length;

    PyObject *result = PyList_New(n);
    if (!result) return NULL;

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
menai_value_to_python_dict(MenaiValue val)
{
    MenaiDict_Object *d = (MenaiDict_Object *)val;
    Py_ssize_t n = d->length;

    PyObject *result = PyDict_New();
    if (!result) return NULL;

    for (Py_ssize_t i = 0; i < n; i++) {
        MenaiValue k = d->keys[i];
        MenaiType *kt = k->ob_type;
        PyObject *py_key;

        /* Use string representation for Python dict keys, matching Python layer */
        if (kt == &MenaiString_Type) {
            py_key = menai_string_to_pyunicode(k);
        } else if (kt == &MenaiSymbol_Type) {
            py_key = ((MenaiSymbol_Object *)k)->name;
            Py_INCREF(py_key);
        } else {
            /* Non-string/symbol keys are stringified, matching slow VM behaviour */
            PyObject *native = menai_value_to_python(k);
            if (!native) { Py_DECREF(result); return NULL; }
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
menai_value_to_python_set(MenaiValue val)
{
    MenaiSet_Object *s = (MenaiSet_Object *)val;
    Py_ssize_t n = s->length;

    PyObject *result = PySet_New(NULL);
    if (!result) return NULL;

    for (Py_ssize_t i = 0; i < n; i++) {
        MenaiValue elem = s->elements[i];
        MenaiType *et = elem->ob_type;
        PyObject *py_elem;

        if (et == &MenaiString_Type) {
            py_elem = menai_string_to_pyunicode(elem);
        } else if (et == &MenaiSymbol_Type) {
            py_elem = ((MenaiSymbol_Object *)elem)->name;
            Py_INCREF(py_elem);
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
menai_value_to_python_function(MenaiValue val)
{
    /* Functions return themselves as opaque Python objects */
    Py_INCREF((PyObject *)val);
    return (PyObject *)val;
}

/* ---------------------------------------------------------------------------
 * Python-callable wrappers (exposed on the module for the C VM's shim init)
 * ------------------------------------------------------------------------- */

static PyObject *
py_convert_value(PyObject *self, PyObject *arg)
{
    (void)self;
    return menai_convert_value(arg);
}

static PyObject *
py_convert_code_constants(PyObject *self, PyObject *arg)
{
    (void)self;
    PyObject *r = menai_convert_code_constants(arg);
    if (!r) return NULL;
    Py_INCREF(r);
    return r;
}

static PyObject *
py_build_closure_caches(PyObject *self, PyObject *arg)
{
    (void)self;
    PyObject *r = menai_build_closure_caches(arg);
    if (!r) return NULL;
    Py_INCREF(r);
    return r;
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
    if (t == &MenaiNone_Type)       return PyUnicode_FromString("none");
    if (t == &MenaiBoolean_Type)    return PyUnicode_FromString("boolean");
    if (t == &MenaiInteger_Type)    return PyUnicode_FromString("integer");
    if (t == &MenaiFloat_Type)      return PyUnicode_FromString("float");
    if (t == &MenaiComplex_Type)    return PyUnicode_FromString("complex");
    if (t == &MenaiString_Type)     return PyUnicode_FromString("string");
    if (t == &MenaiSymbol_Type)     return PyUnicode_FromString("symbol");
    if (t == &MenaiList_Type)       return PyUnicode_FromString("list");
    if (t == &MenaiDict_Type)       return PyUnicode_FromString("dict");
    if (t == &MenaiSet_Type)        return PyUnicode_FromString("set");
    if (t == &MenaiFunction_Type)   return PyUnicode_FromString("function");
    if (t == &MenaiStructType_Type) return PyUnicode_FromString("struct-type");
    if (t == &MenaiStruct_Type)     return PyUnicode_FromString("struct");
    return PyUnicode_FromString(t->tp_name);
}

static PyObject *
py_describe(PyObject *self, PyObject *args)
{
    (void)args;
    return menai_value_describe((MenaiValue)self);
}

static PyObject *
py_to_python(PyObject *self, PyObject *args)
{
    (void)args;
    return menai_value_to_python((MenaiValue)self);
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
    if (!tup) return NULL;
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *pair = PyTuple_Pack(2,
            (PyObject *)d->keys[i], (PyObject *)d->values[i]);
        if (!pair) { Py_DECREF(tup); return NULL; }
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
    return menai_string_to_pyunicode((MenaiValue)self);
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
    return PyFloat_FromDouble(((MenaiFloat_Object *)self)->value);
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
    if (!obj->is_big) return PyLong_FromLong(obj->small);
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
    PyObject *p = ((MenaiFunction_Object *)self)->parameters;
    Py_INCREF(p);
    return p;
}

static PyObject *
func_get_name(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *n = ((MenaiFunction_Object *)self)->name;
    Py_INCREF(n);
    return n;
}

static PyObject *
func_get_bytecode(PyObject *self, void *closure)
{
    (void)closure;
    PyObject *b = ((MenaiFunction_Object *)self)->bytecode;
    Py_INCREF(b);
    return b;
}

static PyObject *
func_get_is_variadic(PyObject *self, void *closure)
{
    (void)closure;
    return PyBool_FromLong(((MenaiFunction_Object *)self)->is_variadic);
}

static PyObject *
func_get_param_count(PyObject *self, void *closure)
{
    (void)closure;
    return PyLong_FromLong(((MenaiFunction_Object *)self)->param_count);
}

static PyObject *
func_get_captured_values(PyObject *self, void *closure)
{
    (void)closure;
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    Py_ssize_t ncap = f->ncap;
    PyObject *lst = PyList_New(ncap);
    if (!lst) return NULL;
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
    MenaiFunction_Object *f = (MenaiFunction_Object *)self;
    Py_ssize_t ncap = f->ncap;
    if (PyList_GET_SIZE(value) != ncap) {
        PyErr_SetString(PyExc_ValueError,
            "captured_values length does not match function capture count");
        return -1;
    }
    for (Py_ssize_t i = 0; i < ncap; i++) {
        MenaiValue nv = (MenaiValue)PyList_GET_ITEM(value, i);
        menai_retain(nv);
        menai_xrelease(f->captures[i]);
        f->captures[i] = nv;
    }
    return 0;
}

static PyGetSetDef _function_getsets[] = {
    {"parameters",      func_get_parameters,      NULL,                      NULL, NULL},
    {"name",            func_get_name,             NULL,                      NULL, NULL},
    {"bytecode",        func_get_bytecode,         NULL,                      NULL, NULL},
    {"is_variadic",     func_get_is_variadic,      NULL,                      NULL, NULL},
    {"param_count",     func_get_param_count,      NULL,                      NULL, NULL},
    {"captured_values", func_get_captured_values,  func_set_captured_values,  NULL, NULL},
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
    PyObject *n = ((MenaiSymbol_Object *)self)->name;
    Py_INCREF(n);
    return n;
}

static PyGetSetDef _symbol_getsets[] = {
    {"name", symbol_get_name, NULL, NULL, NULL},
    {NULL, NULL, NULL, NULL, NULL}
};

static int
fetch_slow_type(PyObject *mod, const char *name, PyTypeObject **dst)
{
    PyObject *obj = PyObject_GetAttrString(mod, name);
    if (!obj) return -1;
    *dst = (PyTypeObject *)obj;
    return 0;
}

static PyMethodDef module_methods[] = {
    {"convert_value", py_convert_value, METH_O, NULL},
    {"convert_code_constants", py_convert_code_constants, METH_O, NULL},
    {"build_closure_caches", py_build_closure_caches, METH_O, NULL},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef module_def = {
    PyModuleDef_HEAD_INIT,
    "menai.menai_vm_value",
    NULL,
    -1,
    module_methods
};

PyObject *
_menai_vm_value_init(void)
{
    /* Fetch slow-world types — needed by menai_convert_value. */
    PyObject *slow_mod = PyImport_ImportModule("menai.menai_value");
    if (!slow_mod) return NULL;

    if (fetch_slow_type(slow_mod, "MenaiNone", &Slow_NoneType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiBoolean", &Slow_BooleanType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiInteger", &Slow_IntegerType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiFloat", &Slow_FloatType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiComplex", &Slow_ComplexType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiString", &Slow_StringType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiSymbol", &Slow_SymbolType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiList", &Slow_ListType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiDict", &Slow_DictType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiSet", &Slow_SetType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiFunction", &Slow_FunctionType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiStructType", &Slow_StructTypeType) < 0) goto fail;
    if (fetch_slow_type(slow_mod, "MenaiStruct", &Slow_StructType) < 0) goto fail;
    Py_DECREF(slow_mod);
    slow_mod = NULL;

    /* Fetch MenaiEvalError */
    PyObject *err_mod = PyImport_ImportModule("menai.menai_error");
    if (!err_mod) return NULL;
    MenaiEvalError_type = PyObject_GetAttrString(err_mod, "MenaiEvalError");
    Py_DECREF(err_mod);
    if (!MenaiEvalError_type) return NULL;

    /*
     * Patch tp_methods and tp_getset onto each type before PyType_Ready.
     * All types get the shared methods (type_name, describe, to_python).
     * Selected types also get type-specific getsets.
     */
    MenaiNone_Type.tp_methods     = _shared_methods;
    MenaiBoolean_Type.tp_methods  = _shared_methods;
    MenaiBoolean_Type.tp_getset   = _boolean_getsets;
    MenaiInteger_Type.tp_methods  = _shared_methods;
    MenaiInteger_Type.tp_getset   = _integer_getsets;
    MenaiFloat_Type.tp_methods    = _shared_methods;
    MenaiFloat_Type.tp_getset     = _float_getsets;
    MenaiComplex_Type.tp_methods  = _shared_methods;
    MenaiString_Type.tp_methods   = _shared_methods;
    MenaiString_Type.tp_getset    = _string_getsets;
    MenaiSymbol_Type.tp_methods   = _shared_methods;
    MenaiSymbol_Type.tp_getset    = _symbol_getsets;
    MenaiList_Type.tp_methods     = _shared_methods;
    MenaiDict_Type.tp_methods     = _shared_methods;
    MenaiDict_Type.tp_getset      = _dict_getsets;
    MenaiSet_Type.tp_methods      = _shared_methods;
    MenaiFunction_Type.tp_methods = _shared_methods;
    MenaiFunction_Type.tp_getset  = _function_getsets;
    MenaiStructType_Type.tp_methods = _shared_methods;
    MenaiStruct_Type.tp_methods   = _shared_methods;

    if (menai_vm_string_init(MenaiEvalError_type) < 0) return NULL;
    if (menai_vm_none_init() < 0) return NULL;
    if (menai_vm_boolean_init() < 0) return NULL;
    if (menai_vm_float_init() < 0) return NULL;
    if (menai_vm_integer_init() < 0) return NULL;
    if (menai_vm_complex_init() < 0) return NULL;
    if (menai_vm_function_init() < 0) return NULL;
    if (menai_vm_symbol_init() < 0) return NULL;
    if (menai_vm_list_init() < 0) return NULL;
    if (menai_vm_set_init() < 0) return NULL;
    if (menai_vm_struct_init() < 0) return NULL;
    if (menai_vm_dict_init() < 0) return NULL;

    /* Call PyType_Ready for types whose init functions don't do it. */
    if (PyType_Ready(&MenaiString_Type) < 0) return NULL;

    /* Create module */
    PyObject *module = PyModule_Create(&module_def);
    if (!module) return NULL;

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
    MenaiValue none_val = menai_none_singleton();
    menai_retain(none_val);
    if (PyModule_AddObject(module, "Menai_NONE", (PyObject *)none_val) < 0) {
        menai_release(none_val);
        Py_DECREF(module);
        return NULL;
    }

    MenaiValue bool_true = menai_boolean_true();
    menai_retain(bool_true);
    if (PyModule_AddObject(module, "Menai_BOOLEAN_TRUE", (PyObject *)bool_true) < 0) {
        menai_release(bool_true);
        Py_DECREF(module);
        return NULL;
    }

    MenaiValue bool_false = menai_boolean_false();
    menai_retain(bool_false);
    if (PyModule_AddObject(module, "Menai_BOOLEAN_FALSE", (PyObject *)bool_false) < 0) {
        menai_release(bool_false);
        Py_DECREF(module);
        return NULL;
    }

    struct {
        const char *name;
        MenaiValue *obj;
    } singletons[] = {
        {"Menai_LIST_EMPTY", &_Menai_EMPTY_LIST},
        {"Menai_DICT_EMPTY", &_Menai_EMPTY_DICT},
        {"Menai_SET_EMPTY", &_Menai_EMPTY_SET},
    };
    for (int i = 0; i < (int)(sizeof(singletons)/sizeof(singletons[0])); i++) {
        menai_retain(*singletons[i].obj);
        if (PyModule_AddObject(module, singletons[i].name,
                               (PyObject *)*singletons[i].obj) < 0) {
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
        {"MenaiNone",       &MenaiNone_Type},
        {"MenaiBoolean",    &MenaiBoolean_Type},
        {"MenaiInteger",    &MenaiInteger_Type},
        {"MenaiFloat",      &MenaiFloat_Type},
        {"MenaiComplex",    &MenaiComplex_Type},
        {"MenaiString",     &MenaiString_Type},
        {"MenaiSymbol",     &MenaiSymbol_Type},
        {"MenaiList",       &MenaiList_Type},
        {"MenaiDict",       &MenaiDict_Type},
        {"MenaiSet",        &MenaiSet_Type},
        {"MenaiFunction",   &MenaiFunction_Type},
        {"MenaiStructType", &MenaiStructType_Type},
        {"MenaiStruct",     &MenaiStruct_Type},
    };
    for (int i = 0; i < (int)(sizeof(types)/sizeof(types[0])); i++) {
        Py_INCREF(types[i].type);
        if (PyModule_AddObject(module, types[i].name,
                               (PyObject *)types[i].type) < 0) {
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
