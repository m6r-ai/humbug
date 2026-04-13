/*
 * menai_vm_value.c — native C implementation of all Menai runtime value types.
 *
 * Replaces menai_value_fast.pyx.  Defines the same types (MenaiNone,
 * MenaiBoolean, MenaiInteger, MenaiFloat, MenaiComplex, MenaiString,
 * MenaiSymbol, MenaiList, MenaiDict, MenaiSet, MenaiFunction,
 * MenaiStructType, MenaiStruct) as Python extension types with known C
 * struct layouts, allowing the C VM to access fields by direct cast.
 *
 * Also provides:
 *   menai_convert_value()       — slow menai_value.py -> fast C type
 *   menai_convert_code_object() — walk CodeObject tree, convert constants
 *   menai_to_slow()             — fast C type -> slow menai_value.py
 *
 * Module name: menai.menai_vm_value
 * Exported singletons: Menai_NONE, Menai_BOOLEAN_TRUE, Menai_BOOLEAN_FALSE,
 *                      Menai_LIST_EMPTY, Menai_DICT_EMPTY, Menai_SET_EMPTY
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stddef.h>
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

/* ---------------------------------------------------------------------------
 * Forward declarations of type objects
 * ------------------------------------------------------------------------- */


/* ---------------------------------------------------------------------------
 * Module-level singletons
 * ------------------------------------------------------------------------- */

static PyObject *_Menai_EMPTY_LIST = NULL;
static PyObject *_Menai_EMPTY_DICT = NULL;
static PyObject *_Menai_EMPTY_SET = NULL;

/* ---------------------------------------------------------------------------
 * Slow-world type objects — fetched once at module init
 * ------------------------------------------------------------------------- */

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
static PyObject *MenaiEvalError_type = NULL;

/* ---------------------------------------------------------------------------
 * menai_hashable_key — convert a MenaiValue key to a hashable Python tuple.
 *
 * Shared by MenaiDict, MenaiSet, and the C VM.  Lives here because it is a
 * cross-cutting value-system utility, not specific to any one collection type.
 * ------------------------------------------------------------------------- */

PyObject *
menai_hashable_key(PyObject *key)
{
    PyTypeObject *t = Py_TYPE(key);
    if (t == &MenaiString_Type) {
        PyObject *pystr = menai_string_to_pyunicode(key);
        if (!pystr) return NULL;
        PyObject *r = Py_BuildValue("(sO)", "str", pystr);
        Py_DECREF(pystr);
        return r;
    }
    if (t == &MenaiInteger_Type) return Py_BuildValue("(sO)", "int", ((MenaiInteger_Object *)key)->value);
    if (t == &MenaiFloat_Type) {
        PyObject *pf = PyFloat_FromDouble(((MenaiFloat_Object *)key)->value);
        if (!pf) return NULL;
        PyObject *r = Py_BuildValue("(sO)", "flt", pf);
        Py_DECREF(pf);
        return r;
    }
    if (t == &MenaiComplex_Type) {
        PyObject *pc = PyComplex_FromDoubles(((MenaiComplex_Object *)key)->real,
                                             ((MenaiComplex_Object *)key)->imag);
        if (!pc) return NULL;
        PyObject *r = Py_BuildValue("(sO)", "cplx", pc);
        Py_DECREF(pc);
        return r;
    }
    if (t == &MenaiBoolean_Type) {
        PyObject *bv = PyBool_FromLong(((MenaiBoolean_Object *)key)->value);
        PyObject *r = Py_BuildValue("(sO)", "bool", bv);
        Py_DECREF(bv);
        return r;
    }
    if (t == &MenaiSymbol_Type) return Py_BuildValue("(sO)", "sym", ((MenaiSymbol_Object *)key)->name);
    if (t == &MenaiStruct_Type) {
        Py_hash_t h = PyObject_Hash(key);
        if (h == -1 && PyErr_Occurred()) {
            /* Re-raise as MenaiEvalError */
            PyObject *exc = PyErr_GetRaisedException();
            PyObject *msg = PyObject_Str(exc);
            Py_XDECREF(exc);
            if (msg) {
                PyErr_SetObject(MenaiEvalError_type, msg);
                Py_DECREF(msg);
            }
            return NULL;
        }
        PyObject *hobj = PyLong_FromSsize_t((Py_ssize_t)h);
        PyObject *r = Py_BuildValue("(sO)", "struct", hobj);
        Py_DECREF(hobj);
        return r;
    }

    PyObject *tn = PyObject_CallMethod(key, "type_name", NULL);
    PyErr_Format(MenaiEvalError_type,
        "Dict keys must be strings, numbers, booleans, or symbols, got %s",
        tn ? PyUnicode_AsUTF8(tn) : "?");

    Py_XDECREF(tn);
    return NULL;
}

/* ===========================================================================
 * Conversion functions
 * =========================================================================*/

/*
 * _is_fast — return 1 if obj is already a fast C value type.
 */
static int
_is_fast(PyObject *obj)
{
    PyTypeObject *t = Py_TYPE(obj);
    return (t == &MenaiNone_Type     || t == &MenaiBoolean_Type  ||
            t == &MenaiInteger_Type  || t == &MenaiFloat_Type    ||
            t == &MenaiComplex_Type  || t == &MenaiString_Type   ||
            t == &MenaiSymbol_Type   || t == &MenaiList_Type     ||
            t == &MenaiDict_Type     || t == &MenaiSet_Type      ||
            t == &MenaiFunction_Type || t == &MenaiStructType_Type ||
            t == &MenaiStruct_Type);
}

/*
 * menai_convert_value — convert one slow menai_value.py object to a fast type.
 *
 * Returns a new reference.  If src is already a fast type, returns it
 * with an incremented refcount.  For MenaiFunction, captured_values are
 * NOT recursively converted here — call_setup in the VM does that lazily
 * at call time to avoid cycles in letrec closures.
 */
PyObject *
menai_convert_value(PyObject *src)
{
    if (_is_fast(src)) {
        Py_INCREF(src);
        return src;
    }

    PyTypeObject *t = Py_TYPE(src);

    if (t == Slow_NoneType)
        return (Py_INCREF(menai_none_singleton()), menai_none_singleton());

    if (t == Slow_BooleanType) {
        PyObject *bv = PyObject_GetAttrString(src, "value");
        if (!bv) return NULL;
        int b = PyObject_IsTrue(bv);
        Py_DECREF(bv);
        if (b < 0) return NULL;
        PyObject *r = b ? menai_boolean_true() : menai_boolean_false();
        Py_INCREF(r);
        return r;
    }

    if (t == Slow_IntegerType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
        if (!PyLong_Check(v)) { Py_DECREF(v); PyErr_SetString(PyExc_TypeError, "MenaiInteger requires an int"); return NULL; }
        MenaiInteger_Object *r = (MenaiInteger_Object *)MenaiInteger_Type.tp_alloc(&MenaiInteger_Type, 0);
        if (r) { r->value = v; } else { Py_DECREF(v); }
        return (PyObject *)r;
    }

    if (t == Slow_FloatType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
        double d = PyFloat_AsDouble(v);
        Py_DECREF(v);
        if (d == -1.0 && PyErr_Occurred()) return NULL;
        MenaiFloat_Object *r = (MenaiFloat_Object *)MenaiFloat_Type.tp_alloc(&MenaiFloat_Type, 0);
        if (r) r->value = d;
        return (PyObject *)r;
    }

    if (t == Slow_ComplexType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
        MenaiComplex_Object *r = (MenaiComplex_Object *)MenaiComplex_Type.tp_alloc(&MenaiComplex_Type, 0);
        if (r) {
            r->real = PyComplex_RealAsDouble(v);
            r->imag = PyComplex_ImagAsDouble(v);
        }
        Py_DECREF(v);
        return (PyObject *)r;
    }

    if (t == Slow_StringType) {
        PyObject *v = PyObject_GetAttrString(src, "value");
        if (!v) return NULL;
        PyObject *r = menai_string_from_pyunicode(v);
        Py_DECREF(v);
        return r;
    }

    if (t == Slow_SymbolType) {
        PyObject *n = PyObject_GetAttrString(src, "name");
        if (!n) return NULL;
        MenaiSymbol_Object *r = (MenaiSymbol_Object *)MenaiSymbol_Type.tp_alloc(&MenaiSymbol_Type, 0);
        if (r) { r->name = n; } else { Py_DECREF(n); }
        return (PyObject *)r;
    }

    if (t == Slow_ListType) {
        PyObject *elems = PyObject_GetAttrString(src, "elements");
        if (!elems) return NULL;
        Py_ssize_t n = PyTuple_GET_SIZE(elems);
        PyObject **arr = n > 0 ? (PyObject **)PyMem_Malloc(n * sizeof(PyObject *)) : NULL;
        if (n > 0 && !arr) { Py_DECREF(elems); PyErr_NoMemory(); return NULL; }
        for (Py_ssize_t i = 0; i < n; i++) {
            arr[i] = menai_convert_value(PyTuple_GET_ITEM(elems, i));
            if (!arr[i]) {
                for (Py_ssize_t j = 0; j < i; j++) Py_DECREF(arr[j]);
                PyMem_Free(arr);
                Py_DECREF(elems);
                return NULL;
            }
        }
        Py_DECREF(elems);
        return menai_list_from_array_steal(arr, n);
    }

    if (t == Slow_DictType) {
        PyObject *pairs = PyObject_GetAttrString(src, "pairs");
        if (!pairs) return NULL;
        Py_ssize_t n = PyTuple_GET_SIZE(pairs);
        PyObject *fast_pairs = PyTuple_New(n);
        if (!fast_pairs) {
            Py_DECREF(pairs);
            return NULL;
        }
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *pair = PyTuple_GET_ITEM(pairs, i);
            PyObject *fk = menai_convert_value(PyTuple_GET_ITEM(pair, 0));
            if (!fk) {
                Py_DECREF(fast_pairs);
                Py_DECREF(pairs);
                return NULL;
            }
            PyObject *fv = menai_convert_value(PyTuple_GET_ITEM(pair, 1));
            if (!fv) {
                Py_DECREF(fk);
                Py_DECREF(fast_pairs);
                Py_DECREF(pairs);
                return NULL;
            }
            PyObject *fp = PyTuple_Pack(2, fk, fv);
            Py_DECREF(fk);
            Py_DECREF(fv);
            if (!fp) {
                Py_DECREF(fast_pairs);
                Py_DECREF(pairs);
                return NULL;
            }
            PyTuple_SET_ITEM(fast_pairs, i, fp);
        }
        Py_DECREF(pairs);
        return menai_dict_from_fast_pairs(fast_pairs);
    }

    if (t == Slow_SetType) {
        PyObject *elems = PyObject_GetAttrString(src, "elements");
        if (!elems) return NULL;
        Py_ssize_t n = PyTuple_GET_SIZE(elems);
        PyObject *fast_tup = PyTuple_New(n);
        if (!fast_tup) {
            Py_DECREF(elems);
            return NULL;
        }
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *fe = menai_convert_value(PyTuple_GET_ITEM(elems, i));
            if (!fe) {
                Py_DECREF(fast_tup);
                Py_DECREF(elems);
                return NULL;
            }
            PyTuple_SET_ITEM(fast_tup, i, fe);
        }
        Py_DECREF(elems);
        return menai_set_from_fast_tuple(fast_tup);
    }

    if (t == Slow_StructTypeType) {
        PyObject *name = PyObject_GetAttrString(src, "name");
        PyObject *tag  = PyObject_GetAttrString(src, "tag");
        PyObject *fn   = PyObject_GetAttrString(src, "field_names");
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
        return menai_struct_type_new_from_args(args);
    }

    if (t == Slow_StructType) {
        PyObject *st     = PyObject_GetAttrString(src, "struct_type");
        PyObject *fields = PyObject_GetAttrString(src, "fields");
        if (!st || !fields) {
            Py_XDECREF(st);
            Py_XDECREF(fields);
            return NULL;
        }
        PyObject *fast_st = menai_convert_value(st);
        Py_DECREF(st);
        if (!fast_st) {
            Py_DECREF(fields);
            return NULL;
        }
        Py_ssize_t n = PyTuple_GET_SIZE(fields);
        PyObject *fast_fields = PyTuple_New(n);
        if (!fast_fields) {
            Py_DECREF(fast_st);
            Py_DECREF(fields);
            return NULL;
        }
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *ff = menai_convert_value(PyTuple_GET_ITEM(fields, i));
            if (!ff) {
                Py_DECREF(fast_fields);
                Py_DECREF(fast_st);
                Py_DECREF(fields);
                return NULL;
            }
            PyTuple_SET_ITEM(fast_fields, i, ff);
        }
        Py_DECREF(fields);
        return menai_struct_new_from_fast(fast_st, fast_fields);
    }

    if (t == Slow_FunctionType) {
        PyObject *params  = PyObject_GetAttrString(src, "parameters");
        PyObject *name    = PyObject_GetAttrString(src, "name");
        PyObject *bc      = PyObject_GetAttrString(src, "bytecode");
        PyObject *cap     = PyObject_GetAttrString(src, "captured_values");
        PyObject *is_var  = PyObject_GetAttrString(src, "is_variadic");
        if (!params || !name || !bc || !cap || !is_var) {
            Py_XDECREF(params);
            Py_XDECREF(name);
            Py_XDECREF(bc);
            Py_XDECREF(cap);
            Py_XDECREF(is_var);
            return NULL;
        }
        /* Recursively convert captured_values to fast types.
         * Prelude closures are fully-formed (no letrec None placeholders),
         * so eager conversion is safe and eliminates the slow-type check
         * in call_setup's hot path. */
        Py_ssize_t ncap = PyList_GET_SIZE(cap);
        PyObject *cap_list = PyList_New(ncap);
        if (!cap_list) {
            Py_DECREF(cap);
            Py_DECREF(params);
            Py_DECREF(name);
            Py_DECREF(bc);
            Py_DECREF(is_var);
            return NULL;
        }
        for (Py_ssize_t ci = 0; ci < ncap; ci++) {
            PyObject *fast_cv = menai_convert_value(PyList_GET_ITEM(cap, ci));
            if (!fast_cv) {
                for (Py_ssize_t cj = 0; cj < ci; cj++)
                    Py_DECREF(PyList_GET_ITEM(cap_list, cj));
                Py_DECREF(cap);
                Py_DECREF(cap_list);
                Py_DECREF(params);
                Py_DECREF(name);
                Py_DECREF(bc);
                Py_DECREF(is_var);
                return NULL;
            }
            PyList_SET_ITEM(cap_list, ci, fast_cv);
        }
        Py_DECREF(cap);
        int iv = PyObject_IsTrue(is_var);
        Py_DECREF(is_var);
        if (iv < 0) {
            Py_DECREF(params);
            Py_DECREF(name);
            Py_DECREF(bc);
            Py_DECREF(cap_list);
            return NULL;
        }
        PyObject *kwargs = Py_BuildValue("{sOsOsOsOsi}",
            "parameters",      params,
            "name",            name,
            "bytecode",        bc,
            "captured_values", cap_list,
            "is_variadic",     iv);
        Py_DECREF(params);
        Py_DECREF(name);
        Py_DECREF(bc);
        Py_DECREF(cap_list);
        if (!kwargs) return NULL;
        PyObject *empty = PyTuple_New(0);
        if (!empty) {
            Py_DECREF(kwargs);
            return NULL;
        }
        PyObject *r = menai_function_new_from_kwargs(empty, kwargs);
        Py_DECREF(empty);
        Py_DECREF(kwargs);
        return r;
    }

    PyErr_Format(PyExc_TypeError, "menai_convert_value: unexpected type %R", (PyObject *)t);
    return NULL;
}

/*
 * menai_convert_code_object — walk a CodeObject tree, converting all
 * constants lists in-place from slow to fast types.
 *
 * Returns the code object (borrowed reference), or NULL on error.
 */
PyObject *
menai_convert_code_object(PyObject *code)
{
    /* Guard against processing the same CodeObject twice.  This happens when
     * a named function's CodeObject appears both as a MenaiFunction constant
     * and as a direct child in code_objects.  The second call would rebuild
     * _code_caches, freeing the list that MenaiFunction_Object.closure_caches
     * already borrowed a pointer to, causing a use-after-free crash. */
    PyObject *_existing = PyObject_GetAttrString(code, "_code_caches");
    int _already_done = (_existing && PyList_Check(_existing));
    Py_XDECREF(_existing);
    PyErr_Clear();
    if (_already_done) return code;

    /* Convert code.constants list in-place.  For function constants, recurse
     * into their bytecode first so that _code_caches is populated before
     * MenaiFunction_new reads it — ensuring closure_caches is set correctly
     * on the resulting MenaiFunction_Object from the very start. */
    PyObject *constants = PyObject_GetAttrString(code, "constants");
    if (!constants) return NULL;
    Py_ssize_t n = PyList_GET_SIZE(constants);
    for (Py_ssize_t i = 0; i < n; i++) {
        PyObject *orig = PyList_GET_ITEM(constants, i);
        /* If this constant is a function (slow or already-fast), recurse into
         * its bytecode before converting it so _code_caches is ready. */
        PyObject *bc = PyObject_GetAttrString(orig, "bytecode");
        if (bc == NULL) {
            PyErr_Clear();  /* not a function — no bytecode attribute */
        } else if (bc != Py_None) {
            if (!menai_convert_code_object(bc)) {
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
        PyObject *child = PyList_GET_ITEM(children, i);
        if (!menai_convert_code_object(child)) {
            Py_DECREF(children);
            return NULL;
        }

        /* Build _closure_cache = (param_names_tuple, name, is_variadic_int,
         * ncap_int, instrs_obj, constants, names, param_count_int,
         * local_count_int, child_code) on this child so OP_MAKE_CLOSURE and
         * menai_function_alloc need zero PyObject_GetAttrString calls.
         * child_code at [9] lets OP_MAKE_CLOSURE pass bytecode to menai_function_alloc
         * without fetching code_objects from the frame's code object. */
        PyObject *param_names = PyObject_GetAttrString(child, "param_names");
        if (!param_names) {
            Py_DECREF(children);
            return NULL;
        }

        /* param_names is a list; convert to tuple for O(1) indexing */
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

        PyObject *constants = PyObject_GetAttrString(child, "constants");
        if (!constants) {
            Py_DECREF(instrs_obj);
            Py_DECREF(cname);
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }

        PyObject *names_list = PyObject_GetAttrString(child, "names");
        if (!names_list) {
            Py_DECREF(constants);
            Py_DECREF(instrs_obj);
            Py_DECREF(cname);
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }

        PyObject *pc_obj = PyObject_GetAttrString(child, "param_count");
        if (!pc_obj) {
            Py_DECREF(names_list);
            Py_DECREF(constants);
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
            Py_DECREF(constants);
            Py_DECREF(instrs_obj);
            Py_DECREF(cname);
            Py_DECREF(param_names_tup);
            Py_DECREF(children);
            return NULL;
        }

        /* Fetch child._code_caches — already populated by the recursive call above. */
        PyObject *child_cc = PyObject_GetAttrString(child, "_code_caches");
        PyObject *child_cc_or_none = (child_cc && PyList_Check(child_cc)) ? child_cc : Py_None;
        if (!child_cc) PyErr_Clear();

        /* Pre-extract the raw instruction pointer and length from instrs_obj so
         * menai_function_alloc needs zero PyObject_GetBuffer calls per closure. */
        Py_buffer _view;
        PyObject *instrs_ptr_obj = NULL;
        PyObject *code_len_obj = NULL;
        if (PyObject_GetBuffer(instrs_obj, &_view, PyBUF_SIMPLE) == 0) {
            instrs_ptr_obj = PyLong_FromVoidPtr(_view.buf);
            code_len_obj   = PyLong_FromSsize_t(_view.len / (Py_ssize_t)sizeof(uint64_t));
            PyBuffer_Release(&_view);
        }
        if (!instrs_ptr_obj || !code_len_obj) {
            Py_XDECREF(instrs_ptr_obj);
            Py_XDECREF(code_len_obj);
            Py_XDECREF(child_cc);
            Py_DECREF(lc_obj); Py_DECREF(pc_obj);
            Py_DECREF(names_list); Py_DECREF(constants); Py_DECREF(instrs_obj);
            Py_DECREF(param_names_tup);
            Py_DECREF(cname);
            Py_DECREF(children);
            return NULL;
        }

        PyObject *cache = Py_BuildValue("(OOiiOOOOOOOOO)", param_names_tup, cname,
                                        is_variadic, (int)ncap,
                                        instrs_obj, constants, names_list,
                                        pc_obj, lc_obj, child,
                                        child_cc_or_none,
                                        instrs_ptr_obj, code_len_obj);
        Py_DECREF(code_len_obj);
        Py_DECREF(instrs_ptr_obj);
        Py_XDECREF(child_cc);  /* drop owned ref — bytecode keeps child._code_caches alive */
        Py_DECREF(lc_obj); Py_DECREF(pc_obj);
        Py_DECREF(names_list); Py_DECREF(constants); Py_DECREF(instrs_obj);
        Py_DECREF(param_names_tup);
        Py_DECREF(cname);
        if (!cache) {
            Py_DECREF(children);
            return NULL;
        }

        int ok = PyObject_SetAttrString(child, "_closure_cache", cache);
        Py_DECREF(cache);
        if (ok < 0) {
            Py_DECREF(children);
            return NULL;
        }
    }

    /* Build _code_caches — a list of each child's _closure_cache tuple,
     * indexed by position in code_objects.  Stored on the parent so
     * frame_setup can cache it once and OP_MAKE_CLOSURE uses PyList_GET_ITEM
     * with zero PyObject_GetAttrString calls in the hot loop. */
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

#define GET_SLOW_CLS(name) PyObject_GetAttrString(mod, name)

/*
 * _to_slow_memo — cycle-safe implementation of menai_to_slow.
 */
static PyObject *
_to_slow_memo(PyObject *src, PyObject *memo)
{
    /* Already slow — pass through */
    if (!_is_fast(src)) {
        Py_INCREF(src);
        return src;
    }

    PyObject *key = PyLong_FromVoidPtr(src);
    if (!key) return NULL;
    PyObject *cached = PyDict_GetItem(memo, key);
    if (cached) {
        Py_DECREF(key);
        Py_INCREF(cached);
        return cached;
    }

    PyTypeObject *t = Py_TYPE(src);
    PyObject *mod = PyImport_ImportModule("menai.menai_value");
    if (!mod) {
        Py_DECREF(key);
        return NULL;
    }

    PyObject *result = NULL;

    if (t == &MenaiNone_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiNone");
        if (cls) {
            result = PyObject_CallNoArgs(cls);
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiBoolean_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiBoolean");
        if (cls) {
            PyObject *bv = PyBool_FromLong(((MenaiBoolean_Object *)src)->value);
            result = PyObject_CallOneArg(cls, bv);
            Py_DECREF(bv);
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiInteger_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiInteger");
        if (cls) {
            result = PyObject_CallOneArg(cls, ((MenaiInteger_Object *)src)->value);
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiFloat_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiFloat");
        if (cls) {
            PyObject *pf = PyFloat_FromDouble(((MenaiFloat_Object *)src)->value);
            result = pf ? PyObject_CallOneArg(cls, pf) : NULL;
            Py_XDECREF(pf);
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiComplex_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiComplex");
        if (cls) {
            PyObject *pc = PyComplex_FromDoubles(((MenaiComplex_Object *)src)->real,
                                                 ((MenaiComplex_Object *)src)->imag);
            if (pc) {
                result = PyObject_CallOneArg(cls, pc);
                Py_DECREF(pc);
            }
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiString_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiString");
        if (cls) {
            PyObject *pystr = menai_string_to_pyunicode(src);
            if (pystr) {
                result = PyObject_CallOneArg(cls, pystr);
                Py_DECREF(pystr);
            }
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiSymbol_Type) {
        PyObject *cls = GET_SLOW_CLS("MenaiSymbol");
        if (cls) {
            result = PyObject_CallOneArg(cls, ((MenaiSymbol_Object *)src)->name);
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiList_Type) {
        /* Register placeholder before recursing */
        if (PyDict_SetItem(memo, key, Py_None) < 0) goto done;
        MenaiList_Object *lst = (MenaiList_Object *)src;
        Py_ssize_t n = lst->length;
        PyObject *slow_tup = PyTuple_New(n);
        if (!slow_tup) goto done;
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *se = _to_slow_memo(lst->elements[i], memo);
            if (!se) {
                Py_DECREF(slow_tup);
                goto done;
            }
            PyTuple_SET_ITEM(slow_tup, i, se);
        }
        PyObject *cls = GET_SLOW_CLS("MenaiList");
        if (cls) {
            result = PyObject_CallOneArg(cls, slow_tup);
            Py_DECREF(cls);
        }
        Py_DECREF(slow_tup);
        if (result) PyDict_SetItem(memo, key, result);
    }
    else if (t == &MenaiDict_Type) {
        if (PyDict_SetItem(memo, key, Py_None) < 0) goto done;
        PyObject *pairs = ((MenaiDict_Object *)src)->pairs;
        Py_ssize_t n = PyTuple_GET_SIZE(pairs);
        PyObject *slow_pairs = PyTuple_New(n);
        if (!slow_pairs) goto done;
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *pair = PyTuple_GET_ITEM(pairs, i);
            PyObject *sk = _to_slow_memo(PyTuple_GET_ITEM(pair, 0), memo);
            if (!sk) {
                Py_DECREF(slow_pairs);
                goto done;
            }
            PyObject *sv = _to_slow_memo(PyTuple_GET_ITEM(pair, 1), memo);
            if (!sv) {
                Py_DECREF(sk);
                Py_DECREF(slow_pairs);
                goto done;
            }
            PyObject *sp = PyTuple_Pack(2, sk, sv);
            Py_DECREF(sk);
            Py_DECREF(sv);
            if (!sp) {
                Py_DECREF(slow_pairs);
                goto done;
            }
            PyTuple_SET_ITEM(slow_pairs, i, sp);
        }
        PyObject *cls = GET_SLOW_CLS("MenaiDict");
        if (cls) {
            result = PyObject_CallOneArg(cls, slow_pairs);
            Py_DECREF(cls);
        }
        Py_DECREF(slow_pairs);
        if (result) PyDict_SetItem(memo, key, result);
    }
    else if (t == &MenaiSet_Type) {
        if (PyDict_SetItem(memo, key, Py_None) < 0) goto done;
        PyObject *elems = ((MenaiSet_Object *)src)->elements;
        Py_ssize_t n = PyTuple_GET_SIZE(elems);
        PyObject *slow_tup = PyTuple_New(n);
        if (!slow_tup) goto done;
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *se = _to_slow_memo(PyTuple_GET_ITEM(elems, i), memo);
            if (!se) {
                Py_DECREF(slow_tup);
                goto done;
            }
            PyTuple_SET_ITEM(slow_tup, i, se);
        }
        PyObject *cls = GET_SLOW_CLS("MenaiSet");
        if (cls) {
            result = PyObject_CallOneArg(cls, slow_tup);
            Py_DECREF(cls);
        }
        Py_DECREF(slow_tup);
        if (result) PyDict_SetItem(memo, key, result);
    }
    else if (t == &MenaiStructType_Type) {
        MenaiStructType_Object *st = (MenaiStructType_Object *)src;
        PyObject *cls = GET_SLOW_CLS("MenaiStructType");
        if (cls) {
            PyObject *tag = PyLong_FromLong(st->tag);
            result = tag ? PyObject_CallFunctionObjArgs(cls, st->name, tag, st->field_names, NULL) : NULL;
            Py_XDECREF(tag);
            Py_DECREF(cls);
        }
    }
    else if (t == &MenaiStruct_Type) {
        if (PyDict_SetItem(memo, key, Py_None) < 0) goto done;
        MenaiStruct_Object *s = (MenaiStruct_Object *)src;
        PyObject *slow_st = _to_slow_memo(s->struct_type, memo);
        if (!slow_st) goto done;
        Py_ssize_t n = PyTuple_GET_SIZE(s->fields);
        PyObject *slow_fields = PyTuple_New(n);
        if (!slow_fields) {
            Py_DECREF(slow_st);
            goto done;
        }
        for (Py_ssize_t i = 0; i < n; i++) {
            PyObject *sf = _to_slow_memo(PyTuple_GET_ITEM(s->fields, i), memo);
            if (!sf) {
                Py_DECREF(slow_fields);
                Py_DECREF(slow_st);
                goto done;
            }
            PyTuple_SET_ITEM(slow_fields, i, sf);
        }
        PyObject *cls = GET_SLOW_CLS("MenaiStruct");
        if (cls) {
            PyObject *kwargs = Py_BuildValue("{sOsO}", "struct_type", slow_st, "fields", slow_fields);
            if (kwargs) {
                PyObject *empty = PyTuple_New(0);
                if (empty) {
                    result = PyObject_Call(cls, empty, kwargs);
                    Py_DECREF(empty);
                }
                Py_DECREF(kwargs);
            }
            Py_DECREF(cls);
        }
        Py_DECREF(slow_st);
        Py_DECREF(slow_fields);
        if (result) PyDict_SetItem(memo, key, result);
    }
    else if (t == &MenaiFunction_Type) {
        MenaiFunction_Object *f = (MenaiFunction_Object *)src;
        PyObject *cls = GET_SLOW_CLS("MenaiFunction");
        if (cls) {
            /* Two-phase: create with empty captures, register, then fill */
            PyObject *empty_list = PyList_New(0);
            if (empty_list) {
                PyObject *kwargs = Py_BuildValue("{sOsOsOsOsi}",
                    "parameters", f->parameters,
                    "name", f->name,
                    "bytecode", f->bytecode,
                    "captured_values", empty_list,
                    "is_variadic", f->is_variadic);
                Py_DECREF(empty_list);
                if (kwargs) {
                    PyObject *empty_args = PyTuple_New(0);
                    if (empty_args) {
                        result = PyObject_Call(cls, empty_args, kwargs);
                        Py_DECREF(empty_args);
                    }
                    Py_DECREF(kwargs);
                }
            }
            Py_DECREF(cls);
        }
        if (result) {
            if (PyDict_SetItem(memo, key, result) < 0) {
                Py_DECREF(result);
                result = NULL;
                goto done;
            }
            /* Now fill captured_values */
            Py_ssize_t n = PyList_GET_SIZE(f->captured_values);
            PyObject *slow_caps = PyList_New(n);
            if (!slow_caps) {
                Py_DECREF(result);
                result = NULL;
                goto done;
            }
            for (Py_ssize_t i = 0; i < n; i++) {
                PyObject *sc = _to_slow_memo(PyList_GET_ITEM(f->captured_values, i), memo);
                if (!sc) {
                    Py_DECREF(slow_caps);
                    Py_DECREF(result);
                    result = NULL;
                    goto done;
                }
                PyList_SET_ITEM(slow_caps, i, sc);
            }
            if (PyObject_SetAttrString(result, "captured_values", slow_caps) < 0) {
                Py_DECREF(slow_caps);
                Py_DECREF(result);
                result = NULL;
                goto done;
            }
            Py_DECREF(slow_caps);
        }
    }

done:
    Py_DECREF(mod);
    Py_DECREF(key);
    return result;

#undef GET_SLOW_CLS
}

PyObject *
menai_to_slow(PyObject *src)
{
    PyObject *memo = PyDict_New();
    if (!memo) return NULL;
    PyObject *result = _to_slow_memo(src, memo);
    Py_DECREF(memo);
    return result;
}

/* ===========================================================================
 * Python-callable wrappers (exposed on the module for the C VM's shim init)
 * =========================================================================*/

static PyObject *
py_convert_value(PyObject *self, PyObject *arg)
{
    (void)self;
    return menai_convert_value(arg);
}

static PyObject *
py_convert_code_object(PyObject *self, PyObject *arg)
{
    (void)self;
    PyObject *r = menai_convert_code_object(arg);
    if (!r) return NULL;
    Py_INCREF(r);
    return r;
}

static PyObject *
py_to_slow(PyObject *self, PyObject *arg)
{
    (void)self;
    return menai_to_slow(arg);
}

/* ===========================================================================
 * Module init
 * =========================================================================*/

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
    {"convert_code_object", py_convert_code_object, METH_O, NULL},
    {"to_slow", py_to_slow, METH_O, NULL},
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
    /* Fetch slow-world types */
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

    if (menai_vm_string_init(MenaiEvalError_type) < 0)
        return NULL;

    if (menai_vm_none_init() < 0)
        return NULL;

    if (menai_vm_boolean_init() < 0)
        return NULL;

    if (menai_vm_float_init() < 0)
        return NULL;

    if (menai_vm_integer_init() < 0)
        return NULL;

    if (menai_vm_complex_init() < 0)
        return NULL;

    if (menai_vm_function_init() < 0)
        return NULL;

    if (menai_vm_symbol_init() < 0)
        return NULL;

    if (menai_vm_list_init() < 0)
        return NULL;

    if (menai_vm_set_init() < 0)
        return NULL;

    if (menai_vm_struct_init() < 0)
        return NULL;

    if (menai_vm_dict_init() < 0)
        return NULL;

    /* Ready all types */
    PyTypeObject *types[] = {
        &MenaiString_Type,  /* extern from menai_vm_string.c */
    };
    for (int i = 0; i < (int)(sizeof(types)/sizeof(types[0])); i++) {
        if (PyType_Ready(types[i]) < 0) return NULL;
    }

    /* Create module */
    PyObject *module = PyModule_Create(&module_def);
    if (!module) return NULL;

    /* Add MenaiNone type — readied by menai_vm_none_init() */
    Py_INCREF(&MenaiNone_Type);
    if (PyModule_AddObject(module, "MenaiNone", (PyObject *)&MenaiNone_Type) < 0) {
        Py_DECREF(&MenaiNone_Type);
        Py_DECREF(module);
        return NULL;
    }

    /* Add MenaiBoolean type — readied by menai_vm_boolean_init() */
    Py_INCREF(&MenaiBoolean_Type);
    if (PyModule_AddObject(module, "MenaiBoolean", (PyObject *)&MenaiBoolean_Type) < 0) {
        Py_DECREF(&MenaiBoolean_Type);
        Py_DECREF(module);
        return NULL;
    }

    /* Add MenaiFloat type — readied by menai_vm_float_init() */
    Py_INCREF(&MenaiFloat_Type);
    if (PyModule_AddObject(module, "MenaiFloat", (PyObject *)&MenaiFloat_Type) < 0) {
        Py_DECREF(&MenaiFloat_Type);
        Py_DECREF(module);
        return NULL;
    }

    /* Add MenaiInteger type — readied by menai_vm_integer_init() */
    Py_INCREF(&MenaiInteger_Type);
    if (PyModule_AddObject(module, "MenaiInteger", (PyObject *)&MenaiInteger_Type) < 0) {
        Py_DECREF(&MenaiInteger_Type);
        Py_DECREF(module);
        return NULL;
    }

    /* Add MenaiComplex type — readied by menai_vm_complex_init() */
    Py_INCREF(&MenaiComplex_Type);
    if (PyModule_AddObject(module, "MenaiComplex", (PyObject *)&MenaiComplex_Type) < 0) {
        Py_DECREF(&MenaiComplex_Type);
        Py_DECREF(module);
        return NULL;
    }

    /* Add MenaiFunction type — readied by menai_vm_function_init() */
    Py_INCREF(&MenaiFunction_Type);
    if (PyModule_AddObject(module, "MenaiFunction", (PyObject *)&MenaiFunction_Type) < 0) {
        Py_DECREF(&MenaiFunction_Type);
        Py_DECREF(module);
        return NULL;
    }

    /* Add MenaiSymbol type — readied by menai_vm_symbol_init() */
    Py_INCREF(&MenaiSymbol_Type);
    if (PyModule_AddObject(module, "MenaiSymbol", (PyObject *)&MenaiSymbol_Type) < 0) {
        Py_DECREF(&MenaiSymbol_Type);
        Py_DECREF(module);
        return NULL;
    }

    /* Add MenaiList type — readied by menai_vm_list_init() */
    Py_INCREF(&MenaiList_Type);
    if (PyModule_AddObject(module, "MenaiList", (PyObject *)&MenaiList_Type) < 0) {
        Py_DECREF(&MenaiList_Type);
        Py_DECREF(module);
        return NULL;
    }

    /* Add MenaiSet type — readied by menai_vm_set_init() */
    Py_INCREF(&MenaiSet_Type);
    if (PyModule_AddObject(module, "MenaiSet", (PyObject *)&MenaiSet_Type) < 0) {
        Py_DECREF(&MenaiSet_Type);
        Py_DECREF(module);
        return NULL;
    }

    /* Add MenaiStructType type — readied by menai_vm_struct_init() */
    Py_INCREF(&MenaiStructType_Type);
    if (PyModule_AddObject(module, "MenaiStructType", (PyObject *)&MenaiStructType_Type) < 0) {
        Py_DECREF(&MenaiStructType_Type);
        Py_DECREF(module);
        return NULL;
    }

    /* Add MenaiStruct type — readied by menai_vm_struct_init() */
    Py_INCREF(&MenaiStruct_Type);
    if (PyModule_AddObject(module, "MenaiStruct", (PyObject *)&MenaiStruct_Type) < 0) {
        Py_DECREF(&MenaiStruct_Type);
        Py_DECREF(module);
        return NULL;
    }

    /* Add types */
    const char *type_names[] = {
        "MenaiString",
    };

    /* Add MenaiDict type — readied by menai_vm_dict_init() */
    Py_INCREF(&MenaiDict_Type);
    if (PyModule_AddObject(module, "MenaiDict", (PyObject *)&MenaiDict_Type) < 0) {
        Py_DECREF(&MenaiDict_Type);
        Py_DECREF(module);
        return NULL;
    }

    for (int i = 0; i < (int)(sizeof(types)/sizeof(types[0])); i++) {
        Py_INCREF(types[i]);
        if (PyModule_AddObject(module, type_names[i], (PyObject *)types[i]) < 0) {
            Py_DECREF(types[i]);
            Py_DECREF(module);
            return NULL;
        }
    }

    /* Create singletons */
    _Menai_EMPTY_LIST = menai_list_new_empty();
    if (!_Menai_EMPTY_LIST) {
        Py_DECREF(module);
        return NULL;
    }
    _Menai_EMPTY_DICT = menai_dict_new_empty();
    _Menai_EMPTY_SET = menai_set_new_empty();
    if (!_Menai_EMPTY_DICT || !_Menai_EMPTY_SET) {
        Py_DECREF(module);
        return NULL;
    }

    /* Add singletons to module */
    PyObject *none_singleton = menai_none_singleton();
    Py_INCREF(none_singleton);
    if (PyModule_AddObject(module, "Menai_NONE", none_singleton) < 0) {
        Py_DECREF(none_singleton);
        Py_DECREF(module);
        return NULL;
    }

    PyObject *bool_true = menai_boolean_true();
    Py_INCREF(bool_true);
    if (PyModule_AddObject(module, "Menai_BOOLEAN_TRUE", bool_true) < 0) {
        Py_DECREF(bool_true);
        Py_DECREF(module);
        return NULL;
    }
    PyObject *bool_false = menai_boolean_false();
    Py_INCREF(bool_false);
    if (PyModule_AddObject(module, "Menai_BOOLEAN_FALSE", bool_false) < 0) {
        Py_DECREF(bool_false);
        Py_DECREF(module);
        return NULL;
    }

    struct {
        const char *name;
        PyObject **obj;
    } singletons[] = {
        {"Menai_LIST_EMPTY", &_Menai_EMPTY_LIST},
        {"Menai_DICT_EMPTY", &_Menai_EMPTY_DICT},
        {"Menai_SET_EMPTY", &_Menai_EMPTY_SET},
    };
    for (int i = 0; i < (int)(sizeof(singletons)/sizeof(singletons[0])); i++) {
        Py_INCREF(*singletons[i].obj);
        if (PyModule_AddObject(module, singletons[i].name, *singletons[i].obj) < 0) {
            Py_DECREF(*singletons[i].obj);
            Py_DECREF(module); return NULL;
        }
    }

    return module;

fail:
    Py_XDECREF(slow_mod);
    return NULL;
}
