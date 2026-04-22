/*
 * menai_vm_code.c — MenaiCodeObject implementation.
 *
 * Converts Python CodeObject trees to native C MenaiCodeObject trees and
 * manages their lifetimes via reference counting.
 */

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdlib.h>
#include <string.h>

#include "menai_vm_code.h"
#include "menai_vm_object.h"

/*
 * Forward declaration — menai_convert_value lives in menai_vm_value.c and is
 * linked into the same shared library.
 */
extern MenaiValue menai_convert_value(PyObject *src);

void
menai_code_object_release(MenaiCodeObject *co)
{
    if (co == NULL) return;
    if (--co->ob_refcnt > 0) return;

    for (Py_ssize_t i = 0; i < co->nconst; i++)
        menai_release(co->constants[i]);
    free(co->constants);

    for (Py_ssize_t i = 0; i < co->nnames; i++)
        free((char *)co->names[i]);
    free(co->names);

    for (Py_ssize_t i = 0; i < co->nparam_names; i++)
        free(co->param_names[i]);
    free(co->param_names);

    for (Py_ssize_t i = 0; i < co->nchildren; i++)
        menai_code_object_release(co->children[i]);
    free(co->children);

    free(co->instrs);
    free(co->name);
    free(co);
}

int
menai_code_object_max_locals(const MenaiCodeObject *co)
{
    int best = co->local_count + co->outgoing_arg_slots;
    for (Py_ssize_t i = 0; i < co->nchildren; i++) {
        int child_best = menai_code_object_max_locals(co->children[i]);
        if (child_best > best) best = child_best;
    }
    return best;
}

/*
 * _read_int — read a named integer attribute from a Python object.
 */
static int
_read_int(PyObject *obj, const char *attr, int *out)
{
    PyObject *v = PyObject_GetAttrString(obj, attr);
    if (!v) return -1;
    long val = PyLong_AsLong(v);
    Py_DECREF(v);
    if (val == -1 && PyErr_Occurred()) return -1;
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
    if (!v) return -1;
    int r = PyObject_IsTrue(v);
    Py_DECREF(v);
    if (r < 0) return -1;
    *out = r;
    return 0;
}

MenaiCodeObject *
menai_code_object_from_python(PyObject *py_code)
{
    MenaiCodeObject *co = (MenaiCodeObject *)calloc(1, sizeof(MenaiCodeObject));
    if (!co) {
        PyErr_NoMemory();
        return NULL;
    }
    co->ob_refcnt = 1;

    /* Scalar fields */
    if (_read_int(py_code, "param_count", &co->param_count) < 0) goto fail;
    if (_read_int(py_code, "local_count", &co->local_count) < 0) goto fail;
    if (_read_int(py_code, "outgoing_arg_slots", &co->outgoing_arg_slots) < 0) goto fail;
    if (_read_bool(py_code, "is_variadic", &co->is_variadic) < 0) goto fail;

    /* name — optional, used only for error messages */
    {
        PyObject *py_name = PyObject_GetAttrString(py_code, "name");
        if (py_name) {
            if (py_name != Py_None) {
                const char *s = PyUnicode_AsUTF8(py_name);
                if (s) co->name = strdup(s);
            }
            Py_DECREF(py_name);
        } else {
            PyErr_Clear();
        }
    }

    /* ncap — length of free_vars list */
    {
        PyObject *fv = PyObject_GetAttrString(py_code, "free_vars");
        if (!fv) goto fail;
        co->ncap = PyList_GET_SIZE(fv);
        Py_DECREF(fv);
    }

    /* param_names — strdup each parameter name string */
    {
        PyObject *py_pnames = PyObject_GetAttrString(py_code, "param_names");
        if (!py_pnames) goto fail;
        co->nparam_names = PyList_GET_SIZE(py_pnames);
        if (co->nparam_names > 0) {
            co->param_names = (char **)calloc((size_t)co->nparam_names, sizeof(char *));
            if (!co->param_names) {
                Py_DECREF(py_pnames);
                PyErr_NoMemory();
                goto fail;
            }
            for (Py_ssize_t i = 0; i < co->nparam_names; i++) {
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
        if (!instrs_obj) goto fail;
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
        if (!py_names) goto fail;
        co->nnames = PyList_GET_SIZE(py_names);
        if (co->nnames > 0) {
            co->names = (const char **)calloc((size_t)co->nnames, sizeof(char *));
            if (!co->names) {
                Py_DECREF(py_names);
                PyErr_NoMemory();
                goto fail;
            }
            for (Py_ssize_t i = 0; i < co->nnames; i++) {
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

    /*
     * children — recurse first so that when we convert constants that are
     * functions, their children already exist and can be referenced.
     */
    {
        PyObject *py_children = PyObject_GetAttrString(py_code, "code_objects");
        if (!py_children) goto fail;
        co->nchildren = PyList_GET_SIZE(py_children);
        if (co->nchildren > 0) {
            co->children = (MenaiCodeObject **)calloc(
                (size_t)co->nchildren, sizeof(MenaiCodeObject *));
            if (!co->children) {
                Py_DECREF(py_children);
                PyErr_NoMemory();
                goto fail;
            }
            for (Py_ssize_t i = 0; i < co->nchildren; i++) {
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
     * constants — convert each slow Python value to a fast MenaiValue.
     * Function constants whose bytecode is a child code object will have
     * their _closure_cache looked up from bc._closure_cache, which is set
     * by menai_build_closure_caches.  Since we now own the children as
     * MenaiCodeObject *, we store a pointer to the child directly on the
     * function's bytecode field after conversion.
     *
     * For now: convert each constant using menai_convert_value, which for
     * function constants reads bc._closure_cache (still set on the Python
     * CodeObject by menai_build_closure_caches).  In the next step
     * menai_build_closure_caches will be replaced entirely.
     */
    {
        PyObject *py_constants = PyObject_GetAttrString(py_code, "constants");
        if (!py_constants) goto fail;
        co->nconst = PyList_GET_SIZE(py_constants);
        if (co->nconst > 0) {
            co->constants = (MenaiValue *)calloc(
                (size_t)co->nconst, sizeof(MenaiValue));
            if (!co->constants) {
                Py_DECREF(py_constants);
                PyErr_NoMemory();
                goto fail;
            }
            for (Py_ssize_t i = 0; i < co->nconst; i++) {
                PyObject *orig = PyList_GET_ITEM(py_constants, i);
                MenaiValue fast = menai_convert_value(orig);
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
