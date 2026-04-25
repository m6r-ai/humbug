/*
 * menai_vm_value.c
 */
#include <stdlib.h>
#include <stdint.h>

#include "menai_vm_c.h"

/*
 * menai_short_type_name — return the short lowercase Menai type name for
 * use in error messages (e.g. "string", "integer", "dict").
 */
const char *
menai_short_type_name(MenaiType t)
{
    switch (t) {
    case MENAITYPE_NONE:
        return "none";

    case MENAITYPE_BOOLEAN:
        return "boolean";

    case MENAITYPE_INTEGER:
        return "integer";

    case MENAITYPE_FLOAT:
        return "float";

    case MENAITYPE_COMPLEX:
        return "complex";

    case MENAITYPE_STRING:
        return "string";

    case MENAITYPE_SYMBOL:
        return "symbol";

    case MENAITYPE_LIST:
        return "list";

    case MENAITYPE_DICT:
        return "dict";

    case MENAITYPE_SET:
        return "set";

    case MENAITYPE_FUNCTION:
        return "function";

    case MENAITYPE_STRUCTTYPE:
        return "struct-type";

    case MENAITYPE_STRUCT:
        return "struct";
    }

    assert(0);
    return "";
}

void menai_value_dealloc(MenaiValue *v)
{
    switch (v->ob_type) {
    case MENAITYPE_NONE:
        menai_none_dealloc(v);
        break;

    case MENAITYPE_BOOLEAN:
        menai_boolean_dealloc(v);
        break;

    case MENAITYPE_INTEGER:
        menai_integer_dealloc(v);
        break;

    case MENAITYPE_FLOAT:
        menai_float_dealloc(v);
        break;

    case MENAITYPE_COMPLEX:
        menai_complex_dealloc(v);
        break;

    case MENAITYPE_STRING:
        menai_string_dealloc(v);
        break;

    case MENAITYPE_SYMBOL:
        menai_symbol_dealloc(v);
        break;

    case MENAITYPE_LIST:
        menai_list_dealloc(v);
        break;

    case MENAITYPE_DICT:
        menai_dict_dealloc(v);
        break;

    case MENAITYPE_SET:
        menai_set_dealloc(v);
        break;

    case MENAITYPE_FUNCTION:
        menai_function_dealloc(v);
        break;

    case MENAITYPE_STRUCTTYPE:
        menai_struct_type_dealloc(v);
        break;

    case MENAITYPE_STRUCT:
        menai_struct_dealloc(v);
        break;

    default:
        assert(0);
    }
}