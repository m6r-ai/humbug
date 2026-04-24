/*
 * menai_vm_value.c
 */
#include <stdlib.h>
#include <stdint.h>
#include "menai_vm_value.h"

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
