# Cython declaration file for menai_value_fast.pyx.
#
# Exposes the C-level struct layout of all cdef class types so that
# menai_vm.pyx can cimport them and use typed local variables and casts
# without going through Python's attribute lookup protocol.
#
# Any .pyx file that needs direct C-level access to these types must:
#   from menai.menai_value_fast cimport MenaiValue, MenaiInteger, ...

cdef class MenaiValue:
    pass

cdef class MenaiNone(MenaiValue):
    pass

cdef class MenaiBoolean(MenaiValue):
    cdef public bint value

cdef class MenaiInteger(MenaiValue):
    cdef public object value

cdef class MenaiFloat(MenaiValue):
    cdef public double value

cdef class MenaiComplex(MenaiValue):
    cdef public object value

cdef class MenaiString(MenaiValue):
    cdef public str value

cdef class MenaiSymbol(MenaiValue):
    cdef public str name

cdef class MenaiList(MenaiValue):
    cdef public tuple elements

cdef class MenaiDict(MenaiValue):
    cdef public tuple pairs
    cdef public dict lookup

cdef class MenaiSet(MenaiValue):
    cdef public tuple elements
    cdef public frozenset members

cdef class MenaiFunction(MenaiValue):
    cdef public tuple parameters
    cdef public object name
    cdef public object bytecode
    cdef public list captured_values
    cdef public bint is_variadic
