# cython: language_level=3
# cython: boundscheck=False
# cython: wraparound=False
"""
Cython-accelerated Menai runtime value types for the VM.

These are the VM-only counterparts of the pure-Python types in menai_value.py.
The compiler pipeline continues to use menai_value.py unchanged.  At the start
of execute(), a conversion pass translates compiler-world values into these
fast types once, before the dispatch loop begins.

Each scalar type stores its payload as a C-level field:
  MenaiInteger  — cdef long long value
  MenaiFloat    — cdef double value
  MenaiBoolean  — cdef bint value
  MenaiNone     — no fields (singleton)

MenaiString, MenaiSymbol hold a Python str — the str itself is a Python heap
object, but the cdef class wrapper eliminates the dataclass overhead on every
field access.

MenaiList holds a Python tuple of MenaiValue instances.  Immutable list ops
still allocate new tuples, but the wrapper overhead is eliminated.

MenaiDict holds a tuple of pairs and a Python dict for O(1) lookup.  The
to_hashable_key() logic uses direct C-level field reads for the common cases.

MenaiFunction is mutable (captured_values is patched by PATCH_CLOSURE) and
participates in reference cycles (closures capturing closures).  It declares
__traverse__ and __clear__ so the cyclic GC can collect closure cycles.
"""

from menai.menai_error import MenaiEvalError


cdef class MenaiValue:
    """Abstract base for all VM runtime values."""

    def to_python(self):
        raise NotImplementedError

    def type_name(self):
        raise NotImplementedError

    def describe(self):
        raise NotImplementedError


cdef class MenaiNone(MenaiValue):
    """Represents the absence of a value (#none)."""

    def to_python(self):
        return None

    def type_name(self):
        return "none"

    def describe(self):
        return "#none"

    def __repr__(self):
        return "MenaiNone()"

    def __hash__(self):
        return hash(None)

    def __eq__(self, other):
        return type(other) is MenaiNone


cdef class MenaiBoolean(MenaiValue):
    """Represents boolean values (#t / #f)."""

    def __init__(self, bint value):
        self.value = value

    def to_python(self):
        return bool(self.value)

    def type_name(self):
        return "boolean"

    def describe(self):
        return "#t" if self.value else "#f"

    def __repr__(self):
        return f"MenaiBoolean({self.value!r})"

    def __hash__(self):
        return hash(bool(self.value))

    def __eq__(self, other):
        if type(other) is not MenaiBoolean:
            return False

        return self.value == (<MenaiBoolean>other).value


cdef class MenaiInteger(MenaiValue):
    """Represents integer values.  Payload stored as a Python int (arbitrary precision)."""

    def __init__(self, value):
        self.value = value

    def to_python(self):
        return self.value

    def type_name(self):
        return "integer"

    def describe(self):
        return str(self.value)

    def __repr__(self):
        return f"MenaiInteger({self.value!r})"

    def __hash__(self):
        return hash(self.value)

    def __eq__(self, other):
        if type(other) is not MenaiInteger:
            return False

        return self.value == (<MenaiInteger>other).value  # Python int comparison


cdef class MenaiFloat(MenaiValue):
    """Represents floating-point values.  Payload stored as a C double."""

    def __init__(self, double value):
        self.value = value

    def to_python(self):
        return self.value

    def type_name(self):
        return "float"

    def describe(self):
        return str(self.value)

    def __repr__(self):
        return f"MenaiFloat({self.value!r})"

    def __hash__(self):
        return hash(self.value)

    def __eq__(self, other):
        if type(other) is not MenaiFloat:
            return False

        return self.value == (<MenaiFloat>other).value


cdef class MenaiComplex(MenaiValue):
    """Represents complex number values.  Payload held as a Python complex."""

    def __init__(self, value):
        self.value = value

    def to_python(self):
        return self.value

    def type_name(self):
        return "complex"

    def describe(self):
        cdef double r, i
        r = self.value.real
        i = self.value.imag

        def _fmt(x):
            try:
                as_int = int(x)
                if x == as_int:
                    return str(as_int)

            except (ValueError, OverflowError):
                pass

            return str(x)

        if r == 0.0 and i == 0.0:
            return "0+0j"

        if r == 0.0:
            return f"{_fmt(i)}j"

        real_str = _fmt(r)
        imag_str = f"+{_fmt(i)}j" if i >= 0.0 else f"{_fmt(i)}j"
        return f"{real_str}{imag_str}"

    def __repr__(self):
        return f"MenaiComplex({self.value!r})"

    def __hash__(self):
        return hash(self.value)

    def __eq__(self, other):
        if type(other) is not MenaiComplex:
            return False

        return self.value == (<MenaiComplex>other).value


cdef class MenaiString(MenaiValue):
    """Represents string values."""

    def __init__(self, str value):
        self.value = value

    def to_python(self):
        return self.value

    def type_name(self):
        return "string"

    def describe(self):
        return '"' + _escape_string(self.value) + '"'

    def __repr__(self):
        return f"MenaiString({self.value!r})"

    def __hash__(self):
        return hash(self.value)

    def __eq__(self, other):
        if type(other) is not MenaiString:
            return False

        return self.value == (<MenaiString>other).value


cdef str _escape_string(str s):
    """Escape a string for Menai display format."""
    result = []
    for char in s:
        if char == '"':
            result.append('\\"')

        elif char == '\\':
            result.append('\\\\')

        elif char == '\n':
            result.append('\\n')

        elif char == '\t':
            result.append('\\t')

        elif char == '\r':
            result.append('\\r')

        elif ord(char) < 32:
            result.append(f'\\u{ord(char):04x}')

        else:
            result.append(char)

    return ''.join(result)


cdef class MenaiSymbol(MenaiValue):
    """Represents symbol values (produced by quote)."""

    def __init__(self, str name):
        self.name = name

    def to_python(self):
        return self.name

    def type_name(self):
        return "symbol"

    def describe(self):
        return self.name

    def __str__(self):
        return self.name

    def __repr__(self):
        return f"MenaiSymbol({self.name!r})"

    def __hash__(self):
        return hash(self.name)

    def __eq__(self, other):
        if type(other) is not MenaiSymbol:
            return False

        return self.name == (<MenaiSymbol>other).name


cdef class MenaiList(MenaiValue):
    """Represents immutable lists.  Elements stored in a Python tuple."""

    def __init__(self, elements=()):
        self.elements = tuple(elements)

    def to_python(self):
        return [elem.to_python() for elem in self.elements]

    def type_name(self):
        return "list"

    def describe(self):
        if len(self.elements) == 0:
            return "()"

        return "(" + " ".join(e.describe() for e in self.elements) + ")"

    def __repr__(self):
        return f"MenaiList({self.elements!r})"

    def __hash__(self):
        return hash(self.elements)

    def __eq__(self, other):
        if type(other) is not MenaiList:
            return False

        return self.elements == (<MenaiList>other).elements


cdef class MenaiDict(MenaiValue):
    """
    Represents immutable key-value dictionaries.

    pairs  — tuple of (key, value) pairs; preserves insertion order.
    lookup — Python dict mapping hashable keys to (key, value) pairs; O(1) access.
    """

    def __init__(self, pairs=()):
        self.pairs = tuple(pairs)
        cdef dict lk = {}
        for key, value in self.pairs:
            lk[_hashable_key(key)] = (key, value)

        self.lookup = lk

    def to_python(self):
        result = {}
        for key, value in self.pairs:
            if type(key) is MenaiString:
                py_key = (<MenaiString>key).value

            elif type(key) is MenaiSymbol:
                py_key = (<MenaiSymbol>key).name

            else:
                py_key = str(key.to_python())

            result[py_key] = value.to_python()

        return result

    def type_name(self):
        return "dict"

    def describe(self):
        if len(self.pairs) == 0:
            return "{}"

        parts = [f"({k.describe()} {v.describe()})" for k, v in self.pairs]
        return "{" + " ".join(parts) + "}"

    def __repr__(self):
        return f"MenaiDict({self.pairs!r})"

    def __hash__(self):
        return hash(self.pairs)

    def __eq__(self, other):
        if type(other) is not MenaiDict:
            return False

        return self.pairs == (<MenaiDict>other).pairs

    @staticmethod
    def to_hashable_key(key):
        """Convert a MenaiValue key to a hashable Python value."""
        return _hashable_key(key)


cdef class MenaiSet(MenaiValue):
    """
    Represents sets - immutable unordered collections of unique hashable values.

    Internally uses a frozenset of hashable keys for O(1) membership testing
    while maintaining insertion order in a tuple for deterministic iteration
    and display.  Duplicate elements are silently dropped on construction.
    Valid element types are the same as dict keys: strings, numbers, booleans,
    and symbols.
    """

    def __init__(self, elements = ()):
        seen: set = set()
        deduped = []
        for elem in elements:
            hk = MenaiDict.to_hashable_key(elem)
            if hk not in seen:
                seen.add(hk)
                deduped.append(elem)

        self.elements: Tuple['MenaiValue', ...] = tuple(deduped)
        self.members: frozenset = frozenset(
            MenaiDict.to_hashable_key(e) for e in self.elements
        )

    def __eq__(self, other) -> bool:
        if not isinstance(other, MenaiSet):
            return False

        return self.members == other.members

    def __hash__(self) -> int:
        return hash(self.members)

    def to_python(self) -> set:
        """Convert to Python set."""
        result = set()
        for elem in self.elements:
            if isinstance(elem, MenaiString):
                result.add(elem.value)

            elif isinstance(elem, MenaiSymbol):
                result.add(elem.name)

            else:
                result.add(elem.to_python())

        return result

    def type_name(self) -> str:
        return "set"

    def describe(self) -> str:
        if not self.elements:
            return "#{}"

        return "#{" + " ".join(e.describe() for e in self.elements) + "}"


cdef object _hashable_key(object key):
    """
    Convert a MenaiValue key to a hashable Python value for dict lookup.

    Uses direct C-level field reads for the common scalar types.
    """
    cdef type t = type(key)
    if t is MenaiString:
        return ('str', (<MenaiString>key).value)

    if t is MenaiInteger:
        return ('int', (<MenaiInteger>key).value)

    if t is MenaiFloat:
        return ('flt', (<MenaiFloat>key).value)

    if t is MenaiComplex:
        return ('cplx', (<MenaiComplex>key).value)

    if t is MenaiBoolean:
        return ('bool', (<MenaiBoolean>key).value)

    if t is MenaiSymbol:
        return ('sym', (<MenaiSymbol>key).name)

    raise MenaiEvalError(
        message="Dict keys must be strings, numbers, booleans, or symbols",
        received=f"Key type: {key.type_name()}",
        example='(dict ("name" "Alice") ("age" 30))',
        suggestion="Use strings for most keys"
    )


cdef class MenaiFunction(MenaiValue):
    """
    Represents a first-class function (lambda or builtin).

    captured_values is a plain Python list so that PATCH_CLOSURE can write
    into it after the closure is created (letrec mutual-recursion wiring).

    Declares __traverse__ and __clear__ so the cyclic GC can collect closure
    cycles (closures that capture other closures that capture them back).
    """

    def __init__(self, parameters=(), name=None, bytecode=None, captured_values=None, is_variadic=False):
        self.parameters = tuple(parameters)
        self.name = name
        self.bytecode = bytecode
        self.captured_values = captured_values if captured_values is not None else []
        self.is_variadic = is_variadic

    def to_python(self):
        return self

    def type_name(self):
        return "function"

    def describe(self):
        if self.is_variadic and len(self.parameters) > 0:
            n = len(self.parameters)
            regular = ', '.join(self.parameters[:n - 1]) if n > 1 else ''
            rest = self.parameters[n - 1]
            param_str = f"{regular} . {rest}".strip(' .')

        else:
            param_str = ', '.join(self.parameters)

        return f"<lambda ({param_str})>"

    def __repr__(self):
        return f"MenaiFunction({self.parameters!r})"

    def __hash__(self):
        return id(self)

    def __eq__(self, other):
        return self is other

    def __traverse__(self, visit, arg):
        """Tell the cyclic GC about Python object references held by this closure."""
        if self.bytecode is not None:
            visit(self.bytecode, arg)

        if self.captured_values is not None:
            visit(self.captured_values, arg)

        return 0

    def __clear__(self):
        """Break reference cycles when the GC collects this closure."""
        self.bytecode = None
        self.captured_values = []


# Module-level singletons
Menai_NONE = MenaiNone()
Menai_BOOLEAN_TRUE = MenaiBoolean(True)
Menai_BOOLEAN_FALSE = MenaiBoolean(False)
Menai_LIST_EMPTY = MenaiList(())
Menai_DICT_EMPTY = MenaiDict(())
Menai_SET_EMPTY = MenaiSet(())


def convert_value(src):
    """
    Convert a single compiler-world MenaiValue to its fast VM equivalent.

    Called once per constant at the start of execute() to translate the
    CodeObject.constants lists from plain Python values to cdef class values.
    """
    # Already a fast type — pass through unchanged.
    if isinstance(src, MenaiValue):
        return src

    # Import here to avoid circular imports at module load time.
    import menai.menai_value as _slow
    t = type(src)
    if t is _slow.MenaiInteger:
        return MenaiInteger(src.value)

    if t is _slow.MenaiFloat:
        return MenaiFloat(src.value)

    if t is _slow.MenaiBoolean:
        return Menai_BOOLEAN_TRUE if src.value else Menai_BOOLEAN_FALSE

    if t is _slow.MenaiNone:
        return Menai_NONE

    if t is _slow.MenaiString:
        return MenaiString(src.value)

    if t is _slow.MenaiSymbol:
        return MenaiSymbol(src.name)

    if t is _slow.MenaiComplex:
        return MenaiComplex(src.value)

    if t is _slow.MenaiList:
        return MenaiList(tuple(convert_value(e) for e in src.elements))

    if t is _slow.MenaiDict:
        return MenaiDict(tuple(
            (convert_value(k), convert_value(v)) for k, v in src.pairs
        ))

    if t is _slow.MenaiSet:
        return MenaiSet(tuple(convert_value(e) for e in src.elements))

    if t is _slow.MenaiFunction:
        # Zero-capture lambdas are stored as LOAD_CONST constants by the
        # bytecode builder.  Convert to a fast MenaiFunction; the nested
        # CodeObject's constants are converted by convert_code_object below.
        return MenaiFunction(
            parameters=src.parameters,
            name=src.name,
            bytecode=src.bytecode,
            captured_values=list(src.captured_values),
            is_variadic=src.is_variadic,
        )

    raise TypeError(f"convert_value: unexpected type {t!r}")


def convert_code_object(code):
    """Recursively convert all constants in a CodeObject tree to fast VM types.

    Walks the full code_objects tree so nested lambdas are also converted.
    Returns the same CodeObject with its constants lists replaced in-place.
    This is safe because CodeObject instances are not shared across executions.
    """
    code.constants = [convert_value(v) for v in code.constants]
    for child in code.code_objects:
        convert_code_object(child)

    return code


def to_slow(src):
    """Convert a single fast VM MenaiValue to its slow compiler-world equivalent.

    Called on the return value of execute() so that no fast types escape the VM
    boundary.  All code outside menai_vm.pyx sees only menai_value.py types.

    If src is already a slow menai_value.py type (e.g. a prelude MenaiFunction
    that was stored in globals and never converted to a fast type), it is
    returned unchanged.
    """
    import menai.menai_value as _slow
    if isinstance(src, _slow.MenaiValue):
        return src

    if type(src) is MenaiNone:
        return _slow.MenaiNone()

    if type(src) is MenaiBoolean:
        return _slow.MenaiBoolean(bool(src.value))

    if type(src) is MenaiInteger:
        return _slow.MenaiInteger(src.value)

    if type(src) is MenaiFloat:
        return _slow.MenaiFloat(src.value)

    if type(src) is MenaiComplex:
        return _slow.MenaiComplex(src.value)

    if type(src) is MenaiString:
        return _slow.MenaiString(src.value)

    if type(src) is MenaiSymbol:
        return _slow.MenaiSymbol(src.name)

    if type(src) is MenaiList:
        return _slow.MenaiList(tuple(to_slow(e) for e in src.elements))

    if type(src) is MenaiDict:
        return _slow.MenaiDict(tuple(
            (to_slow(k), to_slow(v)) for k, v in src.pairs
        ))

    if type(src) is MenaiSet:
        return _slow.MenaiSet(tuple(to_slow(e) for e in src.elements))

    if type(src) is MenaiFunction:
        return _slow.MenaiFunction(
            parameters=src.parameters,
            name=src.name,
            bytecode=src.bytecode,
            captured_values=list(src.captured_values),
            is_variadic=src.is_variadic,
        )
    raise TypeError(f"to_slow: unexpected fast type {type(src)!r}")
