"""Bytecode definitions for AIFPL virtual machine."""

from dataclasses import dataclass, field
from enum import IntEnum, auto
from typing import Dict, List, Tuple

from aifpl.aifpl_value import AIFPLValue


class Opcode(IntEnum):
    """Bytecode operation codes.

    Using IntEnum for fast dispatch and clear debugging.
    """

    # Constants
    LOAD_CONST = auto()      # Load constant from constant pool: LOAD_CONST const_index
    LOAD_TRUE = auto()       # Push True
    LOAD_FALSE = auto()      # Push False
    LOAD_EMPTY_LIST = auto() # Push empty list

    # Variables (lexically addressed)
    LOAD_VAR = auto()        # Load variable by position: LOAD_VAR index
    STORE_VAR = auto()       # Store variable by position: STORE_VAR index
    LOAD_PARENT_VAR = auto() # Load variable from parent frame: LOAD_PARENT_VAR index depth
    LOAD_NAME = auto()       # Load by name lookup: LOAD_NAME name_index

    # Control flow
    JUMP = auto()            # Unconditional jump: JUMP offset
    JUMP_IF_FALSE = auto()   # Conditional jump if false
    JUMP_IF_TRUE = auto()    # Conditional jump if true
    RAISE_ERROR = auto()     # Raise error with message from constant pool: RAISE_ERROR const_index

    # Functions
    MAKE_CLOSURE = auto()    # Create closure: MAKE_CLOSURE code_index capture_count
    CALL = auto()            # Call function: CALL arity
    TAIL_CALL = auto()       # Tail call function: TAIL_CALL arity
    APPLY = auto()           # Apply function to arg list (non-tail): (apply f args)
    TAIL_APPLY = auto()      # Apply function to arg list (tail position)
    ENTER = auto()           # Pop N args from stack into locals 0..N-1: ENTER n
    RETURN = auto()          # Return from function

    # Debugging
    EMIT_TRACE = auto()      # Emit trace: EMIT_TRACE (pops value, emits to watcher)

    # Function operations
    FUNCTION_P = auto()      # Check if function
    FUNCTION_EQ_P = auto()   # (function=? f g) → boolean
    FUNCTION_NEQ_P = auto()  # (function!=? f g) → boolean
    FUNCTION_MIN_ARITY = auto()
                             # (function-min-arity f) → integer
    FUNCTION_VARIADIC_P = auto()
                             # (function-variadic? f) → boolean
    FUNCTION_ACCEPTS_P = auto()
                             # (function-accepts? f n) → boolean

    # Boolean operations
    BOOLEAN_P = auto()       # Check if boolean
    BOOLEAN_EQ_P = auto()    # boolean=? a b
    BOOLEAN_NEQ_P = auto()   # boolean!=? a b
    BOOLEAN_NOT = auto()     # Logical NOT

    # Integer operations
    INTEGER_P = auto()       # Check if integer
    INTEGER_EQ_P = auto()    # integer=? a b
    INTEGER_NEQ_P = auto()   # integer!=? a b
    INTEGER_LT_P = auto()    # integer<? a b
    INTEGER_GT_P = auto()    # integer>? a b
    INTEGER_LTE_P = auto()   # integer<=? a b
    INTEGER_GTE_P = auto()   # integer>=? a b
    INTEGER_ADD = auto()     # integer+ a b
    INTEGER_SUB = auto()     # integer- a b
    INTEGER_MUL = auto()     # integer* a b
    INTEGER_DIV = auto()     # integer/ a b  (floor division)
    INTEGER_MOD = auto()     # integer% a b  (modulo)
    INTEGER_NEG = auto()     # integer-neg x  (unary minus)
    INTEGER_ABS = auto()     # integer-abs x
    INTEGER_BIT_NOT = auto() # Bitwise NOT ~x
    INTEGER_BIT_SHIFT_LEFT = auto()
                             # Bitwise left shift x << n
    INTEGER_BIT_SHIFT_RIGHT = auto()
                             # Bitwise right shift x >> n
    INTEGER_BIT_OR = auto()  # Bitwise OR: a | b
    INTEGER_BIT_AND = auto() # Bitwise AND: a & b
    INTEGER_BIT_XOR = auto() # Bitwise XOR: a ^ b
    INTEGER_MIN = auto()     # integer-min a b
    INTEGER_MAX = auto()     # integer-max a b
    INTEGER_TO_STRING = auto()
                             # Convert integer to string
    INTEGER_TO_STRING_BIN = auto()
                             # Convert integer to binary string
    INTEGER_TO_STRING_HEX = auto()
                             # Convert integer to hex string
    INTEGER_TO_STRING_OCT = auto()
                             # Convert integer to octal string
    INTEGER_TO_FLOAT = auto() # Convert integer to float

    # Floating point operations
    FLOAT_P = auto()         # Check if float
    FLOAT_EQ_P = auto()      # float=? a b
    FLOAT_NEQ_P = auto()     # float!=? a b
    FLOAT_LT_P = auto()      # float<? a b
    FLOAT_GT_P = auto()      # float>? a b
    FLOAT_LTE_P = auto()     # float<=? a b
    FLOAT_GTE_P = auto()     # float>=? a b
    FLOAT_ADD = auto()       # float+ a b
    FLOAT_SUB = auto()       # float- a b
    FLOAT_MUL = auto()       # float* a b
    FLOAT_DIV = auto()       # float/ a b
    FLOAT_FLOOR_DIV = auto() # float// a b  (floor division)
    FLOAT_MOD = auto()       # float% a b  (modulo)
    FLOAT_NEG = auto()       # float-neg x  (unary minus)
    FLOAT_EXPT = auto()      # float-expt a b
    FLOAT_SIN = auto()       # float-sin x
    FLOAT_COS = auto()       # float-cos x
    FLOAT_TAN = auto()       # float-tan x
    FLOAT_LOG = auto()       # float-log x
    FLOAT_LOG10 = auto()     # float-log10 x
    FLOAT_EXP = auto()       # float-exp x
    FLOAT_SQRT = auto()      # float-sqrt x
    FLOAT_ABS = auto()       # float-abs x
    FLOAT_TO_INTEGER = auto()
                             # Convert float to integer
    FLOAT_TO_STRING = auto() # Convert float to string
    FLOAT_FLOOR = auto()     # float-floor x  (returns float)
    FLOAT_CEIL = auto()      # float-ceil x   (returns float)
    FLOAT_ROUND = auto()     # float-round x  (returns float)
    FLOAT_MIN = auto()       # float-min a b
    FLOAT_MAX = auto()       # float-max a b

    # Complex operations
    COMPLEX = auto()         # Construct complex from real and imaginary parts
    COMPLEX_P = auto()       # Check if complex
    COMPLEX_EQ_P = auto()    # complex=? a b
    COMPLEX_NEQ_P = auto()   # complex!=? a b
    COMPLEX_ADD = auto()     # complex+ a b
    COMPLEX_SUB = auto()     # complex- a b
    COMPLEX_MUL = auto()     # complex* a b
    COMPLEX_DIV = auto()     # complex/ a b
    COMPLEX_NEG = auto()     # complex-neg x  (unary minus)
    COMPLEX_REAL = auto()    # Extract real part
    COMPLEX_IMAG = auto()    # Extract imaginary part
    COMPLEX_EXPT = auto()    # complex-expt a b
    COMPLEX_SIN = auto()     # complex-sin x
    COMPLEX_COS = auto()     # complex-cos x
    COMPLEX_TAN = auto()     # complex-tan x
    COMPLEX_LOG = auto()     # complex-log x
    COMPLEX_LOG10 = auto()   # complex-log10 x
    COMPLEX_EXP = auto()     # complex-exp x
    COMPLEX_SQRT = auto()    # complex-sqrt x
    COMPLEX_ABS = auto()     # complex-abs x  (returns float: magnitude)
    COMPLEX_TO_STRING = auto()
                             # Convert complex to string

    # String operations
    STRING_P = auto()        # Check if string
    STRING_EQ_P = auto()     # string=? a b
    STRING_NEQ_P = auto()    # string!=? a b
    STRING_LT_P = auto()     # string<? a b  (lexicographic)
    STRING_GT_P = auto()     # string>? a b  (lexicographic)
    STRING_LTE_P = auto()    # string<=? a b (lexicographic)
    STRING_GTE_P = auto()    # string>=? a b (lexicographic)
    STRING_LENGTH = auto()   # Get length of string
    STRING_UPCASE = auto()   # Convert string to uppercase
    STRING_DOWNCASE = auto() # Convert string to lowercase
    STRING_TRIM = auto()     # Trim whitespace from string
    STRING_TO_NUMBER = auto()
                             # Parse string to number
    STRING_TO_LIST = auto()  # Split string by delimiter: (string->list str delim)
    STRING_REF = auto()      # Get character at index
    STRING_CONTAINS_P = auto()
                             # Check if string contains substring
    STRING_PREFIX_P = auto() # Check if string has prefix
    STRING_SUFFIX_P = auto() # Check if string has suffix
    STRING_CONCAT = auto()   # Concatenate two strings: (string-concat a b)
    STRING_SLICE = auto()    # Extract substring (string, start, end)
    STRING_REPLACE = auto()  # Replace substring (string, old, new)

    # Alist operations
    ALIST = auto()           # Create an alist
    ALIST_P = auto()         # Check if alist
    ALIST_EQ_P = auto()      # alist=? a b
    ALIST_NEQ_P = auto()     # alist!=? a b
    ALIST_KEYS = auto()      # Get all keys from alist
    ALIST_VALUES = auto()    # Get all values from alist
    ALIST_LENGTH = auto()    # Get number of entries in alist
    ALIST_HAS_P = auto()     # Check if alist has key
    ALIST_REMOVE = auto()    # Remove key from alist
    ALIST_MERGE = auto()     # Merge two alists
    ALIST_SET = auto()       # Set key in alist (alist, key, value)
    ALIST_GET = auto()       # Get value from alist by key with default: (alist-get alist key default)

    # List operations
    LIST = auto()            # Build list
    LIST_P = auto()          # Check if list
    LIST_EQ_P = auto()       # list=? a b
    LIST_NEQ_P = auto()      # list!=? a b
    LIST_PREPEND = auto()
    LIST_APPEND = auto()     # Append single item to end of list: (list-append lst item)
    LIST_REVERSE = auto()    # Reverse list on top of stack
    LIST_FIRST = auto()      # Get first element of list
    LIST_REST = auto()       # Get rest of list (all but first element)
    LIST_LAST = auto()       # Get last element of list
    LIST_LENGTH = auto()     # Get length of list
    LIST_REF = auto()        # Get element at index from list: LIST_REF index
    LIST_NULL_P = auto()     # Check if list is empty
    LIST_MEMBER_P = auto()   # Check if item is in list
    LIST_POSITION = auto()   # Find index of item in list
    LIST_SLICE = auto()      # Slice list: (list-slice lst start [end])
    LIST_REMOVE = auto()     # Remove all occurrences of item from list
    LIST_CONCAT = auto()     # Append two lists: (append a b)
    LIST_TO_STRING = auto()  # Join list of strings with separator: (list->string lst sep)

    # Generate integer range list
    RANGE = auto()           # Generate integer range list: (range start end step)


# Maps builtin function name → (opcode, arity) for all fixed-arity builtins.
#
# This is the single source of truth consumed by:
#   - aifpl_codegen.py: to derive UNARY_OPS / BINARY_OPS / TERNARY_OPS for direct call optimisation
#   - aifpl_builtin_registry.py: to generate bytecode stubs for first-class function use
#
# Variadic builtins that are fold-reduced by the desugarer appear here with
# arity=2 (their binary form), since that is the only arity a first-class call
# will ever present to the stub.
#
# 'alist-get' and 'range' have optional arguments; their stubs always use the
# 3-argument opcode form (the codegen synthesises the missing default for direct
# calls, and the stub will do likewise).
BUILTIN_OPCODE_MAP: Dict[str, Tuple[Opcode, int]] = {
    'function?': (Opcode.FUNCTION_P, 1),
    'function=?': (Opcode.FUNCTION_EQ_P, 2),
    'function!=?': (Opcode.FUNCTION_NEQ_P, 2),
    'function-min-arity': (Opcode.FUNCTION_MIN_ARITY, 1),
    'function-variadic?': (Opcode.FUNCTION_VARIADIC_P, 1),
    'function-accepts?': (Opcode.FUNCTION_ACCEPTS_P, 2),
    'boolean?': (Opcode.BOOLEAN_P, 1),
    'boolean=?': (Opcode.BOOLEAN_EQ_P, 2),
    'boolean!=?': (Opcode.BOOLEAN_NEQ_P, 2),
    'boolean-not': (Opcode.BOOLEAN_NOT, 1),
    'integer?': (Opcode.INTEGER_P, 1),
    'integer=?': (Opcode.INTEGER_EQ_P, 2),
    'integer!=?': (Opcode.INTEGER_NEQ_P, 2),
    'integer<?': (Opcode.INTEGER_LT_P, 2),
    'integer>?': (Opcode.INTEGER_GT_P, 2),
    'integer<=?': (Opcode.INTEGER_LTE_P, 2),
    'integer>=?': (Opcode.INTEGER_GTE_P, 2),
    'integer+': (Opcode.INTEGER_ADD, 2),
    'integer-': (Opcode.INTEGER_SUB, 2),
    'integer*': (Opcode.INTEGER_MUL, 2),
    'integer/': (Opcode.INTEGER_DIV, 2),
    'integer%': (Opcode.INTEGER_MOD, 2),
    'integer-neg': (Opcode.INTEGER_NEG, 1),
    'integer-abs': (Opcode.INTEGER_ABS, 1),
    'bit-not': (Opcode.INTEGER_BIT_NOT, 1),
    'bit-shift-left': (Opcode.INTEGER_BIT_SHIFT_LEFT, 2),
    'bit-shift-right': (Opcode.INTEGER_BIT_SHIFT_RIGHT, 2),
    'bit-or': (Opcode.INTEGER_BIT_OR, 2),
    'bit-and': (Opcode.INTEGER_BIT_AND, 2),
    'bit-xor': (Opcode.INTEGER_BIT_XOR, 2),
    'integer-min': (Opcode.INTEGER_MIN, 2),
    'integer-max': (Opcode.INTEGER_MAX, 2),
    'integer->string': (Opcode.INTEGER_TO_STRING, 1),
    'bin': (Opcode.INTEGER_TO_STRING_BIN, 1),
    'hex': (Opcode.INTEGER_TO_STRING_HEX, 1),
    'oct': (Opcode.INTEGER_TO_STRING_OCT, 1),
    'integer->float': (Opcode.INTEGER_TO_FLOAT, 1),
    'float?': (Opcode.FLOAT_P, 1),
    'float=?': (Opcode.FLOAT_EQ_P, 2),
    'float!=?': (Opcode.FLOAT_NEQ_P, 2),
    'float<?': (Opcode.FLOAT_LT_P, 2),
    'float>?': (Opcode.FLOAT_GT_P, 2),
    'float<=?': (Opcode.FLOAT_LTE_P, 2),
    'float>=?': (Opcode.FLOAT_GTE_P, 2),
    'float+': (Opcode.FLOAT_ADD, 2),
    'float-': (Opcode.FLOAT_SUB, 2),
    'float*': (Opcode.FLOAT_MUL, 2),
    'float/': (Opcode.FLOAT_DIV, 2),
    'float//': (Opcode.FLOAT_FLOOR_DIV, 2),
    'float%': (Opcode.FLOAT_MOD, 2),
    'float-neg': (Opcode.FLOAT_NEG, 1),
    'float-expt': (Opcode.FLOAT_EXPT, 2),
    'float-sin': (Opcode.FLOAT_SIN, 1),
    'float-cos': (Opcode.FLOAT_COS, 1),
    'float-tan': (Opcode.FLOAT_TAN, 1),
    'float-log': (Opcode.FLOAT_LOG, 1),
    'float-log10': (Opcode.FLOAT_LOG10, 1),
    'float-exp': (Opcode.FLOAT_EXP, 1),
    'float-sqrt': (Opcode.FLOAT_SQRT, 1),
    'float-abs': (Opcode.FLOAT_ABS, 1),
    'float->integer': (Opcode.FLOAT_TO_INTEGER, 1),
    'float->string': (Opcode.FLOAT_TO_STRING, 1),
    'float-floor': (Opcode.FLOAT_FLOOR, 1),
    'float-ceil': (Opcode.FLOAT_CEIL, 1),
    'float-round': (Opcode.FLOAT_ROUND, 1),
    'float-min': (Opcode.FLOAT_MIN, 2),
    'float-max': (Opcode.FLOAT_MAX, 2),
    'complex': (Opcode.COMPLEX, 2),
    'complex?': (Opcode.COMPLEX_P, 1),
    'complex=?': (Opcode.COMPLEX_EQ_P, 2),
    'complex!=?': (Opcode.COMPLEX_NEQ_P, 2),
    'complex+': (Opcode.COMPLEX_ADD, 2),
    'complex-': (Opcode.COMPLEX_SUB, 2),
    'complex*': (Opcode.COMPLEX_MUL, 2),
    'complex/': (Opcode.COMPLEX_DIV, 2),
    'complex-neg': (Opcode.COMPLEX_NEG, 1),
    'complex-expt': (Opcode.COMPLEX_EXPT, 2),
    'complex-sin': (Opcode.COMPLEX_SIN, 1),
    'complex-cos': (Opcode.COMPLEX_COS, 1),
    'complex-tan': (Opcode.COMPLEX_TAN, 1),
    'complex-log': (Opcode.COMPLEX_LOG, 1),
    'complex-log10': (Opcode.COMPLEX_LOG10, 1),
    'complex-exp': (Opcode.COMPLEX_EXP, 1),
    'complex-sqrt': (Opcode.COMPLEX_SQRT, 1),
    'complex-abs': (Opcode.COMPLEX_ABS, 1),
    'complex->string': (Opcode.COMPLEX_TO_STRING, 1),
    'complex-real': (Opcode.COMPLEX_REAL, 1),
    'complex-imag': (Opcode.COMPLEX_IMAG, 1),
    'string?': (Opcode.STRING_P, 1),
    'string=?': (Opcode.STRING_EQ_P, 2),
    'string!=?': (Opcode.STRING_NEQ_P, 2),
    'string<?': (Opcode.STRING_LT_P, 2),
    'string>?': (Opcode.STRING_GT_P, 2),
    'string<=?': (Opcode.STRING_LTE_P, 2),
    'string>=?': (Opcode.STRING_GTE_P, 2),
    'string-length': (Opcode.STRING_LENGTH, 1),
    'string-upcase': (Opcode.STRING_UPCASE, 1),
    'string-downcase': (Opcode.STRING_DOWNCASE, 1),
    'string-trim': (Opcode.STRING_TRIM, 1),
    'string->number': (Opcode.STRING_TO_NUMBER, 1),
    'string->list': (Opcode.STRING_TO_LIST, 2),
    'string-ref': (Opcode.STRING_REF, 2),
    'string-contains?': (Opcode.STRING_CONTAINS_P, 2),
    'string-prefix?': (Opcode.STRING_PREFIX_P, 2),
    'string-suffix?': (Opcode.STRING_SUFFIX_P, 2),
    'string-concat': (Opcode.STRING_CONCAT, 2),
    'string-slice': (Opcode.STRING_SLICE, 3),
    'string-replace': (Opcode.STRING_REPLACE, 3),
    'list?': (Opcode.LIST_P, 1),
    'list=?': (Opcode.LIST_EQ_P, 2),
    'list!=?': (Opcode.LIST_NEQ_P, 2),
    'list-prepend': (Opcode.LIST_PREPEND, 2),
    'list-append': (Opcode.LIST_APPEND, 2),
    'list-reverse': (Opcode.LIST_REVERSE, 1),
    'list-first': (Opcode.LIST_FIRST, 1),
    'list-rest': (Opcode.LIST_REST, 1),
    'list-last': (Opcode.LIST_LAST, 1),
    'list-length': (Opcode.LIST_LENGTH, 1),
    'list-ref': (Opcode.LIST_REF, 2),
    'list-null?': (Opcode.LIST_NULL_P, 1),
    'list-member?': (Opcode.LIST_MEMBER_P, 2),
    'list-position': (Opcode.LIST_POSITION, 2),
    'list-slice': (Opcode.LIST_SLICE, 3),
    'list-remove': (Opcode.LIST_REMOVE, 2),
    'list-concat': (Opcode.LIST_CONCAT, 2),
    'list->string': (Opcode.LIST_TO_STRING, 2),
    'alist?': (Opcode.ALIST_P, 1),
    'alist=?': (Opcode.ALIST_EQ_P, 2),
    'alist!=?': (Opcode.ALIST_NEQ_P, 2),
    'alist-keys': (Opcode.ALIST_KEYS, 1),
    'alist-values': (Opcode.ALIST_VALUES, 1),
    'alist-length': (Opcode.ALIST_LENGTH, 1),
    'alist-has?': (Opcode.ALIST_HAS_P, 2),
    'alist-remove': (Opcode.ALIST_REMOVE, 2),
    'alist-merge': (Opcode.ALIST_MERGE, 2),
    'alist-set': (Opcode.ALIST_SET, 3),
    'alist-get': (Opcode.ALIST_GET, 3),
    'range': (Opcode.RANGE, 3),

}

@dataclass
class Instruction:
    """Single bytecode instruction.

    Stores opcode and arguments for easier debugging and manipulation.
    In the VM, we'll use a more compact representation.
    """
    opcode: Opcode
    arg1: int = 0
    arg2: int = 0

    def arg_count(self) -> int:
        """Return the number of arguments this instruction takes (0, 1, or 2)."""
        # Opcodes with no arguments (all primitive operations that take operands
        # from the stack rather than from the instruction stream)
        no_arg_opcodes = {
            Opcode.LOAD_TRUE,
            Opcode.LOAD_FALSE,
            Opcode.LOAD_EMPTY_LIST,
            Opcode.TAIL_APPLY,
            Opcode.APPLY,
            Opcode.RETURN,
            Opcode.EMIT_TRACE,
            Opcode.FUNCTION_P,
            Opcode.FUNCTION_EQ_P,
            Opcode.FUNCTION_NEQ_P,
            Opcode.FUNCTION_MIN_ARITY,
            Opcode.FUNCTION_VARIADIC_P,
            Opcode.FUNCTION_ACCEPTS_P,
            Opcode.BOOLEAN_P,
            Opcode.BOOLEAN_EQ_P,
            Opcode.BOOLEAN_NEQ_P,
            Opcode.BOOLEAN_NOT,
            Opcode.INTEGER_P,
            Opcode.INTEGER_EQ_P,
            Opcode.INTEGER_NEQ_P,
            Opcode.INTEGER_LT_P,
            Opcode.INTEGER_GT_P,
            Opcode.INTEGER_LTE_P,
            Opcode.INTEGER_GTE_P,
            Opcode.INTEGER_ADD,
            Opcode.INTEGER_SUB,
            Opcode.INTEGER_MUL,
            Opcode.INTEGER_DIV,
            Opcode.INTEGER_NEG,
            Opcode.INTEGER_ABS,
            Opcode.INTEGER_BIT_OR,
            Opcode.INTEGER_BIT_AND,
            Opcode.INTEGER_BIT_XOR,
            Opcode.INTEGER_BIT_NOT,
            Opcode.INTEGER_BIT_SHIFT_LEFT,
            Opcode.INTEGER_BIT_SHIFT_RIGHT,
            Opcode.INTEGER_MIN,
            Opcode.INTEGER_MAX,
            Opcode.INTEGER_TO_STRING,
            Opcode.INTEGER_TO_STRING_BIN,
            Opcode.INTEGER_TO_STRING_HEX,
            Opcode.INTEGER_TO_STRING_OCT,
            Opcode.INTEGER_TO_FLOAT,
            Opcode.FLOAT_P,
            Opcode.FLOAT_EQ_P,
            Opcode.FLOAT_NEQ_P,
            Opcode.FLOAT_LT_P,
            Opcode.FLOAT_GT_P,
            Opcode.FLOAT_LTE_P,
            Opcode.FLOAT_GTE_P,
            Opcode.FLOAT_ADD,
            Opcode.FLOAT_SUB,
            Opcode.FLOAT_MUL,
            Opcode.FLOAT_DIV,
            Opcode.FLOAT_FLOOR_DIV,
            Opcode.FLOAT_MOD,
            Opcode.FLOAT_NEG,
            Opcode.FLOAT_EXPT,
            Opcode.FLOAT_SIN,
            Opcode.FLOAT_COS,
            Opcode.FLOAT_TAN,
            Opcode.FLOAT_LOG,
            Opcode.FLOAT_LOG10,
            Opcode.FLOAT_EXP,
            Opcode.FLOAT_SQRT,
            Opcode.FLOAT_ABS,
            Opcode.FLOAT_TO_INTEGER,
            Opcode.FLOAT_TO_STRING,
            Opcode.FLOAT_FLOOR,
            Opcode.FLOAT_CEIL,
            Opcode.FLOAT_ROUND,
            Opcode.FLOAT_MIN,
            Opcode.FLOAT_MAX,
            Opcode.COMPLEX,
            Opcode.COMPLEX_P,
            Opcode.COMPLEX_EQ_P,
            Opcode.COMPLEX_NEQ_P,
            Opcode.COMPLEX_ADD,
            Opcode.COMPLEX_SUB,
            Opcode.COMPLEX_MUL,
            Opcode.COMPLEX_DIV,
            Opcode.COMPLEX_NEG,
            Opcode.COMPLEX_EXPT,
            Opcode.COMPLEX_SIN,
            Opcode.COMPLEX_COS,
            Opcode.COMPLEX_TAN,
            Opcode.COMPLEX_LOG,
            Opcode.COMPLEX_EXP,
            Opcode.COMPLEX_SQRT,
            Opcode.COMPLEX_ABS,
            Opcode.COMPLEX_REAL,
            Opcode.COMPLEX_IMAG,
            Opcode.COMPLEX_TO_STRING,
            Opcode.STRING_P,
            Opcode.STRING_EQ_P,
            Opcode.STRING_NEQ_P,
            Opcode.STRING_LT_P,
            Opcode.STRING_GT_P,
            Opcode.STRING_LTE_P,
            Opcode.STRING_GTE_P,
            Opcode.STRING_LENGTH,
            Opcode.STRING_UPCASE,
            Opcode.STRING_DOWNCASE,
            Opcode.STRING_TRIM,
            Opcode.STRING_TO_NUMBER,
            Opcode.STRING_TO_LIST,
            Opcode.STRING_REF,
            Opcode.STRING_CONTAINS_P,
            Opcode.STRING_PREFIX_P,
            Opcode.STRING_SUFFIX_P,
            Opcode.STRING_CONCAT,
            Opcode.STRING_SLICE,
            Opcode.STRING_REPLACE,
            Opcode.LIST_P,
            Opcode.LIST_EQ_P,
            Opcode.LIST_NEQ_P,
            Opcode.LIST_PREPEND,
            Opcode.LIST_APPEND,
            Opcode.LIST_REVERSE,
            Opcode.LIST_FIRST,
            Opcode.LIST_REST,
            Opcode.LIST_LAST,
            Opcode.LIST_LENGTH,
            Opcode.LIST_REF,
            Opcode.LIST_NULL_P,
            Opcode.LIST_MEMBER_P,
            Opcode.LIST_POSITION,
            Opcode.LIST_SLICE,
            Opcode.LIST_SLICE,
            Opcode.LIST_REMOVE,
            Opcode.LIST_CONCAT,
            Opcode.LIST_TO_STRING,
            Opcode.ALIST_P,
            Opcode.ALIST_EQ_P,
            Opcode.ALIST_NEQ_P,
            Opcode.ALIST_KEYS,
            Opcode.ALIST_VALUES,
            Opcode.ALIST_LENGTH,
            Opcode.ALIST_HAS_P,
            Opcode.ALIST_REMOVE,
            Opcode.ALIST_MERGE,
            Opcode.ALIST_GET,
            Opcode.ALIST_SET,
            Opcode.RANGE,
        }
        if self.opcode in no_arg_opcodes:
            return 0

        # Opcodes with two instruction-stream arguments
        two_arg_opcodes = {
            Opcode.MAKE_CLOSURE,
            Opcode.LOAD_PARENT_VAR,
        }
        if self.opcode in two_arg_opcodes:
            return 2

        # All other opcodes take one instruction-stream argument
        return 1

    def __repr__(self) -> str:
        """Human-readable representation."""
        n = self.arg_count()
        if n == 0:
            return f"{self.opcode.name}"

        if n == 2:
            return f"{self.opcode.name} {self.arg1} {self.arg2}"

        return f"{self.opcode.name} {self.arg1}"


@dataclass
class CodeObject:
    """Compiled code object containing bytecode and metadata.

    This represents a compiled AIFPL expression or function body.
    """

    # Bytecode instructions
    instructions: List[Instruction]

    # Constant pool (for LOAD_CONST)
    constants: List[AIFPLValue]

    # Name pool (for LOAD_GLOBAL)
    names: List[str]

    # Nested code objects (for lambdas/closures)
    code_objects: List['CodeObject']

    # Function metadata
    free_vars: List[str] = field(default_factory=list)  # Free variables to capture
    param_names: List[str] = field(default_factory=list)  # Parameter names (in order, parallel to param_count)
    param_count: int = 0  # Number of parameters (for functions)
    local_count: int = 0  # Number of local variables
    is_variadic: bool = False  # True if last param is a rest parameter (packs excess args into a list)
    name: str = "<module>"  # Name for debugging
    source_line: int = 0  # Line number in source code where this function is defined
    source_file: str = ""  # Source file name (if available)

    def __repr__(self) -> str:
        """Human-readable representation."""
        lines = [f"CodeObject: {self.name}"]
        lines.append(f"  Parameters: {self.param_count}")
        lines.append(f"  Locals: {self.local_count}")
        lines.append(f"  Constants: {len(self.constants)}")
        lines.append(f"  Names: {self.names}")
        lines.append("  Instructions:")
        for i, instr in enumerate(self.instructions):
            lines.append(f"    {i:3d}: {instr}")

        return "\n".join(lines)

    def disassemble(self) -> str:
        """Return disassembled bytecode for debugging."""
        return repr(self)
