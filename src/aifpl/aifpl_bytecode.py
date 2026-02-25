"""Bytecode definitions for AIFPL virtual machine."""

from dataclasses import dataclass, field
from enum import IntEnum
from typing import Dict, List, Tuple

from aifpl.aifpl_value import AIFPLValue


def _op(n: int, arg_count: int = 0) -> Tuple[int, int]:
    """Helper to construct an Opcode value: (integer_value, instruction_stream_arg_count).

    arg_count is the number of instruction-stream arguments the opcode encodes
    (i.e. fields read from the bytecode stream, not operands popped from the stack):
      0 — all operands come from the value stack (the common case for primitives)
      1 — one immediate argument follows the opcode in the stream
      2 — two immediate arguments follow the opcode in the stream
    """
    return (n, arg_count)


class Opcode(IntEnum):
    """Bytecode operation codes.

    Each member's value is a (integer_value, instruction_stream_arg_count) tuple.
    The integer value is used for fast VM dispatch (IntEnum identity).
    The arg_count property returns the number of instruction-stream arguments.

    Encoding arg_count directly on the enum eliminates the error-prone
    no_arg_opcodes / two_arg_opcodes sets that previously lived in
    Instruction.arg_count().
    """

    _arg_count: int  # Set in __new__; declared here so mypy knows the attribute exists

    def __new__(cls, int_value: int, arg_count: int = 0) -> 'Opcode':
        obj = int.__new__(cls, int_value,)
        obj._value_ = int_value
        obj._arg_count = arg_count
        return obj

    @property
    def arg_count(self) -> int:
        """Number of instruction-stream arguments (0, 1, or 2)."""
        return self._arg_count

    # Constants
    LOAD_CONST = _op(1, 1)              # LOAD_CONST const_index
    LOAD_TRUE = _op(2, 0)               # Push True
    LOAD_FALSE = _op(3, 0)              # Push False
    LOAD_EMPTY_LIST = _op(4, 0)         # Push empty list

    # Variables (lexically addressed)
    LOAD_VAR = _op(5, 1)                # LOAD_VAR index
    STORE_VAR = _op(6, 1)               # STORE_VAR index
    LOAD_PARENT_VAR = _op(7, 2)         # LOAD_PARENT_VAR index depth
    LOAD_NAME = _op(8, 1)               # LOAD_NAME name_index

    # Control flow
    JUMP = _op(10, 1)                   # Unconditional jump: JUMP offset
    JUMP_IF_FALSE = _op(11, 1)          # Conditional jump if false
    JUMP_IF_TRUE = _op(12, 1)           # Conditional jump if true
    RAISE_ERROR = _op(13, 1)            # RAISE_ERROR const_index

    # Functions
    MAKE_CLOSURE = _op(14, 2)           # MAKE_CLOSURE code_index capture_count
    CALL = _op(15, 1)                   # CALL arity
    TAIL_CALL = _op(16, 1)              # TAIL_CALL arity
    APPLY = _op(17, 0)                  # Apply function to arg list (non-tail)
    TAIL_APPLY = _op(18, 0)             # Apply function to arg list (tail position)
    ENTER = _op(19, 1)                  # ENTER n  (pop N args into locals 0..N-1)
    RETURN = _op(20, 0)                 # Return from function

    # Debugging
    EMIT_TRACE = _op(30, 0)             # Emit trace (pops value, emits to watcher)

    # Function operations
    FUNCTION_P = _op(40, 0)             # (function? x)
    FUNCTION_EQ_P = _op(41, 0)          # (function=? f g)
    FUNCTION_NEQ_P = _op(42, 0)         # (function!=? f g)
    FUNCTION_MIN_ARITY = _op(43, 0)     # (function-min-arity f)
    FUNCTION_VARIADIC_P = _op(44, 0)    # (function-variadic? f)
    FUNCTION_ACCEPTS_P = _op(45, 0)     # (function-accepts? f n)

    # Symbol operations
    SYMBOL_P = _op(60, 0)               # (symbol? x)
    SYMBOL_EQ_P = _op(61, 0)            # symbol=? a b
    SYMBOL_NEQ_P = _op(62, 0)           # symbol!=? a b
    SYMBOL_TO_STRING = _op(63, 0)       # (symbol->string sym)

    # Boolean operations
    BOOLEAN_P = _op(80, 0)              # (boolean? x)
    BOOLEAN_EQ_P = _op(81, 0)           # boolean=? a b
    BOOLEAN_NEQ_P = _op(82, 0)          # boolean!=? a b
    BOOLEAN_NOT = _op(83, 0)            # Logical NOT

    # Integer operations
    INTEGER_P = _op(100, 0)             # (integer? x)
    INTEGER_EQ_P = _op(101, 0)          # integer=? a b
    INTEGER_NEQ_P = _op(102, 0)         # integer!=? a b
    INTEGER_LT_P = _op(103, 0)          # integer<? a b
    INTEGER_GT_P = _op(104, 0)          # integer>? a b
    INTEGER_LTE_P = _op(105, 0)         # integer<=? a b
    INTEGER_GTE_P = _op(106, 0)         # integer>=? a b
    INTEGER_ADD = _op(107, 0)           # integer+ a b
    INTEGER_SUB = _op(108, 0)           # integer- a b
    INTEGER_MUL = _op(109, 0)           # integer* a b
    INTEGER_DIV = _op(110, 0)           # integer/ a b  (floor division)
    INTEGER_MOD = _op(111, 0)           # integer% a b  (modulo)
    INTEGER_NEG = _op(112, 0)           # integer-neg x  (unary minus)
    INTEGER_EXPT = _op(113, 0)          # integer-expt a b  (exact integer exponentiation)
    INTEGER_ABS = _op(114, 0)           # integer-abs x
    INTEGER_BIT_NOT = _op(115, 0)       # Bitwise NOT ~x
    INTEGER_BIT_SHIFT_LEFT = _op(116, 0)
                                        # Bitwise left shift x << n
    INTEGER_BIT_SHIFT_RIGHT = _op(117, 0)
                                        # Bitwise right shift x >> n
    INTEGER_BIT_OR = _op(118, 0)        # Bitwise OR: a | b
    INTEGER_BIT_AND = _op(119, 0)       # Bitwise AND: a & b
    INTEGER_BIT_XOR = _op(120, 0)       # Bitwise XOR: a ^ b
    INTEGER_MIN = _op(121, 0)           # integer-min a b
    INTEGER_MAX = _op(122, 0)           # integer-max a b
    INTEGER_TO_FLOAT = _op(123, 0)      # Convert integer to float
    INTEGER_TO_COMPLEX = _op(124, 0)    # integer->complex: construct complex from integer
    INTEGER_TO_STRING = _op(125, 0)     # Convert integer to string
    INTEGER_TO_STRING_BIN = _op(126, 0) # Convert integer to binary string
    INTEGER_TO_STRING_HEX = _op(127, 0) # Convert integer to hex string
    INTEGER_TO_STRING_OCT = _op(128, 0) # Convert integer to octal string

    # Floating point operations
    FLOAT_P = _op(140, 0)               # (float? x)
    FLOAT_EQ_P = _op(141, 0)            # float=? a b
    FLOAT_NEQ_P = _op(142, 0)           # float!=? a b
    FLOAT_LT_P = _op(143, 0)            # float<? a b
    FLOAT_GT_P = _op(144, 0)            # float>? a b
    FLOAT_LTE_P = _op(145, 0)           # float<=? a b
    FLOAT_GTE_P = _op(146, 0)           # float>=? a b
    FLOAT_ADD = _op(147, 0)             # float+ a b
    FLOAT_SUB = _op(148, 0)             # float- a b
    FLOAT_MUL = _op(149, 0)             # float* a b
    FLOAT_DIV = _op(150, 0)             # float/ a b
    FLOAT_FLOOR_DIV = _op(151, 0)       # float// a b  (floor division)
    FLOAT_MOD = _op(152, 0)             # float% a b  (modulo)
    FLOAT_NEG = _op(153, 0)             # float-neg x  (unary minus)
    FLOAT_EXPT = _op(154, 0)            # float-expt a b
    FLOAT_SIN = _op(155, 0)             # float-sin x
    FLOAT_COS = _op(156, 0)             # float-cos x
    FLOAT_TAN = _op(157, 0)             # float-tan x
    FLOAT_LOG = _op(158, 0)             # float-log x
    FLOAT_LOG10 = _op(159, 0)           # float-log10 x
    FLOAT_EXP = _op(160, 0)             # float-exp x
    FLOAT_SQRT = _op(161, 0)            # float-sqrt x
    FLOAT_ABS = _op(162, 0)             # float-abs x
    FLOAT_TO_INTEGER = _op(163, 0)      # Convert float to integer
    FLOAT_TO_COMPLEX = _op(164, 0)      # float->complex: construct complex from one or two floats
    FLOAT_TO_STRING = _op(165, 0)       # Convert float to string
    FLOAT_FLOOR = _op(166, 0)           # float-floor x  (returns float)
    FLOAT_CEIL = _op(167, 0)            # float-ceil x   (returns float)
    FLOAT_ROUND = _op(168, 0)           # float-round x  (returns float)
    FLOAT_MIN = _op(169, 0)             # float-min a b
    FLOAT_MAX = _op(170, 0)             # float-max a b

    # Complex operations
    COMPLEX_P = _op(180, 0)             # (complex? x)
    COMPLEX_EQ_P = _op(181, 0)          # complex=? a b
    COMPLEX_NEQ_P = _op(182, 0)         # complex!=? a b
    COMPLEX_ADD = _op(183, 0)           # complex+ a b
    COMPLEX_SUB = _op(184, 0)           # complex- a b
    COMPLEX_MUL = _op(185, 0)           # complex* a b
    COMPLEX_DIV = _op(186, 0)           # complex/ a b
    COMPLEX_NEG = _op(187, 0)           # complex-neg x  (unary minus)
    COMPLEX_REAL = _op(188, 0)          # Extract real part
    COMPLEX_IMAG = _op(189, 0)          # Extract imaginary part
    COMPLEX_EXPT = _op(190, 0)          # complex-expt a b
    COMPLEX_SIN = _op(191, 0)           # complex-sin x
    COMPLEX_COS = _op(192, 0)           # complex-cos x
    COMPLEX_TAN = _op(193, 0)           # complex-tan x
    COMPLEX_LOG = _op(194, 0)           # complex-log x
    COMPLEX_LOG10 = _op(195, 0)         # complex-log10 x
    COMPLEX_EXP = _op(196, 0)           # complex-exp x
    COMPLEX_SQRT = _op(197, 0)          # complex-sqrt x
    COMPLEX_ABS = _op(198, 0)           # complex-abs x  (returns float: magnitude)
    COMPLEX_TO_STRING = _op(199, 0)     # Convert complex to string

    # String operations
    STRING_P = _op(220, 0)              # (string? x)
    STRING_EQ_P = _op(221, 0)           # string=? a b
    STRING_NEQ_P = _op(222, 0)          # string!=? a b
    STRING_LT_P = _op(223, 0)           # string<? a b  (lexicographic)
    STRING_GT_P = _op(224, 0)           # string>? a b  (lexicographic)
    STRING_LTE_P = _op(225, 0)          # string<=? a b (lexicographic)
    STRING_GTE_P = _op(226, 0)          # string>=? a b (lexicographic)
    STRING_LENGTH = _op(227, 0)         # Get length of string
    STRING_UPCASE = _op(228, 0)         # Convert string to uppercase
    STRING_DOWNCASE = _op(229, 0)       # Convert string to lowercase
    STRING_TRIM = _op(230, 0)           # Trim whitespace from string
    STRING_TRIM_LEFT = _op(231, 0)      # Trim leading whitespace
    STRING_TRIM_RIGHT = _op(232, 0)     # Trim trailing whitespace
    STRING_TO_NUMBER = _op(233, 0)      # Parse string to number
    STRING_TO_LIST = _op(234, 0)        # Split string by delimiter: (string->list str delim)
    STRING_REF = _op(235, 0)            # Get character at index
    STRING_PREFIX_P = _op(237, 0)       # Check if string has prefix
    STRING_SUFFIX_P = _op(238, 0)       # Check if string has suffix
    STRING_CONCAT = _op(239, 0)         # Concatenate two strings: (string-concat a b)
    STRING_SLICE = _op(240, 0)          # Extract substring (string, start, end)
    STRING_REPLACE = _op(241, 0)        # Replace substring (string, old, new)
    STRING_INDEX = _op(242, 0)          # Find index of substring (string, substring)

    # Alist operations
    ALIST = _op(260, 1)                 # ALIST n  (build alist from n pairs on stack)
    ALIST_P = _op(261, 0)               # (alist? x)
    ALIST_EQ_P = _op(262, 0)            # alist=? a b
    ALIST_NEQ_P = _op(263, 0)           # alist!=? a b
    ALIST_KEYS = _op(264, 0)            # Get all keys from alist
    ALIST_VALUES = _op(265, 0)          # Get all values from alist
    ALIST_LENGTH = _op(266, 0)          # Get number of entries in alist
    ALIST_HAS_P = _op(267, 0)           # Check if alist has key
    ALIST_REMOVE = _op(268, 0)          # Remove key from alist
    ALIST_MERGE = _op(269, 0)           # Merge two alists
    ALIST_SET = _op(270, 0)             # Set key in alist (alist, key, value)
    ALIST_GET = _op(271, 0)             # Get value from alist by key with default

    # List operations
    LIST = _op(300, 1)                  # LIST n  (build list from n elements on stack)
    LIST_P = _op(301, 0)                # (list? x)
    LIST_EQ_P = _op(302, 0)             # list=? a b
    LIST_NEQ_P = _op(303, 0)            # list!=? a b
    LIST_PREPEND = _op(304, 0)          # list-prepend item lst
    LIST_APPEND = _op(305, 0)           # list-append lst item
    LIST_REVERSE = _op(306, 0)          # Reverse list on top of stack
    LIST_FIRST = _op(307, 0)            # Get first element of list
    LIST_REST = _op(308, 0)             # Get rest of list (all but first element)
    LIST_LAST = _op(309, 0)             # Get last element of list
    LIST_LENGTH = _op(310, 0)           # Get length of list
    LIST_REF = _op(311, 0)              # Get element at index from list
    LIST_NULL_P = _op(312, 0)           # Check if list is empty
    LIST_MEMBER_P = _op(313, 0)         # Check if item is in list
    LIST_INDEX = _op(314, 0)            # Find index of item in list
    LIST_SLICE = _op(315, 0)            # Slice list: (list-slice lst start end)
    LIST_REMOVE = _op(316, 0)           # Remove all occurrences of item from list
    LIST_CONCAT = _op(317, 0)           # Append two lists: (append a b)
    LIST_TO_STRING = _op(318, 0)        # Join list of strings with separator

    # Generate integer range list
    RANGE = _op(340, 0)                 # (range start end step)


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
    'integer-expt': (Opcode.INTEGER_EXPT, 2),
    'integer-abs': (Opcode.INTEGER_ABS, 1),
    'bit-not': (Opcode.INTEGER_BIT_NOT, 1),
    'bit-shift-left': (Opcode.INTEGER_BIT_SHIFT_LEFT, 2),
    'bit-shift-right': (Opcode.INTEGER_BIT_SHIFT_RIGHT, 2),
    'bit-or': (Opcode.INTEGER_BIT_OR, 2),
    'bit-and': (Opcode.INTEGER_BIT_AND, 2),
    'bit-xor': (Opcode.INTEGER_BIT_XOR, 2),
    'integer-min': (Opcode.INTEGER_MIN, 2),
    'integer-max': (Opcode.INTEGER_MAX, 2),
    'integer->float': (Opcode.INTEGER_TO_FLOAT, 1),
    'integer->complex': (Opcode.INTEGER_TO_COMPLEX, 1),
    'integer->string': (Opcode.INTEGER_TO_STRING, 1),
    'bin': (Opcode.INTEGER_TO_STRING_BIN, 1),
    'hex': (Opcode.INTEGER_TO_STRING_HEX, 1),
    'oct': (Opcode.INTEGER_TO_STRING_OCT, 1),
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
    'float->complex': (Opcode.FLOAT_TO_COMPLEX, 2),
    'float->string': (Opcode.FLOAT_TO_STRING, 1),
    'float-floor': (Opcode.FLOAT_FLOOR, 1),
    'float-ceil': (Opcode.FLOAT_CEIL, 1),
    'float-round': (Opcode.FLOAT_ROUND, 1),
    'float-min': (Opcode.FLOAT_MIN, 2),
    'float-max': (Opcode.FLOAT_MAX, 2),
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
    'string-trim-left': (Opcode.STRING_TRIM_LEFT, 1),
    'string-trim-right': (Opcode.STRING_TRIM_RIGHT, 1),
    'string->number': (Opcode.STRING_TO_NUMBER, 1),
    'string->list': (Opcode.STRING_TO_LIST, 2),
    'string-ref': (Opcode.STRING_REF, 2),
    'string-index': (Opcode.STRING_INDEX, 2),
    'string-prefix?': (Opcode.STRING_PREFIX_P, 2),
    'string-suffix?': (Opcode.STRING_SUFFIX_P, 2),
    'string-concat': (Opcode.STRING_CONCAT, 2),
    'string-slice': (Opcode.STRING_SLICE, 3),
    'string-replace': (Opcode.STRING_REPLACE, 3),
    'symbol?': (Opcode.SYMBOL_P, 1),
    'symbol=?': (Opcode.SYMBOL_EQ_P, 2),
    'symbol!=?': (Opcode.SYMBOL_NEQ_P, 2),
    'symbol->string': (Opcode.SYMBOL_TO_STRING, 1),
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
    'list-index': (Opcode.LIST_INDEX, 2),
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
        """Return the number of instruction-stream arguments this instruction takes (0, 1, or 2)."""
        return self.opcode.arg_count

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
