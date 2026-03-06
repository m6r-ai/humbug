"""Bytecode definitions for Menai virtual machine."""

from dataclasses import dataclass, field
from enum import IntEnum
from typing import Dict, List, Tuple

from menai.menai_value import MenaiValue


def _op(n: int, arg_count: int = 0) -> Tuple[int, int]:
    """Helper to construct an Opcode value: (integer_value, arg_count).

    arg_count is the number of instruction-stream arguments the opcode encodes
    (i.e. fields read from the bytecode stream, not operands popped from the stack):
      0 — all operands come from the value stack (the common case for primitives)
      1 — one immediate argument follows the opcode in the stream
      2 — two immediate arguments follow the opcode in the stream
    """
    return (n, arg_count)


class Opcode(IntEnum):
    """Bytecode operation codes.

    Each member's value is a (integer_value, arg_count) tuple.
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

    def arg_count(self) -> int:
        """Number of instruction-stream arguments (0, 1, or 2)."""
        return self._arg_count

    # Constants
    LOAD_NONE = _op(0)                  # r_dest = #none
    LOAD_TRUE = _op(1)                  # r_dest = #t
    LOAD_FALSE = _op(2)                 # r_dest = #f
    LOAD_EMPTY_LIST = _op(3)            # r_dest = []
    LOAD_EMPTY_DICT = _op(4)            # r_dest = {}
    LOAD_CONST = _op(5, 1)              # r_dest = constants[src0]
    LOAD_NAME = _op(6, 1)               # r_dest = globals[names[src0]]

    # Stack / register transfer
    PUSH = _op(10, 1)                   # PUSH src0  — push register src0 onto the call stack
    POP = _op(11)                       # r_dest = POP — pop call stack top into register dest

    # Control flow
    JUMP = _op(20, 1)                   # Unconditional jump: JUMP offset
    JUMP_IF_FALSE = _op(21, 2)          # JUMP_IF_FALSE r_src0, @src1 — jump to src1 if r_src0 is false
    JUMP_IF_TRUE = _op(22, 2)           # JUMP_IF_TRUE r_src0, @src1 — jump to src1 if r_src0 is true
    RAISE_ERROR = _op(23, 1)            # RAISE_ERROR const_index

    # Functions
    MAKE_CLOSURE = _op(30, 1)           # r_dest = MAKE_CLOSURE code_idx
    PATCH_CLOSURE = _op(31, 3)          # PATCH_CLOSURE closure_reg, value_reg, capture_idx
    CALL = _op(32, 1)                   # r_dest = CALL arity — result written to dest
    TAIL_CALL = _op(33, 1)              # TAIL_CALL arity — no dest (result propagates up)
    APPLY = _op(34)                     # r_dest = APPLY — result written to dest
    TAIL_APPLY = _op(35)                # TAIL_APPLY — no dest (result propagates up)
    ENTER = _op(36, 1)                  # ENTER n (pop N args into locals 0..N-1)
    RETURN = _op(37, 1)                 # RETURN src0 — push frame.locals[src0] as return value

    # Debugging
    EMIT_TRACE = _op(40, 1)             # EMIT_TRACE src0 — read register src0, emit to trace watcher

    # None operations
    NONE_P = _op(50, 1)                 # r_dest = (none? r_src0)

    # Function operations
    FUNCTION_P = _op(60, 1)             # r_dest = (function? r_src0)
    FUNCTION_EQ_P = _op(61, 2)          # r_dest = (function=? r_src0 r_src1)
    FUNCTION_NEQ_P = _op(62, 2)         # r_dest = (function!=? r_src0 r_src1)
    FUNCTION_MIN_ARITY = _op(63, 1)     # r_dest = (function-min-arity r_src0)
    FUNCTION_VARIADIC_P = _op(64, 1)    # r_dest = (function-variadic? r_src0)
    FUNCTION_ACCEPTS_P = _op(65, 2)     # r_dest = (function-accepts? r_src0 r_src1)

    # Symbol operations
    SYMBOL_P = _op(80, 1)               # r_dest = (symbol? r_src0)
    SYMBOL_EQ_P = _op(81, 2)            # r_dest = (symbol=? r_src0 r_src1)
    SYMBOL_NEQ_P = _op(82, 2)           # r_dest = (symbol!=? r_src0 r_src1)
    SYMBOL_TO_STRING = _op(83, 1)       # r_dest = (symbol->string r_src0)

    # Boolean operations
    BOOLEAN_P = _op(100, 1)             # r_dest = (boolean? r_src0)
    BOOLEAN_EQ_P = _op(101, 2)          # r_dest = (boolean=? r_src0 r_src1)
    BOOLEAN_NEQ_P = _op(102, 2)         # r_dest = (boolean!=? r_src0 r_src1)
    BOOLEAN_NOT = _op(103, 1)           # r_dest = (boolean-not r_src0)

    # Integer operations
    INTEGER_P = _op(120, 1)             # r_dest = (integer? r_src0)
    INTEGER_EQ_P = _op(121, 2)          # r_dest = (integer=? r_src0 r_src1)
    INTEGER_NEQ_P = _op(122, 2)         # r_dest = (integer!=? r_src0 r_src1)
    INTEGER_LT_P = _op(123, 2)          # r_dest = (integer<? r_src0 r_src1)
    INTEGER_GT_P = _op(124, 2)          # r_dest = (integer>? r_src0 r_src1)
    INTEGER_LTE_P = _op(125, 2)         # r_dest = (integer<=? r_src0 r_src1)
    INTEGER_GTE_P = _op(126, 2)         # r_dest = (integer>=? r_src0 r_src1)
    INTEGER_ABS = _op(127, 1)           # r_dest = (integer-abs r_src0)
    INTEGER_ADD = _op(128, 2)           # r_dest = (integer+ r_src0 r_src1)
    INTEGER_SUB = _op(129, 2)           # r_dest = (integer- r_src0 r_src1)
    INTEGER_MUL = _op(130, 2)           # r_dest = (integer* r_src0 r_src1)
    INTEGER_DIV = _op(131, 2)           # r_dest = (integer/ r_src0 r_src1)
    INTEGER_MOD = _op(132, 2)           # r_dest = (integer% r_src0 r_src1)
    INTEGER_NEG = _op(133, 1)           # r_dest = (integer-neg r_src0)
    INTEGER_EXPN = _op(134, 2)          # r_dest = (integer-expn r_src0 r_src1)
    INTEGER_BIT_NOT = _op(135, 1)       # r_dest = (integer-bit-not r_src0)
    INTEGER_BIT_SHIFT_LEFT = _op(136, 2)
                                        # r_dest = (integer-bit-shift-left r_src0 r_src1)
    INTEGER_BIT_SHIFT_RIGHT = _op(137, 2)
                                        # r_dest = (integer-bit-shift-right r_src0 r_src1)
    INTEGER_BIT_OR = _op(138, 2)        # r_dest = (integer-bit-or r_src0 r_src1)
    INTEGER_BIT_AND = _op(139, 2)       # r_dest = (integer-bit-and r_src0 r_src1)
    INTEGER_BIT_XOR = _op(140, 2)       # r_dest = (integer-bit-xor r_src0 r_src1)
    INTEGER_MIN = _op(141, 2)           # r_dest = (integer-min r_src0 r_src1)
    INTEGER_MAX = _op(142, 2)           # r_dest = (integer-max r_src0 r_src1)
    INTEGER_TO_FLOAT = _op(143, 1)      # r_dest = (integer->float r_src0)
    INTEGER_TO_COMPLEX = _op(144, 2)    # r_dest = (integer->complex r_src0 r_src1)
    INTEGER_TO_STRING = _op(145, 2)     # r_dest = (integer->string r_src0 r_src1)

    # Floating point operations
    FLOAT_P = _op(160, 1)               # r_dest = (float? r_src0)
    FLOAT_EQ_P = _op(161, 2)            # r_dest = (float=? r_src0 r_src1)
    FLOAT_NEQ_P = _op(162, 2)           # r_dest = (float!=? r_src0 r_src1)
    FLOAT_LT_P = _op(163, 2)            # r_dest = (float<? r_src0 r_src1)
    FLOAT_GT_P = _op(164, 2)            # r_dest = (float>? r_src0 r_src1)
    FLOAT_LTE_P = _op(165, 2)           # r_dest = (float<=? r_src0 r_src1)
    FLOAT_GTE_P = _op(166, 2)           # r_dest = (float>=? r_src0 r_src1)
    FLOAT_NEG = _op(167, 1)             # r_dest = (float-neg r_src0)
    FLOAT_ADD = _op(168, 2)             # r_dest = (float+ r_src0 r_src1)
    FLOAT_SUB = _op(169, 2)             # r_dest = (float- r_src0 r_src1)
    FLOAT_MUL = _op(170, 2)             # r_dest = (float* r_src0 r_src1)
    FLOAT_DIV = _op(171, 2)             # r_dest = (float/ r_src0 r_src1)
    FLOAT_FLOOR_DIV = _op(172, 2)       # r_dest = (float// r_src0 r_src1)
    FLOAT_MOD = _op(173, 2)             # r_dest = (float% r_src0 r_src1)
    FLOAT_EXP = _op(174, 1)             # r_dest = (float-exp r_src0)
    FLOAT_EXPN = _op(175, 2)            # r_dest = (float-expn r_src0 r_src1)
    FLOAT_LOG = _op(176, 1)             # r_dest = (float-log r_src0)
    FLOAT_LOG10 = _op(177, 1)           # r_dest = (float-log10 r_src0)
    FLOAT_LOG2 = _op(178, 1)            # r_dest = (float-log2 r_src0)
    FLOAT_LOGN = _op(179, 2)            # r_dest = (float-logn r_src0 r_src1)
    FLOAT_SIN = _op(180, 1)             # r_dest = (float-sin r_src0)
    FLOAT_COS = _op(181, 1)             # r_dest = (float-cos r_src0)
    FLOAT_TAN = _op(182, 1)             # r_dest = (float-tan r_src0)
    FLOAT_SQRT = _op(183, 1)            # r_dest = (float-sqrt r_src0)
    FLOAT_ABS = _op(184, 1)             # r_dest = (float-abs r_src0)
    FLOAT_TO_INTEGER = _op(185, 1)      # r_dest = (float->integer r_src0)
    FLOAT_TO_COMPLEX = _op(186, 2)      # r_dest = (float->complex r_src0 r_src1)
    FLOAT_TO_STRING = _op(187, 1)       # r_dest = (float->string r_src0)
    FLOAT_FLOOR = _op(188, 1)           # r_dest = (float-floor r_src0)
    FLOAT_CEIL = _op(189, 1)            # r_dest = (float-ceil r_src0)
    FLOAT_ROUND = _op(190, 1)           # r_dest = (float-round r_src0)
    FLOAT_MIN = _op(191, 2)             # r_dest = (float-min r_src0 r_src1)
    FLOAT_MAX = _op(192, 2)             # r_dest = (float-max r_src0 r_src1)

    # Complex operations
    COMPLEX_P = _op(200, 1)             # r_dest = (complex? r_src0)
    COMPLEX_EQ_P = _op(201, 2)          # r_dest = (complex=? r_src0 r_src1)
    COMPLEX_NEQ_P = _op(202, 2)         # r_dest = (complex!=? r_src0 r_src1)
    COMPLEX_REAL = _op(203, 1)          # r_dest = (complex-real r_src0)
    COMPLEX_IMAG = _op(204, 1)          # r_dest = (complex-imag r_src0)
    COMPLEX_ABS = _op(205, 1)           # r_dest = (complex-abs r_src0)
    COMPLEX_ADD = _op(206, 2)           # r_dest = (complex+ r_src0 r_src1)
    COMPLEX_SUB = _op(207, 2)           # r_dest = (complex- r_src0 r_src1)
    COMPLEX_MUL = _op(208, 2)           # r_dest = (complex* r_src0 r_src1)
    COMPLEX_DIV = _op(209, 2)           # r_dest = (complex/ r_src0 r_src1)
    COMPLEX_NEG = _op(210, 1)           # r_dest = (complex-neg r_src0)
    COMPLEX_EXP = _op(211, 1)           # r_dest = (complex-exp r_src0)
    COMPLEX_EXPN = _op(212, 2)          # r_dest = (complex-expn r_src0 r_src1)
    COMPLEX_LOG = _op(213, 1)           # r_dest = (complex-log r_src0)
    COMPLEX_LOG10 = _op(214, 1)         # r_dest = (complex-log10 r_src0)
    COMPLEX_LOGN = _op(215, 2)          # r_dest = (complex-logn r_src0 r_src1)
    COMPLEX_SIN = _op(216, 1)           # r_dest = (complex-sin r_src0)
    COMPLEX_COS = _op(217, 1)           # r_dest = (complex-cos r_src0)
    COMPLEX_TAN = _op(218, 1)           # r_dest = (complex-tan r_src0)
    COMPLEX_SQRT = _op(219, 1)          # r_dest = (complex-sqrt r_src0)
    COMPLEX_TO_STRING = _op(220, 1)     # r_dest = (complex->string r_src0)

    # String operations
    STRING_P = _op(240, 1)              # r_dest = (string? r_src0)
    STRING_EQ_P = _op(241, 2)           # r_dest = (string=? r_src0 r_src1)
    STRING_NEQ_P = _op(242, 2)          # r_dest = (string!=? r_src0 r_src1)
    STRING_LT_P = _op(243, 2)           # r_dest = (string<? r_src0 r_src1)
    STRING_GT_P = _op(244, 2)           # r_dest = (string>? r_src0 r_src1)
    STRING_LTE_P = _op(245, 2)          # r_dest = (string<=? r_src0 r_src1)
    STRING_GTE_P = _op(246, 2)          # r_dest = (string>=? r_src0 r_src1)
    STRING_LENGTH = _op(247, 1)         # r_dest = (string-length r_src0)
    STRING_UPCASE = _op(248, 1)         # r_dest = (string-upcase r_src0)
    STRING_DOWNCASE = _op(249, 1)       # r_dest = (string-downcase r_src0)
    STRING_TRIM = _op(250, 1)           # r_dest = (string-trim r_src0)
    STRING_TRIM_LEFT = _op(251, 1)      # r_dest = (string-trim-left r_src0)
    STRING_TRIM_RIGHT = _op(252, 1)     # r_dest = (string-trim-right r_src0)
    STRING_TO_INTEGER = _op(253, 2)     # r_dest = (string->integer r_src0 r_src1)
    STRING_TO_NUMBER = _op(254, 1)      # r_dest = (string->number r_src0)
    STRING_TO_LIST = _op(255, 2)        # r_dest = (string->list r_src0 r_src1)
    STRING_REF = _op(256, 2)            # r_dest = (string-ref r_src0 r_src1)
    STRING_PREFIX_P = _op(257, 2)       # r_dest = (string-prefix? r_src0 r_src1)
    STRING_SUFFIX_P = _op(258, 2)       # r_dest = (string-suffix? r_src0 r_src1)
    STRING_CONCAT = _op(259, 2)         # r_dest = (string-concat r_src0 r_src1)
    STRING_SLICE = _op(260, 3)          # r_dest = (string-slice r_src0 r_src1 r_src2)
    STRING_REPLACE = _op(261, 3)        # r_dest = (string-replace r_src0 r_src1 r_src2)
    STRING_INDEX = _op(262, 2)          # r_dest = (string-index r_src0 r_src1)

    # Alist operations
    DICT_P = _op(281, 1)                # r_dest = (dict? r_src0)
    DICT_EQ_P = _op(282, 2)             # r_dest = (dict=? r_src0 r_src1)
    DICT_NEQ_P = _op(283, 2)            # r_dest = (dict!=? r_src0 r_src1)
    DICT_KEYS = _op(284, 1)             # r_dest = (dict-keys r_src0)
    DICT_VALUES = _op(285, 1)           # r_dest = (dict-values r_src0)
    DICT_LENGTH = _op(286, 1)           # r_dest = (dict-length r_src0)
    DICT_HAS_P = _op(287, 2)            # r_dest = (dict-has? r_src0 r_src1)
    DICT_REMOVE = _op(288, 2)           # r_dest = (dict-remove r_src0 r_src1)
    DICT_MERGE = _op(289, 2)            # r_dest = (dict-merge r_src0 r_src1)
    DICT_SET = _op(290, 3)              # r_dest = (dict-set r_src0 r_src1 r_src2)
    DICT_GET = _op(291, 3)              # r_dest = (dict-get r_src0 r_src1 r_src2)

    # List operations
    LIST_P = _op(321, 1)                # r_dest = (list? r_src0)
    LIST_EQ_P = _op(322, 2)             # r_dest = (list=? r_src0 r_src1)
    LIST_NEQ_P = _op(323, 2)            # r_dest = (list!=? r_src0 r_src1)
    LIST_PREPEND = _op(324, 2)          # r_dest = (list-prepend r_src0 r_src1)
    LIST_APPEND = _op(325, 2)           # r_dest = (list-append r_src0 r_src1)
    LIST_REVERSE = _op(326, 1)          # r_dest = (list-reverse r_src0)
    LIST_FIRST = _op(327, 1)            # r_dest = (list-first r_src0)
    LIST_REST = _op(328, 1)             # r_dest = (list-rest r_src0)
    LIST_LAST = _op(329, 1)             # r_dest = (list-last r_src0)
    LIST_LENGTH = _op(330, 1)           # r_dest = (list-length r_src0)
    LIST_REF = _op(331, 2)              # r_dest = (list-ref r_src0 r_src1)
    LIST_NULL_P = _op(332, 1)           # r_dest = (list-null? r_src0)
    LIST_MEMBER_P = _op(333, 2)         # r_dest = (list-member? r_src0 r_src1)
    LIST_INDEX = _op(334, 2)            # r_dest = (list-index r_src0 r_src1)
    LIST_SLICE = _op(335, 3)            # r_dest = (list-slice r_src0 r_src1 r_src2)
    LIST_REMOVE = _op(336, 2)           # r_dest = (list-remove r_src0 r_src1)
    LIST_CONCAT = _op(337, 2)           # r_dest = (list-concat r_src0 r_src1)
    LIST_TO_STRING = _op(338, 2)        # r_dest = (list->string r_src0 r_src1)

    # Generate integer range list
    RANGE = _op(360, 3)                 # r_dest = (range r_src0 r_src1 r_src2)


# Maps builtin function name → (opcode, arity) for all fixed-arity builtins.
#
# This is the single source of truth consumed by:
#   - menai_vm_codegen.py: to derive UNARY_OPS / BINARY_OPS / TERNARY_OPS for direct call optimisation
#   - menai_builtin_registry.py: to generate bytecode stubs for first-class function use
#
# Variadic builtins that are fold-reduced by the desugarer appear here with
# arity=2 (their binary form), since that is the only arity a first-class call
# will ever present to the stub.
#
# 'dict-get' and 'range' have optional arguments; their stubs always use the
# 3-argument opcode form (the codegen synthesises the missing default for direct
# calls, and the stub will do likewise).
BUILTIN_OPCODE_MAP: Dict[str, Tuple[Opcode, int]] = {
    'function?': (Opcode.FUNCTION_P, 1),
    'function=?': (Opcode.FUNCTION_EQ_P, 2),
    'function!=?': (Opcode.FUNCTION_NEQ_P, 2),
    'function-min-arity': (Opcode.FUNCTION_MIN_ARITY, 1),
    'function-variadic?': (Opcode.FUNCTION_VARIADIC_P, 1),
    'function-accepts?': (Opcode.FUNCTION_ACCEPTS_P, 2),
    'symbol?': (Opcode.SYMBOL_P, 1),
    'symbol=?': (Opcode.SYMBOL_EQ_P, 2),
    'symbol!=?': (Opcode.SYMBOL_NEQ_P, 2),
    'symbol->string': (Opcode.SYMBOL_TO_STRING, 1),
    'none?': (Opcode.NONE_P, 1),
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
    'integer-abs': (Opcode.INTEGER_ABS, 1),
    'integer+': (Opcode.INTEGER_ADD, 2),
    'integer-': (Opcode.INTEGER_SUB, 2),
    'integer*': (Opcode.INTEGER_MUL, 2),
    'integer/': (Opcode.INTEGER_DIV, 2),
    'integer%': (Opcode.INTEGER_MOD, 2),
    'integer-neg': (Opcode.INTEGER_NEG, 1),
    'integer-expn': (Opcode.INTEGER_EXPN, 2),
    'integer-bit-not': (Opcode.INTEGER_BIT_NOT, 1),
    'integer-bit-shift-left': (Opcode.INTEGER_BIT_SHIFT_LEFT, 2),
    'integer-bit-shift-right': (Opcode.INTEGER_BIT_SHIFT_RIGHT, 2),
    'integer-bit-or': (Opcode.INTEGER_BIT_OR, 2),
    'integer-bit-and': (Opcode.INTEGER_BIT_AND, 2),
    'integer-bit-xor': (Opcode.INTEGER_BIT_XOR, 2),
    'integer-min': (Opcode.INTEGER_MIN, 2),
    'integer-max': (Opcode.INTEGER_MAX, 2),
    'integer->float': (Opcode.INTEGER_TO_FLOAT, 1),
    'integer->complex': (Opcode.INTEGER_TO_COMPLEX, 2),
    'integer->string': (Opcode.INTEGER_TO_STRING, 2),
    'float?': (Opcode.FLOAT_P, 1),
    'float=?': (Opcode.FLOAT_EQ_P, 2),
    'float!=?': (Opcode.FLOAT_NEQ_P, 2),
    'float<?': (Opcode.FLOAT_LT_P, 2),
    'float>?': (Opcode.FLOAT_GT_P, 2),
    'float<=?': (Opcode.FLOAT_LTE_P, 2),
    'float>=?': (Opcode.FLOAT_GTE_P, 2),
    'float-abs': (Opcode.FLOAT_ABS, 1),
    'float+': (Opcode.FLOAT_ADD, 2),
    'float-': (Opcode.FLOAT_SUB, 2),
    'float*': (Opcode.FLOAT_MUL, 2),
    'float/': (Opcode.FLOAT_DIV, 2),
    'float//': (Opcode.FLOAT_FLOOR_DIV, 2),
    'float%': (Opcode.FLOAT_MOD, 2),
    'float-neg': (Opcode.FLOAT_NEG, 1),
    'float-exp': (Opcode.FLOAT_EXP, 1),
    'float-expn': (Opcode.FLOAT_EXPN, 2),
    'float-log': (Opcode.FLOAT_LOG, 1),
    'float-log10': (Opcode.FLOAT_LOG10, 1),
    'float-log2': (Opcode.FLOAT_LOG2, 1),
    'float-logn': (Opcode.FLOAT_LOGN, 2),
    'float-sin': (Opcode.FLOAT_SIN, 1),
    'float-cos': (Opcode.FLOAT_COS, 1),
    'float-tan': (Opcode.FLOAT_TAN, 1),
    'float-sqrt': (Opcode.FLOAT_SQRT, 1),
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
    'complex-abs': (Opcode.COMPLEX_ABS, 1),
    'complex+': (Opcode.COMPLEX_ADD, 2),
    'complex-': (Opcode.COMPLEX_SUB, 2),
    'complex*': (Opcode.COMPLEX_MUL, 2),
    'complex/': (Opcode.COMPLEX_DIV, 2),
    'complex-neg': (Opcode.COMPLEX_NEG, 1),
    'complex-exp': (Opcode.COMPLEX_EXP, 1),
    'complex-expn': (Opcode.COMPLEX_EXPN, 2),
    'complex-log': (Opcode.COMPLEX_LOG, 1),
    'complex-log10': (Opcode.COMPLEX_LOG10, 1),
    'complex-logn': (Opcode.COMPLEX_LOGN, 2),
    'complex-sin': (Opcode.COMPLEX_SIN, 1),
    'complex-cos': (Opcode.COMPLEX_COS, 1),
    'complex-tan': (Opcode.COMPLEX_TAN, 1),
    'complex-sqrt': (Opcode.COMPLEX_SQRT, 1),
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
    'string->integer': (Opcode.STRING_TO_INTEGER, 2),
    'string->number': (Opcode.STRING_TO_NUMBER, 1),
    'string->list': (Opcode.STRING_TO_LIST, 2),
    'string-ref': (Opcode.STRING_REF, 2),
    'string-index': (Opcode.STRING_INDEX, 2),
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
    'list-index': (Opcode.LIST_INDEX, 2),
    'list-slice': (Opcode.LIST_SLICE, 3),
    'list-remove': (Opcode.LIST_REMOVE, 2),
    'list-concat': (Opcode.LIST_CONCAT, 2),
    'list->string': (Opcode.LIST_TO_STRING, 2),
    'dict?': (Opcode.DICT_P, 1),
    'dict=?': (Opcode.DICT_EQ_P, 2),
    'dict!=?': (Opcode.DICT_NEQ_P, 2),
    'dict-keys': (Opcode.DICT_KEYS, 1),
    'dict-values': (Opcode.DICT_VALUES, 1),
    'dict-length': (Opcode.DICT_LENGTH, 1),
    'dict-has?': (Opcode.DICT_HAS_P, 2),
    'dict-remove': (Opcode.DICT_REMOVE, 2),
    'dict-merge': (Opcode.DICT_MERGE, 2),
    'dict-set': (Opcode.DICT_SET, 3),
    'dict-get': (Opcode.DICT_GET, 3),
    'range': (Opcode.RANGE, 3),
}

@dataclass
class Instruction:
    """Single bytecode instruction.

    Stores opcode and arguments for easier debugging and manipulation.

    Register fields:
      dest  — destination register written by this instruction (0 if unused)
      src0  — first source register or instruction-stream immediate
      src1  — second source register or instruction-stream immediate
      src2  — third source register (unused in current stack-machine phase)

    Opcode conventions:
      PUSH src0                        — push register src0 onto call stack; dest unused
      POP  dest                        — pop call stack into register dest; src0/src1/src2 unused
      CALL dest, src0                  — dest=result reg; src0=arity; pops func+args from stack
      TAIL_CALL src0                   — src0=arity; no dest (result propagates via trampoline)
      APPLY dest                       — dest=result reg; pops func+arg_list from stack
      TAIL_APPLY                       — no dest (result propagates via trampoline)
      RETURN src0                      — src0=register holding return value; pushes it for caller
      MAKE_CLOSURE dest, src0, src1    — dest=result reg; src0=code_idx; src1=capture_count
      PATCH_CLOSURE src0, src1, src2   — src0=closure reg; src1=value reg; src2=capture_idx
      EMIT_TRACE src0                  — src0=value register to trace; no dest
      All remaining stack-machine ops  — src0/src1 carry stream immediates; dest unused
    """
    opcode: Opcode
    dest: int = 0   # destination register (written by MAKE_CLOSURE, POP, and load ops)
    src0: int = 0   # first immediate / source operand (was arg1)
    src1: int = 0   # second immediate / source operand (was arg2)
    src2: int = 0   # third source operand (used by PATCH_CLOSURE for capture_idx)

    def arg_count(self) -> int:
        """Return the number of instruction-stream immediates this instruction takes (0, 1, or 2).

        For PUSH: 1 (src0 is the register index, encoded in the stream).
        For POP:  0 (dest is the register index, encoded in the stream as dest, not src0).
        For most other ops: 0, 1, or 2 as before.
        """
        return self.opcode.arg_count()

    def __repr__(self) -> str:
        """Human-readable representation using register-ISA disassembly style.

        Opcodes that write to a destination register are shown as:
            rN = OPCODE ...
        Opcodes that only consume or have no result are shown as:
            OPCODE ...

        All load ops (LOAD_NONE, LOAD_TRUE, LOAD_FALSE, LOAD_EMPTY_LIST,
        LOAD_CONST, LOAD_NAME) always write to dest.
        """
        opcode = self.opcode
        name = opcode.name

        # POP: dest = POP  (pops stack top into a register — dest is meaningful)
        if opcode == Opcode.POP:
            return f"r{self.dest} = POP"

        # PUSH: PUSH rN  (pushes a register onto the call stack — src0 is the register)
        if opcode == Opcode.PUSH:
            return f"PUSH r{self.src0}"

        if opcode in (Opcode.LOAD_NONE, Opcode.LOAD_TRUE,
                      Opcode.LOAD_FALSE, Opcode.LOAD_EMPTY_LIST):
            return f"r{self.dest} = {name}"

        if opcode == Opcode.LOAD_CONST:
            return f"r{self.dest} = LOAD_CONST {self.src0}"

        if opcode == Opcode.LOAD_NAME:
            return f"r{self.dest} = LOAD_NAME {self.src0}"

        if opcode == Opcode.JUMP_IF_FALSE:
            return f"JUMP_IF_FALSE r{self.src0}, @{self.src1}"

        if opcode == Opcode.JUMP_IF_TRUE:
            return f"JUMP_IF_TRUE r{self.src0}, @{self.src1}"

        if opcode == Opcode.MAKE_CLOSURE:
            return f"r{self.dest} = MAKE_CLOSURE {self.src0}, {self.src1}"

        if opcode == Opcode.PATCH_CLOSURE:
            return f"PATCH_CLOSURE r{self.src0}, r{self.src1}, {self.src2}"

        if opcode == Opcode.RETURN:
            return f"RETURN r{self.src0}"

        if opcode == Opcode.CALL:
            return f"r{self.dest} = CALL {self.src0}"

        if opcode == Opcode.TAIL_CALL:
            return f"TAIL_CALL {self.src0}"

        if opcode == Opcode.APPLY:
            return f"r{self.dest} = APPLY"

        if opcode == Opcode.TAIL_APPLY:
            return "TAIL_APPLY"

        if opcode == Opcode.EMIT_TRACE:
            return f"EMIT_TRACE r{self.src0}"

        n = self.arg_count()
        srcs = [f"r{self.src0}", f"r{self.src1}", f"r{self.src2}"][:n]
        src_str = (", ".join(srcs) + " " if srcs else "")
        return f"r{self.dest} = {name} {src_str}".rstrip()


@dataclass
class CodeObject:
    """Compiled code object containing bytecode and metadata.

    This represents a compiled Menai expression or function body.
    """

    # Bytecode instructions
    instructions: List[Instruction]

    # Constant pool (for LOAD_CONST)
    constants: List[MenaiValue]

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
