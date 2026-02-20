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
    CALL_FUNCTION = auto()   # Call function: CALL_FUNCTION arity
    TAIL_CALL_FUNCTION = auto()  # Tail call function: TAIL_CALL_FUNCTION arity
    RETURN = auto()          # Return from function

    # Debugging
    EMIT_TRACE = auto()      # Emit trace: EMIT_TRACE (pops value, emits to watcher)

    # Arithmetic operations
    ADD = auto()             # Calculate a + b
    SUB = auto()             # Calculate a - b
    MUL = auto()             # Calculate a * b
    DIV = auto()             # Calculate a / b
    FLOOR_DIV = auto()       # Calculate a // b (floor division)
    MOD = auto()             # Calculate a % b (modulo)
    STAR_STAR = auto()       # Calculate a ** b (complex-capable exponentiation)

    # Type predicates
    NUMBER_P = auto()        # Check if number (int/float/complex)
    INTEGER_P = auto()       # Check if integer
    FLOAT_P = auto()         # Check if float
    COMPLEX_P = auto()       # Check if complex
    STRING_P = auto()        # Check if string
    BOOLEAN_P = auto()       # Check if boolean
    LIST_P = auto()          # Check if list
    ALIST_P = auto()         # Check if alist
    FUNCTION_P = auto()      # Check if function

    # Floating point operations
    SIN = auto()             # Calculate sin(x)
    COS = auto()             # Calculate cos(x)
    TAN = auto()             # Calculate tan(x)
    LOG = auto()             # Calculate log(x)
    LOG10 = auto()           # Calculate log10(x)
    EXP = auto()             # Calculate exp(x)
    POW = auto()             # Calculate x ** y
    SQRT = auto()            # Calculate sqrt(x)
    ABS = auto()             # Calculate abs(x)
    FLOOR = auto()           # Calculate floor(x)
    CEIL = auto()            # Calculate ceil(x)
    ROUND = auto()           # Calculate round(x)

    # Numeric conversion operations
    TO_INTEGER = auto()      # Convert to integer
    TO_FLOAT = auto()        # Convert to float
    REAL = auto()            # Extract real part
    IMAG = auto()            # Extract imaginary part
    MAKE_COMPLEX = auto()    # Construct complex from real and imaginary parts
    BIN = auto()             # Convert integer to binary string
    HEX = auto()             # Convert integer to hex string
    OCT = auto()             # Convert integer to octal string

    # Boolean operations
    NOT = auto()             # Logical NOT

    # Bitwise operations
    BIT_NOT = auto()         # Bitwise NOT ~x
    BIT_SHIFT_LEFT = auto()  # Bitwise left shift x << n
    BIT_SHIFT_RIGHT = auto() # Bitwise right shift x >> n

    # List operations
    CONS = auto()
    REVERSE = auto()         # Reverse list on top of stack
    FIRST = auto()           # Get first element of list
    REST = auto()            # Get rest of list (all but first element)
    LAST = auto()            # Get last element of list
    LENGTH = auto()          # Get length of list
    LIST_REF = auto()        # Get element at index from list: LIST_REF index
    NULL_P = auto()          # Check if list is empty
    MEMBER_P = auto()        # Check if item is in list
    POSITION = auto()        # Find index of item in list
    TAKE = auto()            # Take first n elements from list
    DROP = auto()            # Drop first n elements from list
    REMOVE = auto()          # Remove all occurrences of item from list

    # String operations
    STRING_LENGTH = auto()   # Get length of string
    STRING_UPCASE = auto()   # Convert string to uppercase
    STRING_DOWNCASE = auto() # Convert string to lowercase
    STRING_TRIM = auto()     # Trim whitespace from string
    STRING_TO_NUMBER = auto()
                             # Parse string to number
    NUMBER_TO_STRING = auto()
                             # Convert number to string
    STRING_TO_LIST = auto()  # Convert string to list of characters
    LIST_TO_STRING = auto()  # Convert list of characters to string
    STRING_REF = auto()      # Get character at index
    STRING_CONTAINS_P = auto()
                             # Check if string contains substring
    STRING_PREFIX_P = auto() # Check if string has prefix
    STRING_SUFFIX_P = auto() # Check if string has suffix
    STRING_SPLIT = auto()    # Split string by delimiter
    STRING_JOIN = auto()     # Join list of strings with separator
    SUBSTRING = auto()       # Extract substring (string, start, end)
    STRING_REPLACE = auto()  # Replace substring (string, old, new)

    # Alist operations
    ALIST_KEYS = auto()      # Get all keys from alist
    ALIST_VALUES = auto()    # Get all values from alist
    ALIST_LENGTH = auto()    # Get number of entries in alist
    ALIST_HAS_P = auto()     # Check if alist has key
    ALIST_REMOVE = auto()    # Remove key from alist
    ALIST_MERGE = auto()     # Merge two alists
    ALIST_GET = auto()       # Get value from alist by key with default: (alist-get alist key default)
    ALIST_SET = auto()       # Set key in alist (alist, key, value)

    # Fold-reducible variadic operations (desugared to binary by desugarer)
    BIT_OR = auto()          # Bitwise OR: a | b
    BIT_AND = auto()         # Bitwise AND: a & b
    BIT_XOR = auto()         # Bitwise XOR: a ^ b
    APPEND = auto()          # Append two lists: (append a b)
    STRING_APPEND = auto()   # Concatenate two strings: (string-append a b)
    MIN = auto()             # Minimum of two real numbers
    MAX = auto()             # Maximum of two real numbers

    # Comparison / equality operations (desugared to binary by desugarer)
    EQ = auto()              # a = b
    NEQ = auto()             # a != b
    LT = auto()              # a < b
    GT = auto()              # a > b
    LTE = auto()             # a <= b
    GTE = auto()             # a >= b
    STRING_EQ_P = auto()     # string=? a b
    NUMBER_EQ_P = auto()     # number=? a b
    INTEGER_EQ_P = auto()    # integer=? a b
    FLOAT_EQ_P = auto()      # float=? a b
    COMPLEX_EQ_P = auto()    # complex=? a b
    BOOLEAN_EQ_P = auto()    # boolean=? a b
    LIST_EQ_P = auto()       # list=? a b
    ALIST_EQ_P = auto()      # alist=? a b

    # Collection construction opcodes (variadic, count encoded in instruction)
    BUILD_LIST = auto()      # BUILD_LIST n  — pop n values, push AIFPLList
    BUILD_ALIST = auto()     # BUILD_ALIST n — pop n (list key value) pair-lists, push AIFPLAList
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
    'number?': (Opcode.NUMBER_P, 1),
    'integer?': (Opcode.INTEGER_P, 1),
    'float?': (Opcode.FLOAT_P, 1),
    'complex?': (Opcode.COMPLEX_P, 1),
    'string?': (Opcode.STRING_P, 1),
    'boolean?': (Opcode.BOOLEAN_P, 1),
    'list?': (Opcode.LIST_P, 1),
    'alist?': (Opcode.ALIST_P, 1),
    'function?': (Opcode.FUNCTION_P, 1),
    'not': (Opcode.NOT, 1),
    'sin': (Opcode.SIN, 1),
    'cos': (Opcode.COS, 1),
    'tan': (Opcode.TAN, 1),
    'log': (Opcode.LOG, 1),
    'log10': (Opcode.LOG10, 1),
    'exp': (Opcode.EXP, 1),
    'sqrt': (Opcode.SQRT, 1),
    'abs': (Opcode.ABS, 1),
    'floor': (Opcode.FLOOR, 1),
    'ceil': (Opcode.CEIL, 1),
    'round': (Opcode.ROUND, 1),
    'integer': (Opcode.TO_INTEGER, 1),
    'float': (Opcode.TO_FLOAT, 1),
    'real': (Opcode.REAL, 1),
    'imag': (Opcode.IMAG, 1),
    'bin': (Opcode.BIN, 1),
    'hex': (Opcode.HEX, 1),
    'oct': (Opcode.OCT, 1),
    'bit-not': (Opcode.BIT_NOT, 1),
    'reverse': (Opcode.REVERSE, 1),
    'first': (Opcode.FIRST, 1),
    'rest': (Opcode.REST, 1),
    'last': (Opcode.LAST, 1),
    'length': (Opcode.LENGTH, 1),
    'null?': (Opcode.NULL_P, 1),
    'string-length': (Opcode.STRING_LENGTH, 1),
    'string-upcase': (Opcode.STRING_UPCASE, 1),
    'string-downcase': (Opcode.STRING_DOWNCASE, 1),
    'string-trim': (Opcode.STRING_TRIM, 1),
    'string->number': (Opcode.STRING_TO_NUMBER, 1),
    'number->string': (Opcode.NUMBER_TO_STRING, 1),
    'string->list': (Opcode.STRING_TO_LIST, 1),
    'list->string': (Opcode.LIST_TO_STRING, 1),
    'alist-keys': (Opcode.ALIST_KEYS, 1),
    'alist-values': (Opcode.ALIST_VALUES, 1),
    'alist-length': (Opcode.ALIST_LENGTH, 1),
    '+': (Opcode.ADD, 2),
    '-': (Opcode.SUB, 2),
    '*': (Opcode.MUL, 2),
    '/': (Opcode.DIV, 2),
    '//': (Opcode.FLOOR_DIV, 2),
    '%': (Opcode.MOD, 2),
    '**': (Opcode.STAR_STAR, 2),
    'pow': (Opcode.POW, 2),
    'bit-or': (Opcode.BIT_OR, 2),
    'bit-and': (Opcode.BIT_AND, 2),
    'bit-xor': (Opcode.BIT_XOR, 2),
    'bit-shift-left': (Opcode.BIT_SHIFT_LEFT, 2),
    'bit-shift-right': (Opcode.BIT_SHIFT_RIGHT, 2),
    '=': (Opcode.EQ, 2),
    '!=': (Opcode.NEQ, 2),
    '<': (Opcode.LT, 2),
    '>': (Opcode.GT, 2),
    '<=': (Opcode.LTE, 2),
    '>=': (Opcode.GTE, 2),
    'string=?': (Opcode.STRING_EQ_P, 2),
    'number=?': (Opcode.NUMBER_EQ_P, 2),
    'integer=?': (Opcode.INTEGER_EQ_P, 2),
    'float=?': (Opcode.FLOAT_EQ_P, 2),
    'complex=?': (Opcode.COMPLEX_EQ_P, 2),
    'boolean=?': (Opcode.BOOLEAN_EQ_P, 2),
    'list=?': (Opcode.LIST_EQ_P, 2),
    'alist=?': (Opcode.ALIST_EQ_P, 2),
    'cons': (Opcode.CONS, 2),
    'append': (Opcode.APPEND, 2),
    'list-ref': (Opcode.LIST_REF, 2),
    'member?': (Opcode.MEMBER_P, 2),
    'position': (Opcode.POSITION, 2),
    'take': (Opcode.TAKE, 2),
    'drop': (Opcode.DROP, 2),
    'remove': (Opcode.REMOVE, 2),
    'string-ref': (Opcode.STRING_REF, 2),
    'string-contains?': (Opcode.STRING_CONTAINS_P, 2),
    'string-prefix?': (Opcode.STRING_PREFIX_P, 2),
    'string-suffix?': (Opcode.STRING_SUFFIX_P, 2),
    'string-split': (Opcode.STRING_SPLIT, 2),
    'string-join': (Opcode.STRING_JOIN, 2),
    'string-append': (Opcode.STRING_APPEND, 2),
    'alist-has?': (Opcode.ALIST_HAS_P, 2),
    'alist-remove': (Opcode.ALIST_REMOVE, 2),
    'alist-merge': (Opcode.ALIST_MERGE, 2),
    'min': (Opcode.MIN, 2),
    'max': (Opcode.MAX, 2),
    'complex': (Opcode.MAKE_COMPLEX, 2),
    'substring': (Opcode.SUBSTRING, 3),
    'string-replace': (Opcode.STRING_REPLACE, 3),
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
            Opcode.LOAD_TRUE, Opcode.LOAD_FALSE, Opcode.LOAD_EMPTY_LIST, Opcode.RETURN, Opcode.EMIT_TRACE,
            Opcode.ADD, Opcode.SUB, Opcode.MUL, Opcode.DIV,
            Opcode.FLOOR_DIV, Opcode.MOD, Opcode.STAR_STAR,
            Opcode.NUMBER_P, Opcode.INTEGER_P, Opcode.FLOAT_P, Opcode.COMPLEX_P,
            Opcode.STRING_P, Opcode.BOOLEAN_P, Opcode.LIST_P, Opcode.ALIST_P, Opcode.FUNCTION_P,
            Opcode.NOT, Opcode.BIT_NOT, Opcode.BIT_SHIFT_LEFT, Opcode.BIT_SHIFT_RIGHT,
            Opcode.SIN, Opcode.COS, Opcode.TAN, Opcode.LOG, Opcode.LOG10, Opcode.EXP,
            Opcode.POW, Opcode.SQRT, Opcode.ABS, Opcode.FLOOR, Opcode.CEIL, Opcode.ROUND,
            Opcode.TO_INTEGER, Opcode.TO_FLOAT, Opcode.REAL, Opcode.IMAG, Opcode.MAKE_COMPLEX,
            Opcode.BIN, Opcode.HEX, Opcode.OCT,
            Opcode.CONS, Opcode.REVERSE, Opcode.FIRST, Opcode.REST, Opcode.LAST,
            Opcode.LENGTH, Opcode.LIST_REF, Opcode.NULL_P, Opcode.MEMBER_P, Opcode.POSITION,
            Opcode.TAKE, Opcode.DROP, Opcode.REMOVE,
            Opcode.STRING_LENGTH, Opcode.STRING_UPCASE, Opcode.STRING_DOWNCASE, Opcode.STRING_TRIM,
            Opcode.STRING_TO_NUMBER, Opcode.NUMBER_TO_STRING, Opcode.STRING_TO_LIST, Opcode.LIST_TO_STRING,
            Opcode.STRING_REF, Opcode.STRING_CONTAINS_P, Opcode.STRING_PREFIX_P, Opcode.STRING_SUFFIX_P,
            Opcode.STRING_SPLIT, Opcode.STRING_JOIN, Opcode.SUBSTRING, Opcode.STRING_REPLACE,
            Opcode.ALIST_KEYS, Opcode.ALIST_VALUES, Opcode.ALIST_LENGTH,
            Opcode.ALIST_HAS_P, Opcode.ALIST_REMOVE, Opcode.ALIST_MERGE, Opcode.ALIST_GET, Opcode.ALIST_SET,
            Opcode.RANGE,
            Opcode.BIT_OR, Opcode.BIT_AND, Opcode.BIT_XOR,
            Opcode.APPEND, Opcode.STRING_APPEND, Opcode.MIN, Opcode.MAX,
            Opcode.EQ, Opcode.NEQ, Opcode.LT, Opcode.GT, Opcode.LTE, Opcode.GTE,
            Opcode.STRING_EQ_P,
            Opcode.NUMBER_EQ_P, Opcode.INTEGER_EQ_P, Opcode.FLOAT_EQ_P, Opcode.COMPLEX_EQ_P,
            Opcode.BOOLEAN_EQ_P, Opcode.LIST_EQ_P, Opcode.ALIST_EQ_P,
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
