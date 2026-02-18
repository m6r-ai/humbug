"""Bytecode definitions for AIFPL virtual machine."""

from dataclasses import dataclass, field
from enum import IntEnum, auto
from typing import List

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
    CALL_BUILTIN = auto()    # Call builtin: CALL_BUILTIN builtin_index arity
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
    STRING_LENGTH = auto()       # Get length of string
    STRING_UPCASE = auto()       # Convert string to uppercase
    STRING_DOWNCASE = auto()     # Convert string to lowercase
    STRING_TRIM = auto()         # Trim whitespace from string
    STRING_TO_NUMBER = auto()    # Parse string to number
    NUMBER_TO_STRING = auto()    # Convert number to string
    STRING_TO_LIST = auto()      # Convert string to list of characters
    LIST_TO_STRING = auto()      # Convert list of characters to string
    STRING_REF = auto()          # Get character at index
    STRING_CONTAINS_P = auto()   # Check if string contains substring
    STRING_PREFIX_P = auto()     # Check if string has prefix
    STRING_SUFFIX_P = auto()     # Check if string has suffix
    STRING_SPLIT = auto()        # Split string by delimiter
    STRING_JOIN = auto()         # Join list of strings with separator
    SUBSTRING = auto()           # Extract substring (string, start, end)
    STRING_REPLACE = auto()      # Replace substring (string, old, new)

    # Alist operations
    ALIST_KEYS = auto()      # Get all keys from alist
    ALIST_VALUES = auto()    # Get all values from alist
    ALIST_LENGTH = auto()    # Get number of entries in alist
    ALIST_HAS_P = auto()     # Check if alist has key
    ALIST_REMOVE = auto()    # Remove key from alist
    ALIST_MERGE = auto()     # Merge two alists
    ALIST_SET = auto()       # Set key in alist (alist, key, value)

@dataclass
class Instruction:
    """Single bytecode instruction.

    Stores opcode and arguments for easier debugging and manipulation.
    In the VM, we'll use a more compact representation.
    """
    opcode: Opcode
    arg1: int = 0
    arg2: int = 0

    def __repr__(self) -> str:
        """Human-readable representation."""
        # Define which opcodes take 0, 1, or 2 arguments
        # This is more reliable than checking if arguments are zero,
        # since zero is a valid argument value (e.g., LOAD_CONST 0)

        # Opcodes with no arguments
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
            Opcode.ALIST_HAS_P, Opcode.ALIST_REMOVE, Opcode.ALIST_MERGE, Opcode.ALIST_SET,
        }

        # Opcodes with two arguments
        two_arg_opcodes = {
            Opcode.MAKE_CLOSURE,
            Opcode.CALL_BUILTIN, Opcode.LOAD_PARENT_VAR
        }

        # All other opcodes take one argument (LOAD_CONST, LOAD_VAR, STORE_VAR,
        # LOAD_NAME, JUMP, JUMP_IF_FALSE, JUMP_IF_TRUE, RAISE_ERROR,
        # CALL_FUNCTION, TAIL_CALL_FUNCTION)

        if self.opcode in no_arg_opcodes:
            return f"{self.opcode.name}"

        if self.opcode in two_arg_opcodes:
            return f"{self.opcode.name} {self.arg1} {self.arg2}"

        # One-argument opcodes (LOAD_CONST, LOAD_NAME, JUMP, POP_JUMP_IF_FALSE,
        # POP_JUMP_IF_TRUE, RAISE_ERROR, CALL_FUNCTION)
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
    param_count: int = 0  # Number of parameters (for functions)
    local_count: int = 0  # Number of local variables
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
