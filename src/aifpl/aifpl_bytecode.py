"""Bytecode definitions for AIFPL virtual machine."""

from dataclasses import dataclass, field
from enum import IntEnum, auto
from typing import List, Any

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
    LOAD_LOCAL = auto()      # Load local variable: LOAD_LOCAL depth index
    STORE_LOCAL = auto()     # Store local variable: STORE_LOCAL depth index
    LOAD_GLOBAL = auto()     # Load global variable: LOAD_GLOBAL name_index
    STORE_DEFERRED = auto()  # Store but mark as deferred (for recursive closures)

    # Arithmetic (specialized for numbers)
    ADD_NN = auto()          # Add two numbers from stack (no type check)
    SUB_NN = auto()          # Subtract two numbers
    MUL_NN = auto()          # Multiply two numbers
    DIV_NN = auto()          # Divide two numbers

    # Arithmetic (generic with type checking)
    ADD = auto()             # Generic add (handles multiple args from stack)
    SUB = auto()             # Generic subtract
    MUL = auto()             # Generic multiply
    DIV = auto()             # Generic divide

    # Comparisons (specialized)
    EQ_NN = auto()           # Compare two numbers for equality
    LT_NN = auto()           # Less than for numbers
    GT_NN = auto()           # Greater than for numbers
    LTE_NN = auto()          # Less than or equal for numbers
    GTE_NN = auto()          # Greater than or equal for numbers

    # Comparisons (generic)
    EQ = auto()              # Generic equality
    LT = auto()              # Generic less than
    GT = auto()              # Generic greater than
    LTE = auto()             # Generic less than or equal
    GTE = auto()             # Generic greater than or equal

    # Control flow
    JUMP = auto()            # Unconditional jump: JUMP offset
    JUMP_IF_FALSE = auto()   # Jump if top of stack is false: JUMP_IF_FALSE offset
    JUMP_IF_TRUE = auto()    # Jump if top of stack is true: JUMP_IF_TRUE offset
    POP_JUMP_IF_FALSE = auto()  # Pop and jump if false
    POP_JUMP_IF_TRUE = auto()   # Pop and jump if true

    # Functions
    MAKE_CLOSURE = auto()    # Create closure: MAKE_CLOSURE code_index capture_count
    CALL_FUNCTION = auto()   # Call function: CALL_FUNCTION arity
    CALL_BUILTIN = auto()    # Call builtin: CALL_BUILTIN builtin_index arity
    TAIL_CALL = auto()       # Tail call optimization: TAIL_CALL arity
    PATCH_CLOSURE_SELF = auto()  # Patch closure to reference itself: PATCH_CLOSURE_SELF depth index
    RETURN = auto()          # Return from function

    # Lists
    MAKE_LIST = auto()       # Make list from n stack items: MAKE_LIST n
    CONS = auto()            # Cons two items from stack
    LIST_FIRST = auto()      # Get first element of list
    LIST_REST = auto()       # Get rest of list

    # Stack manipulation
    POP = auto()             # Discard top of stack
    DUP = auto()             # Duplicate top of stack

    # Frames
    MAKE_FRAME = auto()      # Create new local frame: MAKE_FRAME size
    POP_FRAME = auto()       # Pop local frame


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
        if self.arg1 == 0 and self.arg2 == 0:
            return f"{self.opcode.name}"
        elif self.arg2 == 0:
            return f"{self.opcode.name} {self.arg1}"
        else:
            return f"{self.opcode.name} {self.arg1} {self.arg2}"


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
    param_count: int = 0      # Number of parameters (for functions)
    local_count: int = 0      # Number of local variables
    name: str = "<module>"    # Name for debugging

    def __repr__(self) -> str:
        """Human-readable representation."""
        lines = [f"CodeObject: {self.name}"]
        lines.append(f"  Parameters: {self.param_count}")
        lines.append(f"  Locals: {self.local_count}")
        lines.append(f"  Constants: {len(self.constants)}")
        lines.append(f"  Names: {self.names}")
        lines.append(f"  Instructions:")
        for i, instr in enumerate(self.instructions):
            lines.append(f"    {i:3d}: {instr}")
        return "\n".join(lines)

    def disassemble(self) -> str:
        """Return disassembled bytecode for debugging."""
        return repr(self)


def make_instruction(opcode: Opcode, arg1: int = 0, arg2: int = 0) -> Instruction:
    """Helper to create instruction."""
    return Instruction(opcode, arg1, arg2)
