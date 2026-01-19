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
    LOAD_VAR = auto()        # Load variable by position: LOAD_VAR depth index
    STORE_VAR = auto()       # Store variable by position: STORE_VAR depth index
    LOAD_NAME = auto()       # Load by name lookup: LOAD_NAME name_index

    # Control flow
    JUMP = auto()            # Unconditional jump: JUMP offset
    POP_JUMP_IF_FALSE = auto()  # Pop and jump if false
    POP_JUMP_IF_TRUE = auto()   # Pop and jump if true

    # Functions
    MAKE_CLOSURE = auto()    # Create closure: MAKE_CLOSURE code_index capture_count
    CALL_FUNCTION = auto()   # Call function: CALL_FUNCTION arity
    CALL_BUILTIN = auto()    # Call builtin: CALL_BUILTIN builtin_index arity
    PATCH_CLOSURE_SELF = auto()  # Patch closure to reference itself: PATCH_CLOSURE_SELF depth index
    PATCH_CLOSURE_SIBLING = auto()  # Patch closure to add sibling reference: PATCH_CLOSURE_SIBLING closure_idx sibling_idx name_idx
    RETURN = auto()          # Return from function

    # Lists
    MAKE_LIST = auto()       # Make list from n stack items: MAKE_LIST n

    # Stack manipulation
    POP = auto()             # Discard top of stack

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
