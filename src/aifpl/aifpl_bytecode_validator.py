"""
Bytecode validator for AIFPL virtual machine.

This validator performs static analysis on bytecode to ensure it's well-formed
and safe to execute. By validating once before execution, we can remove many
runtime checks from the hot VM execution loop.

The validator checks:
- Structural invariants (valid jumps, indices in bounds)
- Stack depth consistency across all paths
- Variable access validity
- Control flow correctness (all paths return)
- Function/closure well-formedness
"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional, Tuple, Set
from enum import Enum

from aifpl.aifpl_bytecode import CodeObject, Instruction, Opcode
from aifpl.aifpl_builtin_registry import AIFPLBuiltinRegistry


class ValidationErrorType(Enum):
    """Types of validation errors."""
    INVALID_JUMP_TARGET = "invalid_jump_target"
    INDEX_OUT_OF_BOUNDS = "index_out_of_bounds"
    STACK_UNDERFLOW = "stack_underflow"
    STACK_INCONSISTENT = "stack_inconsistent"
    MISSING_RETURN = "missing_return"
    INVALID_OPCODE = "invalid_opcode"
    UNREACHABLE_CODE = "unreachable_code"
    INVALID_VARIABLE_ACCESS = "invalid_variable_access"
    UNINITIALIZED_VARIABLE = "uninitialized_variable"


@dataclass
class ValidationError(Exception):
    """Bytecode validation error with detailed context."""
    error_type: ValidationErrorType
    message: str
    instruction_index: Optional[int] = None
    opcode: Optional[Opcode] = None
    context: Optional[str] = None

    def __str__(self) -> str:
        parts = [f"Bytecode validation error: {self.message}"]
        if self.instruction_index is not None:
            parts.append(f"  at instruction {self.instruction_index}")

        if self.opcode is not None:
            parts.append(f"  opcode: {self.opcode.name}")

        if self.context:
            parts.append(f"  context: {self.context}")
        return "\n".join(parts)


@dataclass
class StackState:
    """Represents stack depth at a program point.

    We track min/max depth to handle different paths through the code.
    """
    depth: int

    def __repr__(self) -> str:
        return f"Stack({self.depth})"


@dataclass
class BasicBlock:
    """A basic block in the control flow graph."""
    start_index: int
    end_index: int  # Inclusive
    successors: List[int] = field(default_factory=list)  # Instruction indices
    predecessors: List[int] = field(default_factory=list)  # Instruction indices
    stack_depth_in: Optional[int] = None  # Stack depth on entry
    stack_depth_out: Optional[int] = None  # Stack depth on exit
    visited: bool = False


class BytecodeValidator:
    """
    Validates AIFPL bytecode for correctness and safety.

    This performs static analysis to catch errors before execution,
    enabling the VM to remove redundant runtime checks.
    """

    def __init__(self) -> None:
        """Initialize validator."""
        # Get builtin count for validation
        self.builtin_count = len(AIFPLBuiltinRegistry.BUILTIN_TABLE)

        # Track which opcodes affect stack depth and how
        self._init_opcode_metadata()

    def _init_opcode_metadata(self) -> None:
        """Initialize metadata about opcodes for validation."""
        # Stack effect: how many items are popped (-) and pushed (+)
        # Format: (pop_count, push_count)
        self.stack_effects: Dict[Opcode, Tuple[int, int]] = {
            # Constants - push 1
            Opcode.LOAD_CONST: (0, 1),
            Opcode.LOAD_TRUE: (0, 1),
            Opcode.LOAD_FALSE: (0, 1),
            Opcode.LOAD_EMPTY_LIST: (0, 1),

            # Variables - load pushes 1, store pops 1
            Opcode.LOAD_VAR: (0, 1),
            Opcode.STORE_VAR: (1, 0),
            Opcode.LOAD_PARENT_VAR: (0, 1),
            Opcode.LOAD_NAME: (0, 1),

            # Control flow - jumps don't affect stack, conditionals pop 1
            Opcode.JUMP: (0, 0),
            Opcode.JUMP_IF_FALSE: (1, 0),
            Opcode.JUMP_IF_TRUE: (1, 0),
            Opcode.RAISE_ERROR: (0, 0),  # Doesn't return, but doesn't matter

            # Functions - MAKE_CLOSURE pops captures and pushes closure
            # CALL_* effects depend on arity (handled specially)
            # RETURN pops 1 (handled specially as it exits)
            Opcode.MAKE_CLOSURE: (-1, 1),
            Opcode.CALL_FUNCTION: (-1, 1),
            Opcode.TAIL_CALL_FUNCTION: (-1, 0),
            Opcode.CALL_BUILTIN: (-1, 1),
            Opcode.RETURN: (1, 0),

            # Trace debug
            Opcode.EMIT_TRACE: (1, 0),  # Pops 1 (message), pushes 0

            # Primitive arithmetic operations (binary)
            Opcode.ADD: (2, 1),
            Opcode.SUB: (2, 1),
            Opcode.MUL: (2, 1),
            Opcode.DIV: (2, 1),

            # Type predicate operations (unary)
            Opcode.NUMBER_P: (1, 1),
            Opcode.INTEGER_P: (1, 1),
            Opcode.FLOAT_P: (1, 1),
            Opcode.COMPLEX_P: (1, 1),
            Opcode.STRING_P: (1, 1),
            Opcode.BOOLEAN_P: (1, 1),
            Opcode.LIST_P: (1, 1),
            Opcode.ALIST_P: (1, 1),
            Opcode.FUNCTION_P: (1, 1),

            # Boolean operations
            Opcode.NOT: (1, 1),

            # Floating point operations (unary or binary)
            Opcode.SIN: (1, 1),
            Opcode.COS: (1, 1),
            Opcode.TAN: (1, 1),
            Opcode.EXP: (1, 1),
            Opcode.POW: (2, 1),
            Opcode.LOG: (1, 1),
            Opcode.LOG10: (1, 1),
            Opcode.SQRT: (1, 1),
        }

    def validate(self, code: CodeObject) -> None:
        """
        Validate a code object.

        Raises ValidationError if bytecode is invalid.

        Args:
            code: Code object to validate
        """
        # First validate all nested code objects recursively
        for nested_code in code.code_objects:
            self.validate(nested_code)

        # Validate this code object
        self._validate_structure(code)
        self._validate_indices(code)
        self._validate_control_flow(code)
        self._validate_stack_depth(code)
        self._validate_initialization(code)

    def _validate_structure(self, code: CodeObject) -> None:
        """Validate basic structural properties."""
        # Must have at least one instruction
        if not code.instructions:
            raise ValidationError(
                ValidationErrorType.INVALID_OPCODE,
                "Code object has no instructions"
            )

        # Check all opcodes are valid
        for i, instr in enumerate(code.instructions):
            if not isinstance(instr.opcode, Opcode):
                raise ValidationError(
                    ValidationErrorType.INVALID_OPCODE,
                    f"Invalid opcode type: {type(instr.opcode)}",
                    instruction_index=i
                )

    def _validate_indices(self, code: CodeObject) -> None:
        """Validate all indices (constants, names, code objects, variables)."""
        for i, instr in enumerate(code.instructions):
            opcode = instr.opcode

            # Validate constant pool indices
            if opcode in (Opcode.LOAD_CONST, Opcode.RAISE_ERROR):
                const_index = instr.arg1
                if const_index < 0 or const_index >= len(code.constants):
                    raise ValidationError(
                        ValidationErrorType.INDEX_OUT_OF_BOUNDS,
                        f"Constant index {const_index} out of bounds (pool size: {len(code.constants)})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate name pool indices
            if opcode == Opcode.LOAD_NAME:
                name_index = instr.arg1
                if name_index < 0 or name_index >= len(code.names):
                    raise ValidationError(
                        ValidationErrorType.INDEX_OUT_OF_BOUNDS,
                        f"Name index {name_index} out of bounds (pool size: {len(code.names)})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate code object indices
            if opcode == Opcode.MAKE_CLOSURE:
                code_index = instr.arg1
                if code_index < 0 or code_index >= len(code.code_objects):
                    raise ValidationError(
                        ValidationErrorType.INDEX_OUT_OF_BOUNDS,
                        f"Code object index {code_index} out of bounds (pool size: {len(code.code_objects)})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate variable indices (must be < local_count)
            if opcode in (Opcode.LOAD_VAR, Opcode.STORE_VAR):
                var_index = instr.arg1
                if var_index < 0 or var_index >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"Variable index {var_index} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate LOAD_PARENT_VAR indices
            if opcode == Opcode.LOAD_PARENT_VAR:
                var_index = instr.arg1
                depth = instr.arg2
                # Depth must be at least 1 (parent frame)
                if depth < 1:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"LOAD_PARENT_VAR depth must be >= 1, got {depth}",
                        instruction_index=i,
                        opcode=opcode
                    )
                # We can't validate the exact depth without runtime context,
                # but we can check the index is reasonable
                if var_index < 0:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"LOAD_PARENT_VAR index must be >= 0, got {var_index}",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate builtin indices
            if opcode == Opcode.CALL_BUILTIN:
                builtin_index = instr.arg1
                if builtin_index < 0 or builtin_index >= self.builtin_count:
                    raise ValidationError(
                        ValidationErrorType.INDEX_OUT_OF_BOUNDS,
                        f"Builtin index {builtin_index} out of bounds (builtin count: {self.builtin_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate jump targets
            if opcode in (Opcode.JUMP, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE):
                target = instr.arg1
                if target < 0 or target >= len(code.instructions):
                    raise ValidationError(
                        ValidationErrorType.INVALID_JUMP_TARGET,
                        f"Jump target {target} out of bounds (instruction count: {len(code.instructions)})",
                        instruction_index=i,
                        opcode=opcode
                    )

    def _validate_control_flow(self, code: CodeObject) -> None:
        """Validate control flow: all paths must end with RETURN or TAIL_CALL."""
        # Build control flow graph
        cfg = self._build_cfg(code)

        # Check that all reachable paths end with RETURN or TAIL_CALL
        # We do this by checking that every basic block either:
        # 1. Ends with RETURN or TAIL_CALL_FUNCTION
        # 2. Has successors
        for block in cfg.values():
            if not block.visited:
                continue  # Unreachable code, skip

            last_instr = code.instructions[block.end_index]

            # Check if block ends properly
            ends_properly = (
                last_instr.opcode in (Opcode.RETURN, Opcode.TAIL_CALL_FUNCTION, Opcode.RAISE_ERROR) or
                len(block.successors) > 0
            )

            if not ends_properly:
                raise ValidationError(
                    ValidationErrorType.MISSING_RETURN,
                    f"Control flow falls off end of block at instruction {block.end_index}",
                    instruction_index=block.end_index,
                    context=f"Block [{block.start_index}..{block.end_index}]"
                )

    def _validate_stack_depth(self, code: CodeObject) -> None:
        """
        Validate stack depth consistency across all paths.

        This performs abstract interpretation to track stack depth through
        all execution paths and ensures:
        1. No stack underflows
        2. Stack depth is consistent at merge points (jump targets)
        """
        # Build control flow graph
        _ = self._build_cfg(code)

        # Track stack depth at each instruction
        stack_depths: Dict[int, int] = {}

        # Worklist algorithm for dataflow analysis
        # For functions with parameters, the initial stack depth is param_count
        # (parameters are pushed by caller before entering function)
        initial_depth = code.param_count

        worklist: List[int] = [0]  # Start at instruction 0
        stack_depths[0] = initial_depth

        while worklist:
            instr_idx = worklist.pop(0)

            if instr_idx not in stack_depths:
                # This instruction is unreachable
                continue

            current_depth = stack_depths[instr_idx]
            instr = code.instructions[instr_idx]

            # Calculate stack effect of this instruction
            pop_count, push_count = self._get_stack_effect(instr)

            # Check for stack underflow
            if current_depth < pop_count:
                raise ValidationError(
                    ValidationErrorType.STACK_UNDERFLOW,
                    f"Stack underflow: depth={current_depth}, need={pop_count}",
                    instruction_index=instr_idx,
                    opcode=instr.opcode
                )

            # Calculate new stack depth
            new_depth = current_depth - pop_count + push_count

            # Find successors
            successors = self._get_successors(instr_idx, instr, code)

            # Propagate depth to successors
            for succ_idx in successors:
                if succ_idx in stack_depths:
                    # Already visited - check consistency
                    if stack_depths[succ_idx] != new_depth:
                        raise ValidationError(
                            ValidationErrorType.STACK_INCONSISTENT,
                            f"Inconsistent stack depth at merge point: "
                            f"expected {stack_depths[succ_idx]}, got {new_depth}",
                            instruction_index=succ_idx,
                            context=f"Predecessor at {instr_idx}"
                        )
                else:
                    # First time visiting - record depth and add to worklist
                    stack_depths[succ_idx] = new_depth
                    worklist.append(succ_idx)

    def _validate_initialization(self, code: CodeObject) -> None:
        """
        Validate that all variables are initialized before use.

        This performs definite assignment analysis to track which variables
        are guaranteed to be initialized at each program point.
        """
        # Track which variables are definitely initialized at each instruction
        # Maps instruction index -> set of initialized variable indices
        initialized_at: Dict[int, Set[int]] = {}

        # Initial state: for functions, parameters are initialized by caller
        # For closures, captured variables (free_vars) are initialized during closure creation
        # Captured variables are stored after parameters: indices [param_count, param_count + len(free_vars))
        initial_initialized = set(range(code.param_count))
        if code.free_vars:
            initial_initialized.update(range(code.param_count, code.param_count + len(code.free_vars)))

        # Worklist algorithm
        worklist: List[int] = [0]
        initialized_at[0] = initial_initialized.copy()

        while worklist:
            instr_idx = worklist.pop(0)

            if instr_idx not in initialized_at:
                # Unreachable
                continue

            current_initialized = initialized_at[instr_idx]
            instr = code.instructions[instr_idx]
            opcode = instr.opcode

            # Check LOAD_VAR - must be initialized
            if opcode == Opcode.LOAD_VAR:
                var_index = instr.arg1
                if var_index not in current_initialized:
                    raise ValidationError(
                        ValidationErrorType.UNINITIALIZED_VARIABLE,
                        f"Variable at index {var_index} may be uninitialized",
                        instruction_index=instr_idx,
                        opcode=opcode,
                        context=f"Initialized variables: {sorted(current_initialized)}"
                    )

            # Calculate new initialized set after this instruction
            new_initialized = current_initialized.copy()

            # STORE_VAR marks variable as initialized
            if opcode == Opcode.STORE_VAR:
                var_index = instr.arg1
                new_initialized.add(var_index)

            # Get successors
            successors = self._get_successors(instr_idx, instr, code)

            # Propagate to successors
            for succ_idx in successors:
                if succ_idx in initialized_at:
                    # Merge: only keep variables initialized on ALL paths
                    merged = initialized_at[succ_idx] & new_initialized
                    if merged != initialized_at[succ_idx]:
                        initialized_at[succ_idx] = merged
                        worklist.append(succ_idx)
                else:
                    # First time visiting
                    initialized_at[succ_idx] = new_initialized.copy()
                    worklist.append(succ_idx)

    def _get_stack_effect(self, instr: Instruction) -> Tuple[int, int]:
        """
        Get stack effect (pop_count, push_count) for an instruction.

        Returns:
            (pop_count, push_count) tuple
        """
        opcode = instr.opcode

        # Special cases that depend on arguments
        if opcode == Opcode.MAKE_CLOSURE:
            capture_count = instr.arg2
            return (capture_count, 1)  # Pop captures, push closure

        if opcode == Opcode.CALL_FUNCTION:
            arity = instr.arg1
            return (arity + 1, 1)  # Pop function + args, push result

        if opcode == Opcode.TAIL_CALL_FUNCTION:
            arity = instr.arg1
            return (arity + 1, 0)  # Pop function + args, tail position (no push)

        if opcode == Opcode.CALL_BUILTIN:
            arity = instr.arg2
            return (arity, 1)  # Pop args, push result

        # Default case
        return self.stack_effects.get(opcode, (0, 0))

    def _get_successors(self, instr_idx: int, instr: Instruction, code: CodeObject) -> List[int]:
        """Get successor instruction indices for an instruction."""
        opcode = instr.opcode

        # Terminal instructions have no successors
        if opcode in (Opcode.RETURN, Opcode.RAISE_ERROR):
            return []

        # Tail calls are terminal (they replace the frame)
        if opcode == Opcode.TAIL_CALL_FUNCTION:
            return []

        successors = []

        # Unconditional jump
        if opcode == Opcode.JUMP:
            successors.append(instr.arg1)

        # Conditional jumps have two successors
        elif opcode in (Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE):
            successors.append(instr.arg1)  # Jump target
            if instr_idx + 1 < len(code.instructions):
                successors.append(instr_idx + 1)  # Fall through

        # Regular instructions fall through
        else:
            if instr_idx + 1 < len(code.instructions):
                successors.append(instr_idx + 1)

        return successors

    def _build_cfg(self, code: CodeObject) -> Dict[int, BasicBlock]:
        """
        Build control flow graph.

        Returns a dict mapping block start indices to BasicBlock objects.
        This is a simplified CFG where we track reachability and successors.
        """
        # Find block boundaries (leaders)
        leaders = {0}  # First instruction is always a leader

        for i, instr in enumerate(code.instructions):
            # Jump targets are leaders
            if instr.opcode in (Opcode.JUMP, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE):
                leaders.add(instr.arg1)
                # Instruction after conditional jump is a leader
                if instr.opcode in (Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE):
                    if i + 1 < len(code.instructions):
                        leaders.add(i + 1)

            # Instruction after RETURN/RAISE_ERROR is a leader (if exists)
            if instr.opcode in (Opcode.RETURN, Opcode.RAISE_ERROR, Opcode.TAIL_CALL_FUNCTION):
                if i + 1 < len(code.instructions):
                    leaders.add(i + 1)

        # Create blocks
        leaders_list = sorted(leaders)
        blocks: Dict[int, BasicBlock] = {}

        for i, start in enumerate(leaders_list):
            end = leaders_list[i + 1] - 1 if i + 1 < len(leaders_list) else len(code.instructions) - 1
            blocks[start] = BasicBlock(start_index=start, end_index=end)

        # Build edges
        for start, block in blocks.items():
            last_instr = code.instructions[block.end_index]
            successors = self._get_successors(block.end_index, last_instr, code)

            # Find which blocks these successors belong to
            for succ_idx in successors:
                # Find the block containing succ_idx
                for block_start in sorted(blocks.keys(), reverse=True):
                    if block_start <= succ_idx:
                        block.successors.append(block_start)
                        blocks[block_start].predecessors.append(start)
                        break

        # Mark reachable blocks
        self._mark_reachable(blocks, 0)

        return blocks

    def _mark_reachable(self, blocks: Dict[int, BasicBlock], start: int) -> None:
        """Mark all reachable blocks starting from start."""
        if start not in blocks or blocks[start].visited:
            return

        blocks[start].visited = True
        for succ in blocks[start].successors:
            self._mark_reachable(blocks, succ)


def validate_bytecode(code: CodeObject) -> None:
    """
    Convenience function to validate bytecode.

    Args:
        code: Code object to validate

    Raises:
        ValidationError: If bytecode is invalid
    """
    validator = BytecodeValidator()
    validator.validate(code)
