"""
Bytecode validator for Menai virtual machine.

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
from typing import List, Dict, Tuple, Set
from enum import Enum

from menai.menai_bytecode import CodeObject, Instruction, Opcode


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
    instruction_index: int | None = None
    opcode: Opcode | None = None
    context: str | None = None

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
    stack_depth_in: int | None = None  # Stack depth on entry
    stack_depth_out: int | None = None  # Stack depth on exit
    visited: bool = False


class BytecodeValidator:
    """
    Validates Menai bytecode for correctness and safety.

    This performs static analysis to catch errors before execution,
    enabling the VM to remove redundant runtime checks.
    """

    def __init__(self) -> None:
        """Initialize validator."""
        # Track which opcodes affect stack depth and how
        self._init_opcode_metadata()

    def _init_opcode_metadata(self) -> None:
        """Initialize metadata about opcodes for validation."""
        # Stack effect: how many items are popped (-) and pushed (+)
        # Format: (pop_count, push_count)
        self.stack_effects: Dict[Opcode, Tuple[int, int]] = {
            # Register-based load ops: always write to dest, no stack effect.
            Opcode.LOAD_NONE: (0, 0),
            Opcode.LOAD_TRUE: (0, 0),
            Opcode.LOAD_FALSE: (0, 0),
            Opcode.LOAD_EMPTY_LIST: (0, 0),
            Opcode.LOAD_CONST: (0, 0),
            Opcode.LOAD_NAME: (0, 0),

            # Stack/register transfer: PUSH pushes 1 from a register, POP pops 1 into a register
            Opcode.PUSH: (0, 1),
            Opcode.POP: (1, 0),

            # Control flow - jumps don't affect stack, conditionals pop 1
            Opcode.JUMP: (0, 0),
            Opcode.JUMP_IF_FALSE: (1, 0),
            Opcode.JUMP_IF_TRUE: (1, 0),
            Opcode.RAISE_ERROR: (0, 0),  # Doesn't return, but doesn't matter

            # Functions - MAKE_CLOSURE pops captures and pushes closure
            # CALL_* effects depend on arity (handled specially)
            # RETURN pops 1 (handled specially as it exits)
            Opcode.MAKE_CLOSURE: (-1, 1),
            Opcode.PATCH_CLOSURE: (1, 0),  # pops 1 value, mutates existing closure slot
            Opcode.CALL: (-1, 1),
            Opcode.TAIL_CALL: (-1, 0),
            Opcode.APPLY: (2, 1),
            Opcode.TAIL_APPLY: (2, 0),
            # ENTER effect is n-dependent; handled in _get_stack_effect
            Opcode.RETURN: (1, 0),

            # Trace debug
            Opcode.EMIT_TRACE: (1, 0),  # Pops 1 (message), pushes 0

            # Function operations
            Opcode.FUNCTION_P: (1, 1),
            Opcode.FUNCTION_MIN_ARITY: (1, 1),
            Opcode.FUNCTION_VARIADIC_P: (1, 1),
            Opcode.FUNCTION_ACCEPTS_P: (2, 1),
            Opcode.FUNCTION_EQ_P: (2, 1),
            Opcode.FUNCTION_NEQ_P: (2, 1),

            # Symbol
            Opcode.SYMBOL_P: (1, 1),
            Opcode.SYMBOL_EQ_P: (2, 1),
            Opcode.SYMBOL_NEQ_P: (2, 1),
            Opcode.SYMBOL_TO_STRING: (1, 1),

            # None operations
            Opcode.NONE_P: (1, 1),

            # Boolean operations
            Opcode.BOOLEAN_P: (1, 1),
            Opcode.BOOLEAN_EQ_P: (2, 1),
            Opcode.BOOLEAN_NEQ_P: (2, 1),
            Opcode.BOOLEAN_NOT: (1, 1),

            # Integer operations
            Opcode.INTEGER_P: (1, 1),
            Opcode.INTEGER_EQ_P: (2, 1),
            Opcode.INTEGER_NEQ_P: (2, 1),
            Opcode.INTEGER_LT_P: (2, 1),
            Opcode.INTEGER_GT_P: (2, 1),
            Opcode.INTEGER_LTE_P: (2, 1),
            Opcode.INTEGER_GTE_P: (2, 1),
            Opcode.INTEGER_ABS: (1, 1),
            Opcode.INTEGER_ADD: (2, 1),
            Opcode.INTEGER_SUB: (2, 1),
            Opcode.INTEGER_MUL: (2, 1),
            Opcode.INTEGER_DIV: (2, 1),
            Opcode.INTEGER_MOD: (2, 1),
            Opcode.INTEGER_NEG: (1, 1),
            Opcode.INTEGER_EXPN: (2, 1),
            Opcode.INTEGER_BIT_NOT: (1, 1),
            Opcode.INTEGER_BIT_SHIFT_LEFT: (2, 1),
            Opcode.INTEGER_BIT_SHIFT_RIGHT: (2, 1),
            Opcode.INTEGER_BIT_OR: (2, 1),
            Opcode.INTEGER_BIT_AND: (2, 1),
            Opcode.INTEGER_BIT_XOR: (2, 1),
            Opcode.INTEGER_MIN: (2, 1),
            Opcode.INTEGER_MAX: (2, 1),
            Opcode.INTEGER_TO_FLOAT: (1, 1),
            Opcode.INTEGER_TO_COMPLEX: (2, 1),
            Opcode.INTEGER_TO_STRING: (2, 1),

            # Floating point operations
            Opcode.FLOAT_P: (1, 1),
            Opcode.FLOAT_EQ_P: (2, 1),
            Opcode.FLOAT_NEQ_P: (2, 1),
            Opcode.FLOAT_LT_P: (2, 1),
            Opcode.FLOAT_GT_P: (2, 1),
            Opcode.FLOAT_LTE_P: (2, 1),
            Opcode.FLOAT_GTE_P: (2, 1),
            Opcode.FLOAT_ABS: (1, 1),
            Opcode.FLOAT_ADD: (2, 1),
            Opcode.FLOAT_SUB: (2, 1),
            Opcode.FLOAT_MUL: (2, 1),
            Opcode.FLOAT_DIV: (2, 1),
            Opcode.FLOAT_FLOOR_DIV: (2, 1),
            Opcode.FLOAT_MOD: (2, 1),
            Opcode.FLOAT_NEG: (1, 1),
            Opcode.FLOAT_EXP: (1, 1),
            Opcode.FLOAT_EXPN: (2, 1),
            Opcode.FLOAT_LOG: (1, 1),
            Opcode.FLOAT_LOG10: (1, 1),
            Opcode.FLOAT_LOG2: (1, 1),
            Opcode.FLOAT_LOGN: (2, 1),
            Opcode.FLOAT_SIN: (1, 1),
            Opcode.FLOAT_COS: (1, 1),
            Opcode.FLOAT_TAN: (1, 1),
            Opcode.FLOAT_SQRT: (1, 1),
            Opcode.FLOAT_FLOOR: (1, 1),
            Opcode.FLOAT_CEIL: (1, 1),
            Opcode.FLOAT_ROUND: (1, 1),
            Opcode.FLOAT_MIN: (2, 1),
            Opcode.FLOAT_MAX: (2, 1),
            Opcode.FLOAT_TO_INTEGER: (1, 1),
            Opcode.FLOAT_TO_COMPLEX: (2, 1),
            Opcode.FLOAT_TO_STRING: (1, 1),

            # Complex number operations
            Opcode.COMPLEX_P: (1, 1),
            Opcode.COMPLEX_EQ_P: (2, 1),
            Opcode.COMPLEX_NEQ_P: (2, 1),
            Opcode.COMPLEX_REAL: (1, 1),
            Opcode.COMPLEX_IMAG: (1, 1),
            Opcode.COMPLEX_ABS: (1, 1),
            Opcode.COMPLEX_ADD: (2, 1),
            Opcode.COMPLEX_SUB: (2, 1),
            Opcode.COMPLEX_MUL: (2, 1),
            Opcode.COMPLEX_DIV: (2, 1),
            Opcode.COMPLEX_NEG: (1, 1),
            Opcode.COMPLEX_EXP: (1, 1),
            Opcode.COMPLEX_EXPN: (2, 1),
            Opcode.COMPLEX_LOG: (1, 1),
            Opcode.COMPLEX_LOG10: (1, 1),
            Opcode.COMPLEX_LOGN: (2, 1),
            Opcode.COMPLEX_SIN: (1, 1),
            Opcode.COMPLEX_COS: (1, 1),
            Opcode.COMPLEX_TAN: (1, 1),
            Opcode.COMPLEX_SQRT: (1, 1),
            Opcode.COMPLEX_TO_STRING: (1, 1),

            # String
            Opcode.STRING_P: (1, 1),
            Opcode.STRING_EQ_P: (2, 1),
            Opcode.STRING_NEQ_P: (2, 1),
            Opcode.STRING_LT_P: (2, 1),
            Opcode.STRING_GT_P: (2, 1),
            Opcode.STRING_LTE_P: (2, 1),
            Opcode.STRING_GTE_P: (2, 1),
            Opcode.STRING_LENGTH: (1, 1),
            Opcode.STRING_UPCASE: (1, 1),
            Opcode.STRING_DOWNCASE: (1, 1),
            Opcode.STRING_TRIM: (1, 1),
            Opcode.STRING_TRIM_LEFT: (1, 1),
            Opcode.STRING_TRIM_RIGHT: (1, 1),
            Opcode.STRING_TO_INTEGER: (2, 1),
            Opcode.STRING_TO_NUMBER: (1, 1),
            Opcode.STRING_TO_LIST: (2, 1),
            Opcode.STRING_REF: (2, 1),
            Opcode.STRING_PREFIX_P: (2, 1),
            Opcode.STRING_SUFFIX_P: (2, 1),
            Opcode.STRING_SLICE: (3, 1),
            Opcode.STRING_REPLACE: (3, 1),
            Opcode.STRING_INDEX: (2, 1),
            Opcode.STRING_CONCAT: (2, 1),

            # Alist
            Opcode.DICT_P: (1, 1),
            Opcode.DICT_EQ_P: (2, 1),
            Opcode.DICT_NEQ_P: (2, 1),
            Opcode.DICT_KEYS: (1, 1),
            Opcode.DICT_VALUES: (1, 1),
            Opcode.DICT_LENGTH: (1, 1),
            Opcode.DICT_HAS_P: (2, 1),
            Opcode.DICT_REMOVE: (2, 1),
            Opcode.DICT_MERGE: (2, 1),
            Opcode.DICT_SET: (3, 1),
            Opcode.DICT_GET: (3, 1),

            # List operations
            Opcode.LIST_P: (1, 1),
            Opcode.LIST_EQ_P: (2, 1),
            Opcode.LIST_NEQ_P: (2, 1),
            Opcode.LIST_PREPEND: (2, 1),
            Opcode.LIST_REVERSE: (1, 1),
            Opcode.LIST_FIRST: (1, 1),
            Opcode.LIST_REST: (1, 1),
            Opcode.LIST_LAST: (1, 1),
            Opcode.LIST_LENGTH: (1, 1),
            Opcode.LIST_REF: (2, 1),
            Opcode.LIST_NULL_P: (1, 1),
            Opcode.LIST_MEMBER_P: (2, 1),
            Opcode.LIST_INDEX: (2, 1),
            Opcode.LIST_APPEND: (2, 1),
            Opcode.LIST_SLICE: (3, 1),
            Opcode.LIST_REMOVE: (2, 1),
            Opcode.LIST_CONCAT: (2, 1),
            Opcode.LIST_TO_STRING: (2, 1),

            Opcode.RANGE: (3, 1),
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
        load_reg_ops = (
            Opcode.LOAD_NONE, Opcode.LOAD_TRUE, Opcode.LOAD_FALSE,
            Opcode.LOAD_EMPTY_LIST, Opcode.LOAD_CONST, Opcode.LOAD_NAME,
        )
        for i, instr in enumerate(code.instructions):
            opcode = instr.opcode

            # Validate constant pool indices
            if opcode in (Opcode.LOAD_CONST, Opcode.RAISE_ERROR):
                const_index = instr.src0
                if const_index < 0 or const_index >= len(code.constants):
                    raise ValidationError(
                        ValidationErrorType.INDEX_OUT_OF_BOUNDS,
                        f"Constant index {const_index} out of bounds (pool size: {len(code.constants)})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate name pool indices
            if opcode == Opcode.LOAD_NAME:
                name_index = instr.src0
                if name_index < 0 or name_index >= len(code.names):
                    raise ValidationError(
                        ValidationErrorType.INDEX_OUT_OF_BOUNDS,
                        f"Name index {name_index} out of bounds (pool size: {len(code.names)})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate code object indices
            if opcode == Opcode.MAKE_CLOSURE:
                code_index = instr.src0
                if code_index < 0 or code_index >= len(code.code_objects):
                    raise ValidationError(
                        ValidationErrorType.INDEX_OUT_OF_BOUNDS,
                        f"Code object index {code_index} out of bounds (pool size: {len(code.code_objects)})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate register indices (must be < local_count)
            # PUSH reads from src0; POP writes to dest.
            if opcode == Opcode.PUSH:
                var_index = instr.src0
                if var_index < 0 or var_index >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"Variable index {var_index} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            if opcode == Opcode.POP:
                var_index = instr.dest
                if var_index < 0 or var_index >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"Variable index {var_index} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate dest register bounds for register-based load ops.
            if opcode in load_reg_ops:
                var_index = instr.dest
                if var_index >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"Destination register {var_index} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate ENTER: n must match param_count and fit within local_count
            if opcode == Opcode.ENTER:
                n = instr.src0
                if n != code.param_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"ENTER count {n} does not match param_count {code.param_count}",
                        instruction_index=i,
                        opcode=opcode
                    )
                if n > code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"ENTER count {n} exceeds local_count {code.local_count}",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate PATCH_CLOSURE: var_index must be a valid local slot
            if opcode == Opcode.PATCH_CLOSURE:
                var_index = instr.src0
                if var_index < 0 or var_index >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"PATCH_CLOSURE var_index {var_index} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate jump targets
            if opcode in (Opcode.JUMP, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE):
                target = instr.src0
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
        # 1. Ends with RETURN or TAIL_CALL
        # 2. Has successors
        for block in cfg.values():
            if not block.visited:
                continue  # Unreachable code, skip

            last_instr = code.instructions[block.end_index]

            # Check if block ends properly
            ends_properly = (
                last_instr.opcode in (Opcode.RETURN, Opcode.TAIL_CALL, Opcode.TAIL_APPLY, Opcode.RAISE_ERROR) or
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

        In addition to the initialized-slot set, we track a *closure map*:
        a mapping from slot index to code_object index for slots that are
        definitively known to hold a closure created by MAKE_CLOSURE.  This
        is needed to validate PATCH_CLOSURE, which has three requirements:

          1. src0 (var_index) must refer to an initialized slot.
          2. That slot must definitively hold a closure (not an arbitrary value).
          3. src1 (capture_slot) must be < len(code_objects[code_index].free_vars)
             for the closure stored in that slot.

        At merge points the closure map is intersected conservatively: a slot
        is only kept in the map if both incoming paths agree on the same
        code_object index.  If the paths disagree (or one path doesn't have a
        closure there), the slot is dropped from the map, making any subsequent
        PATCH_CLOSURE against it a validation error.
        """
        # Track which variables are definitely initialized at each instruction
        # Maps instruction index -> (set of initialized variable indices,
        #                            dict of slot -> code_object_index for closure slots)
        initialized_at: Dict[int, Tuple[Set[int], Dict[int, int]]] = {}

        # Initial state: captured variables are pre-initialized by MAKE_CLOSURE before the frame runs.
        # Parameters are no longer pre-seeded here; ENTER at instruction 0 initializes them,
        # and the dataflow analysis tracks that naturally.
        #
        # Captured-value slots (param_count .. param_count+len(free_vars)-1) are frame
        # invariants: the VM copies them from MenaiFunction.captured_values into
        # frame.locals before the first instruction runs.  Only exactly len(free_vars)
        # slots are pre-populated — plain locals above that range must be initialised
        # by the code itself.  These slots must survive back-edge merges (e.g. JUMP 0
        # self-recursion), so we restore them into new_initialized on every step below.
        initial_initialized: Set[int] = set()
        n_captured = len(code.free_vars)
        if n_captured > 0:
            initial_initialized.update(range(code.param_count, code.param_count + n_captured))

        # Worklist algorithm
        worklist: List[int] = [0]

        # Captured-value slots are never closures created in this frame, so the
        # initial closure map is empty.
        initialized_at[0] = (initial_initialized.copy(), {})

        load_reg_ops = (
            Opcode.LOAD_NONE, Opcode.LOAD_TRUE, Opcode.LOAD_FALSE,
            Opcode.LOAD_EMPTY_LIST, Opcode.LOAD_CONST, Opcode.LOAD_NAME,
        )
        while worklist:
            instr_idx = worklist.pop(0)

            if instr_idx not in initialized_at:
                # Unreachable
                continue

            current_initialized, current_closures = initialized_at[instr_idx]
            instr = code.instructions[instr_idx]
            opcode = instr.opcode

            # Check PUSH - source register must be initialized
            if opcode == Opcode.PUSH:
                var_index = instr.src0
                if var_index not in current_initialized:
                    raise ValidationError(
                        ValidationErrorType.UNINITIALIZED_VARIABLE,
                        f"Variable at index {var_index} may be uninitialized",
                        instruction_index=instr_idx,
                        opcode=opcode,
                        context=f"Initialized variables: {sorted(current_initialized)}"
                    )

            # Check PATCH_CLOSURE:
            #   1. var_index (src0) must be initialized.
            #   2. That slot must definitively hold a closure.
            #   3. capture_slot (src1) must be in range for that closure's free_vars.
            if opcode == Opcode.PATCH_CLOSURE:
                var_index = instr.src0
                capture_slot = instr.src1

                if var_index not in current_initialized:
                    raise ValidationError(
                        ValidationErrorType.UNINITIALIZED_VARIABLE,
                        f"PATCH_CLOSURE target slot {var_index} may be uninitialized",
                        instruction_index=instr_idx,
                        opcode=opcode,
                        context=f"Initialized variables: {sorted(current_initialized)}"
                    )

                if var_index not in current_closures:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"PATCH_CLOSURE target slot {var_index} is not known to hold a closure",
                        instruction_index=instr_idx,
                        opcode=opcode,
                        context=(
                            f"Slots known to hold closures: {sorted(current_closures.keys())}"
                        )
                    )

                code_obj_index = current_closures[var_index]
                target_code = code.code_objects[code_obj_index]
                n_free = len(target_code.free_vars)
                if capture_slot < 0 or capture_slot >= n_free:
                    raise ValidationError(
                        ValidationErrorType.INDEX_OUT_OF_BOUNDS,
                        f"PATCH_CLOSURE capture_slot {capture_slot} out of range "
                        f"for closure with {n_free} free variable(s)",
                        instruction_index=instr_idx,
                        opcode=opcode,
                        context=f"Closure is code_objects[{code_obj_index}] ({target_code.name!r})"
                    )

            # Calculate new initialized set after this instruction
            new_initialized = current_initialized.copy()
            new_closures = current_closures.copy()

            # POP marks destination register as initialized
            if opcode == Opcode.POP:
                var_index = instr.dest
                new_initialized.add(var_index)
                # Check if preceding instruction was MAKE_CLOSURE for closure tracking
                if instr_idx > 0:
                    prev_instr = code.instructions[instr_idx - 1]
                    if prev_instr.opcode == Opcode.MAKE_CLOSURE:
                        new_closures[var_index] = prev_instr.src0
                    else:
                        new_closures.pop(var_index, None)
                else:
                    new_closures.pop(var_index, None)

            # Load ops mark the destination register as initialized
            if opcode in load_reg_ops:
                new_initialized.add(instr.dest)
                new_closures.pop(instr.dest, None)

            # ENTER marks locals 0..n-1 as initialized
            if opcode == Opcode.ENTER:
                new_initialized.update(range(instr.src0))

            # Re-apply frame invariants: free-var slots are always initialized.
            new_initialized |= initial_initialized

            # Get successors
            successors = self._get_successors(instr_idx, instr, code)

            # Propagate to successors
            for succ_idx in successors:
                if succ_idx in initialized_at:
                    # Merge: only keep variables initialized on ALL paths
                    existing_init, existing_closures = initialized_at[succ_idx]
                    merged_init = existing_init & new_initialized
                    # Closure map: keep only slots where both paths agree on the
                    # same code_object index.
                    merged_closures = {
                        slot: cidx
                        for slot, cidx in existing_closures.items()
                        if new_closures.get(slot) == cidx
                    }
                    if merged_init != existing_init or merged_closures != existing_closures:
                        initialized_at[succ_idx] = (merged_init, merged_closures)
                        worklist.append(succ_idx)

                else:
                    # First time visiting
                    initialized_at[succ_idx] = (new_initialized.copy(), new_closures.copy())
                    worklist.append(succ_idx)

    def _get_stack_effect(self, instr: Instruction) -> Tuple[int, int]:
        """
        Get stack effect (pop_count, push_count) for an instruction.

        Returns:
            (pop_count, push_count) tuple
        """
        opcode = instr.opcode

        if opcode == Opcode.MAKE_CLOSURE:
            capture_count = instr.src1
            return (capture_count, 1)  # Pop captures, push closure

        if opcode == Opcode.CALL:
            arity = instr.src0
            return (arity + 1, 1)  # Pop function + args, push result

        if opcode == Opcode.TAIL_CALL:
            arity = instr.src0
            return (arity + 1, 0)  # Pop function + args, tail position (no push)

        if opcode == Opcode.ENTER:
            n = instr.src0
            return (n, 0)  # Pop n args from stack, store into locals 0..n-1

        if opcode == Opcode.LIST:
            n = instr.src0
            return (n, 1)  # Pop n elements, push list

        if opcode == Opcode.DICT:
            n = instr.src0
            return (n, 1)  # Pop n pair-lists, push dict

        # Default case
        return self.stack_effects.get(opcode, (0, 0))

    def _get_successors(self, instr_idx: int, instr: Instruction, code: CodeObject) -> List[int]:
        """Get successor instruction indices for an instruction."""
        opcode = instr.opcode

        # Terminal instructions have no successors
        if opcode in (Opcode.RETURN, Opcode.RAISE_ERROR):
            return []

        # Tail calls/applies are terminal (they replace the frame)
        if opcode in (Opcode.TAIL_CALL, Opcode.TAIL_APPLY):
            return []

        successors = []

        # Unconditional jump
        if opcode == Opcode.JUMP:
            successors.append(instr.src0)

        # Conditional jumps have two successors
        elif opcode in (Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE):
            successors.append(instr.src0)  # Jump target
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
                leaders.add(instr.src0)
                # Instruction after conditional jump is a leader
                if instr.opcode in (Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE):
                    if i + 1 < len(code.instructions):
                        leaders.add(i + 1)

            # Instruction after RETURN/RAISE_ERROR is a leader (if exists)
            if instr.opcode in (Opcode.RETURN, Opcode.RAISE_ERROR, Opcode.TAIL_CALL, Opcode.TAIL_APPLY):
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
