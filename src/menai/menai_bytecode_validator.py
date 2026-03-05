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
            Opcode.LOAD_EMPTY_DICT: (0, 0),
            Opcode.LOAD_CONST: (0, 0),
            Opcode.LOAD_NAME: (0, 0),

            # Stack/register transfer: PUSH pushes 1 from a register, POP pops 1 into a register
            Opcode.PUSH: (0, 1),
            Opcode.POP: (1, 0),

            # Control flow - no stack effects (conditionals now read from a register)
            Opcode.JUMP: (0, 0),
            Opcode.JUMP_IF_FALSE: (0, 0),
            Opcode.JUMP_IF_TRUE: (0, 0),
            Opcode.RAISE_ERROR: (0, 0),  # Doesn't return, but doesn't matter

            # Functions
            Opcode.MAKE_CLOSURE: (0, 0),
            Opcode.PATCH_CLOSURE: (0, 0),
            Opcode.CALL: (-1, 0),
            Opcode.TAIL_CALL: (-1, 0),
            Opcode.APPLY: (2, 0),
            Opcode.TAIL_APPLY: (2, 0),
            # ENTER effect is n-dependent; handled in _get_stack_effect
            Opcode.RETURN: (0, 0),

            # Trace debug
            Opcode.EMIT_TRACE: (0, 0),

            # Function operations
            Opcode.FUNCTION_P: (0, 0),
            Opcode.FUNCTION_MIN_ARITY: (0, 0),
            Opcode.FUNCTION_VARIADIC_P: (0, 0),
            Opcode.FUNCTION_ACCEPTS_P: (0, 0),
            Opcode.FUNCTION_EQ_P: (0, 0),
            Opcode.FUNCTION_NEQ_P: (0, 0),

            # Symbol
            Opcode.SYMBOL_P: (0, 0),
            Opcode.SYMBOL_EQ_P: (0, 0),
            Opcode.SYMBOL_NEQ_P: (0, 0),
            Opcode.SYMBOL_TO_STRING: (0, 0),

            # None operations
            Opcode.NONE_P: (0, 0),

            # Boolean operations
            Opcode.BOOLEAN_P: (0, 0),
            Opcode.BOOLEAN_EQ_P: (0, 0),
            Opcode.BOOLEAN_NEQ_P: (0, 0),
            Opcode.BOOLEAN_NOT: (0, 0),

            # Integer operations
            Opcode.INTEGER_P: (0, 0),
            Opcode.INTEGER_EQ_P: (0, 0),
            Opcode.INTEGER_NEQ_P: (0, 0),
            Opcode.INTEGER_LT_P: (0, 0),
            Opcode.INTEGER_GT_P: (0, 0),
            Opcode.INTEGER_LTE_P: (0, 0),
            Opcode.INTEGER_GTE_P: (0, 0),
            Opcode.INTEGER_ABS: (0, 0),
            Opcode.INTEGER_ADD: (0, 0),
            Opcode.INTEGER_SUB: (0, 0),
            Opcode.INTEGER_MUL: (0, 0),
            Opcode.INTEGER_DIV: (0, 0),
            Opcode.INTEGER_MOD: (0, 0),
            Opcode.INTEGER_NEG: (0, 0),
            Opcode.INTEGER_EXPN: (0, 0),
            Opcode.INTEGER_BIT_NOT: (0, 0),
            Opcode.INTEGER_BIT_SHIFT_LEFT: (0, 0),
            Opcode.INTEGER_BIT_SHIFT_RIGHT: (0, 0),
            Opcode.INTEGER_BIT_OR: (0, 0),
            Opcode.INTEGER_BIT_AND: (0, 0),
            Opcode.INTEGER_BIT_XOR: (0, 0),
            Opcode.INTEGER_MIN: (0, 0),
            Opcode.INTEGER_MAX: (0, 0),
            Opcode.INTEGER_TO_FLOAT: (0, 0),
            Opcode.INTEGER_TO_COMPLEX: (0, 0),
            Opcode.INTEGER_TO_STRING: (0, 0),

            # Floating point operations
            Opcode.FLOAT_P: (0, 0),
            Opcode.FLOAT_EQ_P: (0, 0),
            Opcode.FLOAT_NEQ_P: (0, 0),
            Opcode.FLOAT_LT_P: (0, 0),
            Opcode.FLOAT_GT_P: (0, 0),
            Opcode.FLOAT_LTE_P: (0, 0),
            Opcode.FLOAT_GTE_P: (0, 0),
            Opcode.FLOAT_ABS: (0, 0),
            Opcode.FLOAT_ADD: (0, 0),
            Opcode.FLOAT_SUB: (0, 0),
            Opcode.FLOAT_MUL: (0, 0),
            Opcode.FLOAT_DIV: (0, 0),
            Opcode.FLOAT_FLOOR_DIV: (0, 0),
            Opcode.FLOAT_MOD: (0, 0),
            Opcode.FLOAT_NEG: (0, 0),
            Opcode.FLOAT_EXP: (0, 0),
            Opcode.FLOAT_EXPN: (0, 0),
            Opcode.FLOAT_LOG: (0, 0),
            Opcode.FLOAT_LOG10: (0, 0),
            Opcode.FLOAT_LOG2: (0, 0),
            Opcode.FLOAT_LOGN: (0, 0),
            Opcode.FLOAT_SIN: (0, 0),
            Opcode.FLOAT_COS: (0, 0),
            Opcode.FLOAT_TAN: (0, 0),
            Opcode.FLOAT_SQRT: (0, 0),
            Opcode.FLOAT_FLOOR: (0, 0),
            Opcode.FLOAT_CEIL: (0, 0),
            Opcode.FLOAT_ROUND: (0, 0),
            Opcode.FLOAT_MIN: (0, 0),
            Opcode.FLOAT_MAX: (0, 0),
            Opcode.FLOAT_TO_INTEGER: (0, 0),
            Opcode.FLOAT_TO_COMPLEX: (0, 0),
            Opcode.FLOAT_TO_STRING: (0, 0),

            # Complex number operations
            Opcode.COMPLEX_P: (0, 0),
            Opcode.COMPLEX_EQ_P: (0, 0),
            Opcode.COMPLEX_NEQ_P: (0, 0),
            Opcode.COMPLEX_REAL: (0, 0),
            Opcode.COMPLEX_IMAG: (0, 0),
            Opcode.COMPLEX_ABS: (0, 0),
            Opcode.COMPLEX_ADD: (0, 0),
            Opcode.COMPLEX_SUB: (0, 0),
            Opcode.COMPLEX_MUL: (0, 0),
            Opcode.COMPLEX_DIV: (0, 0),
            Opcode.COMPLEX_NEG: (0, 0),
            Opcode.COMPLEX_EXP: (0, 0),
            Opcode.COMPLEX_EXPN: (0, 0),
            Opcode.COMPLEX_LOG: (0, 0),
            Opcode.COMPLEX_LOG10: (0, 0),
            Opcode.COMPLEX_LOGN: (0, 0),
            Opcode.COMPLEX_SIN: (0, 0),
            Opcode.COMPLEX_COS: (0, 0),
            Opcode.COMPLEX_TAN: (0, 0),
            Opcode.COMPLEX_SQRT: (0, 0),
            Opcode.COMPLEX_TO_STRING: (0, 0),

            # String
            Opcode.STRING_P: (0, 0),
            Opcode.STRING_EQ_P: (0, 0),
            Opcode.STRING_NEQ_P: (0, 0),
            Opcode.STRING_LT_P: (0, 0),
            Opcode.STRING_GT_P: (0, 0),
            Opcode.STRING_LTE_P: (0, 0),
            Opcode.STRING_GTE_P: (0, 0),
            Opcode.STRING_LENGTH: (0, 0),
            Opcode.STRING_UPCASE: (0, 0),
            Opcode.STRING_DOWNCASE: (0, 0),
            Opcode.STRING_TRIM: (0, 0),
            Opcode.STRING_TRIM_LEFT: (0, 0),
            Opcode.STRING_TRIM_RIGHT: (0, 0),
            Opcode.STRING_TO_INTEGER: (0, 0),
            Opcode.STRING_TO_NUMBER: (0, 0),
            Opcode.STRING_TO_LIST: (0, 0),
            Opcode.STRING_REF: (0, 0),
            Opcode.STRING_PREFIX_P: (0, 0),
            Opcode.STRING_SUFFIX_P: (0, 0),
            Opcode.STRING_SLICE: (0, 0),
            Opcode.STRING_REPLACE: (0, 0),
            Opcode.STRING_INDEX: (0, 0),
            Opcode.STRING_CONCAT: (0, 0),

            # Alist
            Opcode.DICT_P: (0, 0),
            Opcode.DICT_EQ_P: (0, 0),
            Opcode.DICT_NEQ_P: (0, 0),
            Opcode.DICT_KEYS: (0, 0),
            Opcode.DICT_VALUES: (0, 0),
            Opcode.DICT_LENGTH: (0, 0),
            Opcode.DICT_HAS_P: (0, 0),
            Opcode.DICT_REMOVE: (0, 0),
            Opcode.DICT_MERGE: (0, 0),
            Opcode.DICT_SET: (0, 0),
            Opcode.DICT_GET: (0, 0),

            # List operations
            Opcode.LIST_P: (0, 0),
            Opcode.LIST_EQ_P: (0, 0),
            Opcode.LIST_NEQ_P: (0, 0),
            Opcode.LIST_PREPEND: (0, 0),
            Opcode.LIST_REVERSE: (0, 0),
            Opcode.LIST_FIRST: (0, 0),
            Opcode.LIST_REST: (0, 0),
            Opcode.LIST_LAST: (0, 0),
            Opcode.LIST_LENGTH: (0, 0),
            Opcode.LIST_REF: (0, 0),
            Opcode.LIST_NULL_P: (0, 0),
            Opcode.LIST_MEMBER_P: (0, 0),
            Opcode.LIST_INDEX: (0, 0),
            Opcode.LIST_APPEND: (0, 0),
            Opcode.LIST_SLICE: (0, 0),
            Opcode.LIST_REMOVE: (0, 0),
            Opcode.LIST_CONCAT: (0, 0),
            Opcode.LIST_TO_STRING: (0, 0),

            Opcode.RANGE: (0, 0),
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
            Opcode.LOAD_EMPTY_LIST, Opcode.LOAD_EMPTY_DICT, Opcode.LOAD_CONST, Opcode.LOAD_NAME,
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

            # Validate MAKE_CLOSURE dest register and code_object index
            if opcode == Opcode.MAKE_CLOSURE:
                if instr.dest < 0 or instr.dest >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"MAKE_CLOSURE dest {instr.dest} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate EMIT_TRACE src0 register
            if opcode == Opcode.EMIT_TRACE:
                if instr.src0 < 0 or instr.src0 >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"EMIT_TRACE src0 {instr.src0} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate RETURN src0 register
            if opcode == Opcode.RETURN:
                if instr.src0 < 0 or instr.src0 >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"RETURN src0 {instr.src0} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate CALL dest register
            if opcode == Opcode.CALL:
                if instr.dest < 0 or instr.dest >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"CALL dest {instr.dest} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate APPLY dest register
            if opcode == Opcode.APPLY:
                if instr.dest < 0 or instr.dest >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"APPLY dest {instr.dest} out of bounds (local_count: {code.local_count})",
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

            # Validate PATCH_CLOSURE: all three register operands must be valid
            if opcode == Opcode.PATCH_CLOSURE:
                for field_name, reg in (('src0 (closure)', instr.src0), ('src1 (value)', instr.src1)):
                    if reg < 0 or reg >= code.local_count:
                        raise ValidationError(
                            ValidationErrorType.INVALID_VARIABLE_ACCESS,
                            f"PATCH_CLOSURE {field_name} register {reg} out of bounds (local_count: {code.local_count})",
                            instruction_index=i,
                            opcode=opcode
                        )

                # src2 is the capture index — validated in _validate_variable_initialization
                if instr.src2 < 0:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"PATCH_CLOSURE capture_idx (src2) {instr.src2} is negative",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate jump targets
            if opcode == Opcode.JUMP:
                target = instr.src0
                if target < 0 or target >= len(code.instructions):
                    raise ValidationError(
                        ValidationErrorType.INVALID_JUMP_TARGET,
                        f"Jump target {target} out of bounds (instruction count: {len(code.instructions)})",
                        instruction_index=i,
                        opcode=opcode
                    )

            if opcode in (Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_TRUE):
                cond_reg = instr.src0
                target = instr.src1
                if cond_reg < 0 or cond_reg >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"Condition register {cond_reg} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

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
            Opcode.LOAD_EMPTY_LIST, Opcode.LOAD_EMPTY_DICT, Opcode.LOAD_CONST, Opcode.LOAD_NAME,
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

            # Check EMIT_TRACE - source register must be initialized
            if opcode == Opcode.EMIT_TRACE:
                var_index = instr.src0
                if var_index not in current_initialized:
                    raise ValidationError(
                        ValidationErrorType.UNINITIALIZED_VARIABLE,
                        f"EMIT_TRACE source register {var_index} may be uninitialized",
                        instruction_index=instr_idx,
                        opcode=opcode,
                        context=f"Initialized variables: {sorted(current_initialized)}"
                    )

            # Check RETURN - source register must be initialized
            if opcode == Opcode.RETURN:
                var_index = instr.src0
                if var_index not in current_initialized:
                    raise ValidationError(
                        ValidationErrorType.UNINITIALIZED_VARIABLE,
                        f"RETURN source register {var_index} may be uninitialized",
                        instruction_index=instr_idx,
                        opcode=opcode,
                        context=f"Initialized variables: {sorted(current_initialized)}"
                    )

            # Check PATCH_CLOSURE:
            #   src0 = closure register — must be initialized and hold a closure.
            #   src1 = value register   — must be initialized.
            #   src2 = capture index    — must be in range for the closure's free_vars.
            if opcode == Opcode.PATCH_CLOSURE:
                var_index = instr.src0
                value_reg = instr.src1
                capture_slot = instr.src2

                if var_index not in current_initialized:
                    raise ValidationError(
                        ValidationErrorType.UNINITIALIZED_VARIABLE,
                        f"PATCH_CLOSURE target slot {var_index} may be uninitialized",
                        instruction_index=instr_idx,
                        opcode=opcode,
                        context=f"Initialized variables: {sorted(current_initialized)}"
                    )

                if value_reg not in current_initialized:
                    raise ValidationError(
                        ValidationErrorType.UNINITIALIZED_VARIABLE,
                        f"PATCH_CLOSURE value register {value_reg} may be uninitialized",
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
                new_closures.pop(var_index, None)

            # MAKE_CLOSURE marks dest as initialized and records the closure identity
            if opcode == Opcode.MAKE_CLOSURE:
                var_index = instr.dest
                new_initialized.add(var_index)
                new_closures[var_index] = instr.src0  # src0 = code_object index

            # Load ops mark the destination register as initialized
            if opcode in load_reg_ops:
                new_initialized.add(instr.dest)
                new_closures.pop(instr.dest, None)

            # CALL and APPLY mark dest as initialized (result written by VM)
            if opcode in (Opcode.CALL, Opcode.APPLY):
                new_initialized.add(instr.dest)
                new_closures.pop(instr.dest, None)

            # Any register-based op with a dest register marks it as initialized
            if opcode.has_dest() and opcode not in (Opcode.MAKE_CLOSURE, Opcode.CALL, Opcode.APPLY):
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
            return (capture_count, 0)  # Pop captures, write closure to dest register

        if opcode == Opcode.CALL:
            arity = instr.src0
            return (arity + 1, 0)  # Pop function + args, result written to dest register

        if opcode == Opcode.TAIL_CALL:
            arity = instr.src0
            return (arity + 1, 0)  # Pop function + args, tail position (no push)

        if opcode == Opcode.ENTER:
            n = instr.src0
            return (n, 0)  # Pop n args from stack, store into locals 0..n-1

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
            successors.append(instr.src1)  # Jump target (condition is in src0)
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
                leaders.add(instr.src0 if instr.opcode == Opcode.JUMP else instr.src1)
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
