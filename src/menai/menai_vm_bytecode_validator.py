"""
Bytecode validator for Menai virtual machine.

This validator performs static analysis on bytecode to ensure it's well-formed
and safe to execute. By validating once before execution, we can remove many
runtime checks from the hot VM execution loop.

The validator checks:
- Structural invariants (valid jumps, indices in bounds)
- Variable access validity
- Control flow correctness (all paths return)
- Function/closure well-formedness
"""

from dataclasses import dataclass, field
from collections import deque
from typing import Deque, List, Dict, Tuple, Set
from enum import Enum

from menai.menai_bytecode import CodeObject, Instruction, Opcode


class ValidationErrorType(Enum):
    """Types of validation errors."""
    INVALID_JUMP_TARGET = "invalid_jump_target"
    INDEX_OUT_OF_BOUNDS = "index_out_of_bounds"
    MISSING_RETURN = "missing_return"
    INVALID_OPCODE = "invalid_opcode"
    INVALID_VARIABLE_ACCESS = "invalid_variable_access"
    UNINITIALIZED_VARIABLE = "uninitialized_variable"


@dataclass
class ValidationError(Exception):
    """Bytecode validation error with detailed context."""
    error_type: ValidationErrorType
    message: str
    instruction_index: int | None = None
    opcode: int | None = None
    context: str | None = None

    def __str__(self) -> str:
        parts = [f"Bytecode validation error: {self.message}"]
        if self.instruction_index is not None:
            parts.append(f"  at instruction {self.instruction_index}")

        if self.opcode is not None:
            parts.append(f"  opcode: {Opcode(self.opcode).name}")

        if self.context:
            parts.append(f"  context: {self.context}")
        return "\n".join(parts)


@dataclass
class BasicBlock:
    """A basic block in the control flow graph."""
    start_index: int
    end_index: int  # Inclusive
    successors: List[int] = field(default_factory=list)  # Instruction indices
    predecessors: List[int] = field(default_factory=list)  # Instruction indices
    visited: bool = False


class BytecodeValidator:
    """
    Validates Menai bytecode for correctness and safety.

    This performs static analysis to catch errors before execution,
    enabling the VM to remove redundant runtime checks.
    """

    # Opcodes that do not write a destination register — dest field is unused.
    # Every opcode not in this set writes its result to instr.dest.
    NO_DEST_OPCODES: frozenset = frozenset({
        Opcode.TAIL_CALL, Opcode.TAIL_APPLY,
        Opcode.PATCH_CLOSURE, Opcode.RETURN,
        Opcode.EMIT_TRACE, Opcode.JUMP, Opcode.JUMP_IF_FALSE,
        Opcode.JUMP_IF_TRUE, Opcode.RAISE_ERROR,
    })

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
            try:
                Opcode(instr.opcode)

            except ValueError as e:
                raise ValidationError(
                    ValidationErrorType.INVALID_OPCODE,
                    f"Invalid opcode value: {instr.opcode}",
                    instruction_index=i
                ) from e

    def _validate_indices(self, code: CodeObject) -> None:
        """Validate all indices (constants, names, code objects, variables)."""
        total_slots = code.local_count + code.outgoing_arg_slots
        for i, instr in enumerate(code.instructions):
            opcode = instr.opcode

            # Validate constant pool indices
            if opcode in (Opcode.LOAD_CONST, Opcode.RAISE_ERROR, Opcode.LOAD_STRUCT_TYPE):
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

            # Validate MOVE: src and dest may reach into the outgoing zone.
            if opcode == Opcode.MOVE:
                var_index = instr.src0
                if var_index < 0 or var_index >= total_slots:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"MOVE source {var_index} out of bounds (total_slots: {total_slots})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate dest register bounds for all opcodes that write a dest register.
            # Any dest-writing op may write into the outgoing zone (dest < total_slots).
            # The outgoing zone is populated by argument-placement instructions before a CALL.
            if opcode not in self.NO_DEST_OPCODES:
                if instr.dest < 0 or instr.dest >= total_slots:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"Destination register {instr.dest} out of bounds (total_slots: {total_slots})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate func register (src0) for call/apply opcodes.
            if opcode in (Opcode.CALL, Opcode.TAIL_CALL, Opcode.APPLY, Opcode.TAIL_APPLY):
                func_reg = instr.src0
                if func_reg < 0 or func_reg >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"Function register {func_reg} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            # Validate PATCH_CLOSURE: all three register operands must be valid
            # Validate APPLY/TAIL_APPLY: src1 is the arg_list register, must be within local_count.
            if opcode in (Opcode.APPLY, Opcode.TAIL_APPLY):
                arg_list_reg = instr.src1
                if arg_list_reg < 0 or arg_list_reg >= code.local_count:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"APPLY arg_list register {arg_list_reg} out of bounds (local_count: {code.local_count})",
                        instruction_index=i,
                        opcode=opcode
                    )

            if opcode == Opcode.PATCH_CLOSURE:
                for field_name, reg in (('src0 (closure)', instr.src0), ('src2 (value)', instr.src2)):
                    if reg < 0 or reg >= code.local_count:
                        raise ValidationError(
                            ValidationErrorType.INVALID_VARIABLE_ACCESS,
                            f"PATCH_CLOSURE {field_name} register {reg} out of bounds (local_count: {code.local_count})",
                            instruction_index=i,
                            opcode=opcode
                        )

                # src1 is the capture index — validated in _validate_variable_initialization
                if instr.src1 < 0:
                    raise ValidationError(
                        ValidationErrorType.INVALID_VARIABLE_ACCESS,
                        f"PATCH_CLOSURE capture_idx (src1) {instr.src1} is negative",
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

        # Parameter slots (0..param_count-1) and captured-value slots
        # (param_count..param_count+len(free_vars)-1) are both pre-populated
        # by the caller/VM before the first instruction runs.
        # They survive back-edge merges by being unioned in on every step.
        initial_initialized: Set[int] = set()
        if code.param_count > 0:
            initial_initialized.update(range(code.param_count))
        n_captured = len(code.free_vars)
        if n_captured > 0:
            initial_initialized.update(range(code.param_count, code.param_count + n_captured))

        # Use a deque for O(1) popleft rather than O(N) pop(0) on a list.
        worklist: Deque[int] = deque([0])
        initialized_at[0] = (initial_initialized.copy(), {})  # closure map starts empty

        while worklist:
            instr_idx = worklist.popleft()

            if instr_idx not in initialized_at:
                continue

            current_initialized, current_closures = initialized_at[instr_idx]
            instr = code.instructions[instr_idx]
            opcode = instr.opcode

            # Check MOVE - source register must be initialized
            if opcode == Opcode.MOVE:
                var_index = instr.src0
                if var_index not in current_initialized:
                    raise ValidationError(
                        ValidationErrorType.UNINITIALIZED_VARIABLE,
                        f"MOVE source register {var_index} may be uninitialized",
                        instruction_index=instr_idx,
                        opcode=opcode,
                        context=f"Initialized variables: {sorted(current_initialized)}"
                    )

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
            #   src1 = capture index    — must be in range for the closure's free_vars.
            #   src2 = value register   — must be initialized.
            if opcode == Opcode.PATCH_CLOSURE:
                var_index = instr.src0
                capture_slot = instr.src1
                value_reg = instr.src2

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

            # Check CALL/TAIL_CALL/APPLY/TAIL_APPLY — func register (src0) must be initialized.
            # For APPLY/TAIL_APPLY also check the arg_list register (src1).
            if opcode in (Opcode.CALL, Opcode.TAIL_CALL, Opcode.APPLY, Opcode.TAIL_APPLY):
                func_reg = instr.src0
                if func_reg not in current_initialized:
                    raise ValidationError(
                        ValidationErrorType.UNINITIALIZED_VARIABLE,
                        f"Function register {func_reg} may be uninitialized",
                        instruction_index=instr_idx,
                        opcode=opcode,
                        context=f"Initialized variables: {sorted(current_initialized)}"
                    )

            if opcode in (Opcode.APPLY, Opcode.TAIL_APPLY):
                arg_list_reg = instr.src1
                if arg_list_reg not in current_initialized:
                    raise ValidationError(
                        ValidationErrorType.UNINITIALIZED_VARIABLE,
                        f"APPLY arg_list register {arg_list_reg} may be uninitialized",
                        instruction_index=instr_idx,
                        opcode=opcode,
                        context=f"Initialized variables: {sorted(current_initialized)}"
                    )

            # Calculate new initialized set after this instruction
            # Compute the exit state by mutating current_initialized in place.
            # We only copy if there are two successors (branch), so that each
            # successor gets an independent snapshot.  For the common straight-line
            # case (one successor, not yet visited) no copy is needed at all.
            if opcode == Opcode.MAKE_CLOSURE:
                current_initialized.add(instr.dest)
                current_closures[instr.dest] = instr.src0
            elif opcode not in self.NO_DEST_OPCODES:
                current_initialized.add(instr.dest)
                current_closures.pop(instr.dest, None)

            if initial_initialized:
                current_initialized |= initial_initialized

            successors = self._get_successors(instr_idx, instr, code)

            # Propagate exit state to each successor.
            # For two successors (branch) we must copy so each gets an independent state.
            for i, succ_idx in enumerate(successors):
                need_copy = i < len(successors) - 1  # copy for all but the last successor
                if succ_idx in initialized_at:
                    existing_init, existing_closures = initialized_at[succ_idx]
                    merged_init = existing_init & current_initialized
                    merged_closures = {
                        slot: cidx
                        for slot, cidx in existing_closures.items()
                        if current_closures.get(slot) == cidx
                    }
                    if merged_init != existing_init or merged_closures != existing_closures:
                        initialized_at[succ_idx] = (merged_init, merged_closures)
                        worklist.append(succ_idx)
                else:
                    initialized_at[succ_idx] = (
                        current_initialized.copy() if need_copy else current_initialized,
                        current_closures.copy() if need_copy else current_closures,
                    )
                    worklist.append(succ_idx)

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

            for succ_idx in successors:
                # Successor indices are always block leaders by construction.
                block.successors.append(succ_idx)
                blocks[succ_idx].predecessors.append(start)

        # Mark reachable blocks
        self._mark_reachable(blocks, 0)

        return blocks

    def _mark_reachable(self, blocks: Dict[int, BasicBlock], start: int) -> None:
        """Mark all blocks reachable from start using an iterative worklist."""
        worklist: Deque[int] = deque([start])
        while worklist:
            idx = worklist.popleft()
            if idx not in blocks or blocks[idx].visited:
                continue
            blocks[idx].visited = True
            worklist.extend(blocks[idx].successors)


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
