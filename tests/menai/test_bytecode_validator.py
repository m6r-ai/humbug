"""Tests for bytecode validator.

This tests the validator's ability to catch various bytecode errors.
"""

import pytest

from menai.menai_bytecode import CodeObject, Instruction, Opcode
from menai.menai_bytecode_validator import (
    BytecodeValidator, ValidationError, ValidationErrorType, validate_bytecode
)
from menai.menai_value import MenaiInteger, MenaiString


class TestBytecodeValidator:
    """Test bytecode validation."""

    def test_valid_simple_code(self):
        """Test that valid bytecode passes validation."""
        # Simple code: LOAD_CONST 0, RETURN
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=0
        )

        # Should not raise
        validate_bytecode(code)

    def test_invalid_constant_index(self):
        """Test that invalid constant index is caught."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 5),  # Index 5 but only 1 constant
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=0
        )

        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS
        assert "Constant index" in exc_info.value.message

    def test_invalid_name_index(self):
        """Test that invalid name index is caught."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_NAME, 3),  # Index 3 but only 1 name
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=["x"],
            code_objects=[],
            local_count=0
        )

        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS
        assert "Name index" in exc_info.value.message

    def test_invalid_jump_target(self):
        """Test that invalid jump target is caught."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.JUMP, 100),  # Jump to instruction 100 (doesn't exist)
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=0
        )

        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INVALID_JUMP_TARGET
        assert "Jump target" in exc_info.value.message

    def test_invalid_variable_index(self):
        """Test that invalid variable index is caught."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_VAR, 5, 0),  # Index 5 but local_count is 2
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=2
        )

        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS
        assert "Variable index" in exc_info.value.message

    def test_stack_underflow(self):
        """Test that stack underflow is caught."""
        # Try to RETURN without anything on stack
        code = CodeObject(
            instructions=[
                Instruction(Opcode.RETURN),  # Stack is empty, can't pop
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=0
        )

        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.STACK_UNDERFLOW

    def test_stack_underflow_in_call(self):
        """Test that stack underflow in function call is caught."""
        # Try to call function with arity 2 but only 1 item on stack
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 0),  # Push 1 item
                Instruction(Opcode.CALL, 2),  # Try to call with arity 2 (needs 3 items: func + 2 args)
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=0
        )

        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.STACK_UNDERFLOW

    def test_inconsistent_stack_depth(self):
        """Test that inconsistent stack depth at merge point is caught."""
        # Two paths to same instruction with different stack depths
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE),           # 0: Push true (depth=1)
                Instruction(Opcode.JUMP_IF_FALSE, 4),    # 1: Jump to 4 if false (pops, depth=0)
                # Fall through path: depth=0
                Instruction(Opcode.LOAD_CONST, 0),       # 2: Push constant (depth=1)
                Instruction(Opcode.JUMP, 5),             # 3: Jump to 5 (depth=1)
                # Jump path: depth=0
                Instruction(Opcode.LOAD_CONST, 0),       # 4: Push constant (depth=1), then fall to 5 (depth=1)
                Instruction(Opcode.RETURN),              # 5: Return (depth from 3 is 1, from 4 is 1 - consistent!)
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=0
        )

        # This should actually pass - the stack is consistent
        validate_bytecode(code)

    def test_truly_inconsistent_stack_depth(self):
        """Test that truly inconsistent stack depth at merge point is caught."""
        # Two paths to same instruction with different stack depths
        # This creates a situation where merging paths have different depths
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE),           # 0: depth=0->1
                Instruction(Opcode.JUMP_IF_FALSE, 4),    # 1: pops, jump to 4 (depth=0) or fall through (depth=0)
                Instruction(Opcode.LOAD_CONST, 0),       # 2: depth=0->1
                Instruction(Opcode.JUMP, 5),             # 3: depth=1, jump to 5
                # From jump at 1:
                Instruction(Opcode.JUMP, 5),             # 4: depth=0, jump to 5
                # Merge point:
                Instruction(Opcode.RETURN),              # 5: depth from 3 is 1, from 4 is 0 - inconsistent!
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=0
        )

        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        # Could be STACK_INCONSISTENT or STACK_UNDERFLOW depending on which path is analyzed first
        assert exc_info.value.error_type in (ValidationErrorType.STACK_INCONSISTENT, ValidationErrorType.STACK_UNDERFLOW)

    def test_valid_conditional_jump(self):
        """Test that valid conditional jump passes validation."""
        # if true: return 1 else: return 2
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE),           # 0: Push true
                Instruction(Opcode.JUMP_IF_FALSE, 4),    # 1: Jump to 4 if false
                Instruction(Opcode.LOAD_CONST, 0),       # 2: Push 1
                Instruction(Opcode.RETURN),              # 3: Return
                Instruction(Opcode.LOAD_CONST, 1),       # 4: Push 2
                Instruction(Opcode.RETURN),              # 5: Return
            ],
            constants=[MenaiInteger(1), MenaiInteger(2)],
            names=[],
            code_objects=[],
            local_count=0
        )

        # Should not raise
        validate_bytecode(code)

    def test_valid_loop(self):
        """Test that valid loop (backward jump) passes validation."""
        # Simple loop that jumps back
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 0),       # 0: Push constant
                Instruction(Opcode.LOAD_TRUE),           # 1: Push true
                Instruction(Opcode.JUMP_IF_FALSE, 5),    # 2: Exit loop if false
                Instruction(Opcode.JUMP, 1),             # 3: Jump back to 1 (loop)
                Instruction(Opcode.JUMP, 1),             # 4: Unreachable (but valid)
                Instruction(Opcode.RETURN),              # 5: Return
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=0
        )

        # Should not raise
        validate_bytecode(code)

    def test_valid_make_closure(self):
        """Test that valid MAKE_CLOSURE passes validation."""
        # Create a simple lambda
        lambda_code = CodeObject(
            instructions=[
                Instruction(Opcode.STORE_VAR, 0),     # Store parameter
                Instruction(Opcode.LOAD_VAR, 0),      # Load parameter
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=[],
            code_objects=[],
            param_count=1,
            local_count=1
        )

        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 0),       # Push captured value
                Instruction(Opcode.MAKE_CLOSURE, 0, 1),  # Make closure with 1 capture
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[lambda_code],
            local_count=0
        )

        # Should not raise
        validate_bytecode(code)

    def test_empty_code_object(self):
        """Test that empty code object is caught."""
        code = CodeObject(
            instructions=[],  # No instructions
            constants=[],
            names=[],
            code_objects=[],
            local_count=0
        )

        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INVALID_OPCODE
        assert "no instructions" in exc_info.value.message

    def test_missing_return(self):
        """Test that missing return is caught."""
        # Code that falls off the end without RETURN
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 0),  # Push constant
                # Missing RETURN
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=0
        )

        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.MISSING_RETURN

    def test_tail_call_is_terminal(self):
        """Test that TAIL_CALL is treated as terminal (no successors)."""
        # TAIL_CALL should be terminal, code after it is unreachable but valid
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_NAME, 0),        # Load function
                Instruction(Opcode.LOAD_CONST, 0),       # Load arg
                Instruction(Opcode.TAIL_CALL, 1),        # Tail call
                Instruction(Opcode.LOAD_CONST, 0),       # Unreachable (but valid)
                Instruction(Opcode.RETURN),              # Unreachable (but valid)
            ],
            constants=[MenaiInteger(42)],
            names=["f"],
            code_objects=[],
            local_count=0
        )

        # Should not raise
        validate_bytecode(code)

    def test_nested_code_validation(self):
        """Test that nested code objects are validated recursively."""
        # Create invalid nested code
        invalid_lambda = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 99),  # Invalid constant index
                Instruction(Opcode.RETURN),
            ],
            constants=[],  # No constants!
            names=[],
            code_objects=[],
            local_count=0
        )

        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, 0, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=[],
            code_objects=[invalid_lambda],
            local_count=0
        )

        # Should catch error in nested code
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS

    def test_uninitialized_variable_simple(self):
        """Test that using uninitialized variable is caught."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_VAR, 0),  # Load var 0 without storing first
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=1
        )

        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE

    def test_initialized_variable_ok(self):
        """Test that initialized variable passes validation."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 0),   # Push value
                Instruction(Opcode.STORE_VAR, 0), # Store to var 0
                Instruction(Opcode.LOAD_VAR, 0),  # Load var 0 - OK
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1
        )

        # Should not raise
        validate_bytecode(code)

    def test_function_parameters_initialized(self):
        """Test that function parameters are treated as initialized."""
        # Function with 1 parameter
        code = CodeObject(
            instructions=[
                Instruction(Opcode.STORE_VAR, 0), # Store parameter (from stack)
                Instruction(Opcode.LOAD_VAR, 0),  # Load parameter - OK
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=[],
            code_objects=[],
            param_count=1,
            local_count=1
        )

        # Should not raise
        validate_bytecode(code)

    def test_conditional_both_branches_initialize(self):
        """Test that variable initialized in both branches is OK."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE),           # 0: condition
                Instruction(Opcode.JUMP_IF_FALSE, 5),    # 1: jump to else
                # Then branch
                Instruction(Opcode.LOAD_CONST, 0),       # 2: push value
                Instruction(Opcode.STORE_VAR, 0),     # 3: store to var 0
                Instruction(Opcode.JUMP, 7),             # 4: jump to merge
                # Else branch
                Instruction(Opcode.LOAD_CONST, 0),       # 5: push value
                Instruction(Opcode.STORE_VAR, 0),     # 6: store to var 0
                # Merge point
                Instruction(Opcode.LOAD_VAR, 0),      # 7: load var 0 - OK (both branches stored)
                Instruction(Opcode.RETURN),              # 8
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1
        )

        # Should not raise
        validate_bytecode(code)

    def test_conditional_one_branch_initializes(self):
        """Test that variable initialized in only one branch is caught."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE),           # 0: condition
                Instruction(Opcode.JUMP_IF_FALSE, 5),    # 1: jump to else
                # Then branch
                Instruction(Opcode.LOAD_CONST, 0),       # 2: push value
                Instruction(Opcode.STORE_VAR, 0),     # 3: store to var 0
                Instruction(Opcode.JUMP, 6),             # 4: jump to merge
                # Else branch - doesn't initialize!
                Instruction(Opcode.JUMP, 6),             # 5: jump to merge
                # Merge point
                Instruction(Opcode.LOAD_VAR, 0),      # 6: load var 0 - ERROR (boolean-not initialized on else path)
                Instruction(Opcode.RETURN),              # 7
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1
        )

        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE

    def test_loop_with_initialization(self):
        """Test that variable initialized before loop is OK."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 0),       # 0: push value
                Instruction(Opcode.STORE_VAR, 0),     # 1: store to var 0
                # Loop start
                Instruction(Opcode.LOAD_TRUE),           # 2: condition
                Instruction(Opcode.JUMP_IF_FALSE, 6),    # 3: exit loop
                # Loop body
                Instruction(Opcode.LOAD_VAR, 0),      # 4: load var 0 - OK
                Instruction(Opcode.RETURN),              # 5: return (pop from stack)
                # After loop
                Instruction(Opcode.LOAD_CONST, 1),       # 6: push value
                Instruction(Opcode.RETURN),              # 7: return
            ],
            constants=[MenaiInteger(42), MenaiInteger(99)],
            names=[],
            code_objects=[],
            local_count=1
        )

        # Should not raise
        validate_bytecode(code)




class TestPatchClosureValidation:
    """
    Tests for the three PATCH_CLOSURE validation checks added to
    _validate_initialization:

      1. The target slot (arg1) must be initialised.
      2. The target slot must definitively hold a closure (created by MAKE_CLOSURE).
      3. The capture_slot (arg2) must be within the free_vars range of that closure.
    """

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _make_closure_code(n_free_vars: int, name: str = "<inner>") -> CodeObject:
        """Return a minimal closed CodeObject with n_free_vars free variable slots."""
        return CodeObject(
            instructions=[
                Instruction(Opcode.ENTER, 1),
                Instruction(Opcode.LOAD_VAR, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=[],
            code_objects=[],
            param_count=1,
            local_count=1 + n_free_vars,
            free_vars=[f"fv{i}" for i in range(n_free_vars)],
            name=name,
        )

    # ------------------------------------------------------------------
    # Valid cases
    # ------------------------------------------------------------------

    def test_valid_patch_closure(self):
        """PATCH_CLOSURE against a known closure slot with a valid capture_slot passes."""
        inner = self._make_closure_code(2)
        # Outer frame:
        #   0: MAKE_CLOSURE 0 0   (create skeleton with 0 pre-captured values)
        #   1: STORE_VAR 0        (slot 0 now holds the closure)
        #   2: LOAD_CONST 0       (value to patch in)
        #   3: PATCH_CLOSURE 0 0  (patch capture slot 0 — valid: inner has 2 free vars)
        #   4: LOAD_CONST 0
        #   5: PATCH_CLOSURE 0 1  (patch capture slot 1 — valid)
        #   6: LOAD_VAR 0
        #   7: RETURN
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, 0, 0),
                Instruction(Opcode.STORE_VAR, 0),
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.PATCH_CLOSURE, 0, 0),
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.PATCH_CLOSURE, 0, 1),
                Instruction(Opcode.LOAD_VAR, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[inner],
            local_count=1,
        )
        validate_bytecode(code)  # must not raise

    def test_valid_patch_closure_single_free_var(self):
        """PATCH_CLOSURE with exactly one free var and capture_slot=0 passes."""
        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, 0, 0),
                Instruction(Opcode.STORE_VAR, 0),
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.PATCH_CLOSURE, 0, 0),
                Instruction(Opcode.LOAD_VAR, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner],
            local_count=1,
        )
        validate_bytecode(code)  # must not raise

    # ------------------------------------------------------------------
    # Gap 1: target slot is uninitialised
    # ------------------------------------------------------------------

    def test_patch_closure_uninitialized_slot(self):
        """PATCH_CLOSURE against an uninitialised slot is rejected."""
        inner = self._make_closure_code(1)
        # slot 0 is never written — PATCH_CLOSURE reads it without a prior STORE_VAR
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.PATCH_CLOSURE, 0, 0),   # slot 0 never initialised
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE
        assert "PATCH_CLOSURE" in exc_info.value.message

    def test_patch_closure_slot_initialized_only_on_one_branch(self):
        """PATCH_CLOSURE is rejected when the slot is only initialised on one branch."""
        inner = self._make_closure_code(1)
        # Branch A (instr 2-4): creates closure, stores to slot 0
        # Branch B (instr 5):   skips creation
        # Merge (instr 6):      PATCH_CLOSURE — slot 0 may be uninitialised
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE),            # 0
                Instruction(Opcode.JUMP_IF_FALSE, 5),     # 1: branch to 5
                Instruction(Opcode.MAKE_CLOSURE, 0, 0),   # 2
                Instruction(Opcode.STORE_VAR, 0),         # 3
                Instruction(Opcode.JUMP, 6),              # 4
                Instruction(Opcode.JUMP, 6),              # 5: no STORE_VAR
                Instruction(Opcode.LOAD_CONST, 0),        # 6
                Instruction(Opcode.PATCH_CLOSURE, 0, 0),  # 7: slot 0 may be uninit
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE

    # ------------------------------------------------------------------
    # Gap 2: target slot does not hold a closure
    # ------------------------------------------------------------------

    def test_patch_closure_slot_holds_constant_not_closure(self):
        """PATCH_CLOSURE against a slot holding a plain constant is rejected."""
        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 0),        # push a plain integer
                Instruction(Opcode.STORE_VAR, 0),         # slot 0 = integer (not a closure)
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.PATCH_CLOSURE, 0, 0),  # slot 0 is not a closure
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[inner],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS
        assert "not known to hold a closure" in exc_info.value.message

    def test_patch_closure_slot_overwritten_after_make_closure(self):
        """PATCH_CLOSURE is rejected when a second STORE_VAR overwrites the closure slot."""
        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, 0, 0),   # create closure
                Instruction(Opcode.STORE_VAR, 0),         # slot 0 = closure
                Instruction(Opcode.LOAD_CONST, 0),        # push integer
                Instruction(Opcode.STORE_VAR, 0),         # slot 0 = integer (overwrites closure)
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.PATCH_CLOSURE, 0, 0),  # slot 0 no longer a closure
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[inner],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS
        assert "not known to hold a closure" in exc_info.value.message

    def test_patch_closure_slot_holds_different_closures_on_branches(self):
        """PATCH_CLOSURE is rejected when two branches store different closures in the slot."""
        inner_a = self._make_closure_code(1, name="<inner-a>")
        inner_b = self._make_closure_code(1, name="<inner-b>")
        # Branch A stores code_objects[0], branch B stores code_objects[1].
        # At the merge point the validator cannot determine which closure is in
        # slot 0, so PATCH_CLOSURE must be rejected.
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE),            # 0
                Instruction(Opcode.JUMP_IF_FALSE, 5),     # 1: branch to 5
                Instruction(Opcode.MAKE_CLOSURE, 0, 0),   # 2: closure from code_objects[0]
                Instruction(Opcode.STORE_VAR, 0),         # 3
                Instruction(Opcode.JUMP, 7),              # 4
                Instruction(Opcode.MAKE_CLOSURE, 1, 0),   # 5: closure from code_objects[1]
                Instruction(Opcode.STORE_VAR, 0),         # 6
                Instruction(Opcode.LOAD_CONST, 0),        # 7 (merge)
                Instruction(Opcode.PATCH_CLOSURE, 0, 0),  # 8: ambiguous — which closure?
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner_a, inner_b],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS
        assert "not known to hold a closure" in exc_info.value.message

    # ------------------------------------------------------------------
    # Gap 3: capture_slot out of range
    # ------------------------------------------------------------------

    def test_patch_closure_capture_slot_too_large(self):
        """PATCH_CLOSURE with capture_slot >= n_free_vars is rejected."""
        inner = self._make_closure_code(2)  # free_vars has 2 entries (slots 0 and 1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, 0, 0),
                Instruction(Opcode.STORE_VAR, 0),
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.PATCH_CLOSURE, 0, 2),  # capture_slot=2, but only 0 and 1 exist
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS
        assert "capture_slot" in exc_info.value.message
        assert "out of range" in exc_info.value.message

    def test_patch_closure_capture_slot_zero_free_vars(self):
        """PATCH_CLOSURE against a closure with no free vars is always out of range."""
        inner = self._make_closure_code(0)  # no free vars at all
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, 0, 0),
                Instruction(Opcode.STORE_VAR, 0),
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.PATCH_CLOSURE, 0, 0),  # capture_slot=0, but n_free=0
                Instruction(Opcode.LOAD_CONST, 0),
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS
        assert "capture_slot" in exc_info.value.message


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
