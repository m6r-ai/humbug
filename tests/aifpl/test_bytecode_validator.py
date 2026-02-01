"""Tests for bytecode validator.

This tests the validator's ability to catch various bytecode errors.
"""

import pytest

from aifpl.aifpl_bytecode import CodeObject, Instruction, Opcode
from aifpl.aifpl_bytecode_validator import (
    BytecodeValidator, ValidationError, ValidationErrorType, validate_bytecode
)
from aifpl.aifpl_value import AIFPLInteger, AIFPLString


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
            constants=[AIFPLInteger(42)],
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
            constants=[AIFPLInteger(42)],
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
                Instruction(Opcode.LOAD_VAR, 0, 5),  # Index 5 but local_count is 2
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
                Instruction(Opcode.CALL_FUNCTION, 2),  # Try to call with arity 2 (needs 3 items: func + 2 args)
                Instruction(Opcode.RETURN),
            ],
            constants=[AIFPLInteger(42)],
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
            constants=[AIFPLInteger(42)],
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
            constants=[AIFPLInteger(42)],
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
            constants=[AIFPLInteger(1), AIFPLInteger(2)],
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
            constants=[AIFPLInteger(42)],
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
                Instruction(Opcode.STORE_VAR, 0, 0),     # Store parameter
                Instruction(Opcode.LOAD_VAR, 0, 0),      # Load parameter
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
            constants=[AIFPLInteger(42)],
            names=[],
            code_objects=[lambda_code],
            local_count=0
        )
        
        # Should not raise
        validate_bytecode(code)

    def test_invalid_builtin_index(self):
        """Test that invalid builtin index is caught."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.CALL_BUILTIN, 999, 0),  # Invalid builtin index
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=0
        )
        
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        
        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS
        assert "Builtin index" in exc_info.value.message

    def test_valid_builtin_call(self):
        """Test that valid builtin call passes validation."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, 0),       # Push arg
                Instruction(Opcode.LOAD_CONST, 1),       # Push arg
                Instruction(Opcode.CALL_BUILTIN, 0, 2),  # Call builtin with 2 args
                Instruction(Opcode.RETURN),
            ],
            constants=[AIFPLInteger(1), AIFPLInteger(2)],
            names=[],
            code_objects=[],
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
            constants=[AIFPLInteger(42)],
            names=[],
            code_objects=[],
            local_count=0
        )
        
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        
        assert exc_info.value.error_type == ValidationErrorType.MISSING_RETURN

    def test_tail_call_is_terminal(self):
        """Test that TAIL_CALL_FUNCTION is treated as terminal (no successors)."""
        # TAIL_CALL_FUNCTION should be terminal, code after it is unreachable but valid
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_NAME, 0),        # Load function
                Instruction(Opcode.LOAD_CONST, 0),       # Load arg
                Instruction(Opcode.TAIL_CALL_FUNCTION, 1),  # Tail call
                Instruction(Opcode.LOAD_CONST, 0),       # Unreachable (but valid)
                Instruction(Opcode.RETURN),              # Unreachable (but valid)
            ],
            constants=[AIFPLInteger(42)],
            names=["f"],
            code_objects=[],
            local_count=0
        )
        
        # Should not raise
        validate_bytecode(code)

    def test_invalid_parent_var_depth(self):
        """Test that LOAD_PARENT_VAR with depth 0 is caught."""
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_PARENT_VAR, 0, 0),  # Depth must be >= 1
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=0
        )
        
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        
        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS
        assert "depth must be >= 1" in exc_info.value.message

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


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
