"""Tests for bytecode validator.

This tests the validator's ability to catch various bytecode errors.

ISA summary (register-based LOAD transition):

  Register-based LOAD ops (write to dest register, no stack effect):
    LOAD_NONE dest              — frame.locals[dest] = #none
    LOAD_TRUE dest              — frame.locals[dest] = #t
    LOAD_FALSE dest             — frame.locals[dest] = #f
    LOAD_EMPTY_LIST dest        — frame.locals[dest] = []
    LOAD_CONST dest, src0       — frame.locals[dest] = constants[src0]
    LOAD_NAME  dest, src0       — frame.locals[dest] = globals[names[src0]]

  Stack/register transfer:
    PUSH src0                   — push frame.locals[src0] onto stack  (+1)
    POP  dest                   — pop stack top into frame.locals[dest] (-1)

  ENTER:
    ENTER n                     — pop n args from stack into locals[0..n-1] (-n)
                                  must be first instruction of any function with params
                                  n must equal param_count

  All other ops remain stack-based (pop operands, push result).

  RETURN pops 1 value and returns (terminal).
  CALL dest, src0, src1         — func in register src0, arity in src1, args on stack.
  TAIL_CALL src0, src1          — func in register src0, arity in src1, terminal.
  MAKE_CLOSURE code_idx, capture_count pops capture_count, pushes 1 closure.
  PATCH_CLOSURE var_idx, capture_slot pops 1 value (the captured value to patch in).

Validator initial stack depth = param_count (args pushed by caller before entry).

Index constraints:
  All LOAD ops:  dest < local_count
  PUSH:          src0 < local_count
  POP:           dest < local_count
  ENTER:         src0 == param_count and src0 <= local_count
  PATCH_CLOSURE: src0 < local_count
  CALL/TAIL_CALL/APPLY/TAIL_APPLY: src0 (func register) < local_count

Minimal valid "load and return" sequence:
  LOAD_CONST dest=0, src0=0   (local_count >= 1)
  PUSH src0=0
  RETURN
"""

import pytest

from menai.menai_bytecode import CodeObject, Instruction, Opcode
from menai.menai_vm_bytecode_validator import ValidationError, ValidationErrorType, validate_bytecode
from menai.menai_value import MenaiInteger


class TestBytecodeValidator:
    """Test bytecode validation."""

    # ------------------------------------------------------------------
    # Category 1: Valid simple code
    # ------------------------------------------------------------------

    def test_valid_simple_code(self):
        """Test that valid bytecode passes validation.

        Minimal valid sequence under the register-based ISA:
          LOAD_CONST dest=0, src0=0  — write 42 into r0; no stack effect
          PUSH src0=0                — push r0 onto stack  (depth: 0 → 1)
          RETURN                     — pop 1, terminal     (depth: 1 → 0)
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),  # r0 = constants[0] = 42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                 # stack: 0 → 1
                Instruction(Opcode.RETURN),                       # stack: 1 → 0 (terminal)
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        # Should not raise
        validate_bytecode(code)

    # ------------------------------------------------------------------
    # Category 2: Invalid constant index
    # ------------------------------------------------------------------

    def test_invalid_constant_index(self):
        """Test that LOAD_CONST with an out-of-bounds src0 is caught.

        LOAD_CONST dest=0, src0=5 — src0=5 is out of bounds (only 1 constant).
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=5),  # src0=5 out of bounds
                Instruction(Opcode.PUSH, src0=0),
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS
        assert "Constant index" in exc_info.value.message

    # ------------------------------------------------------------------
    # Category 3: Invalid name index
    # ------------------------------------------------------------------

    def test_invalid_name_index(self):
        """Test that LOAD_NAME with an out-of-bounds src0 is caught.

        LOAD_NAME dest=0, src0=3 — src0=3 is out of bounds (only 1 name).
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_NAME, dest=0, src0=3),  # src0=3 out of bounds
                Instruction(Opcode.PUSH, src0=0),
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=["x"],
            code_objects=[],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS
        assert "Name index" in exc_info.value.message

    # ------------------------------------------------------------------
    # Category 4: Invalid jump target
    # ------------------------------------------------------------------

    def test_invalid_jump_target(self):
        """Test that a JUMP to a non-existent instruction is caught.

        JUMP src0=100 — target 100 does not exist (only 1 instruction).
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.JUMP, src0=100),  # target 100 out of bounds
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=0,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INVALID_JUMP_TARGET
        assert "Jump target" in exc_info.value.message

    # ------------------------------------------------------------------
    # Category 5: Invalid variable index
    # ------------------------------------------------------------------

    def test_invalid_variable_index(self):
        """Test that PUSH with src0 >= local_count is caught.

        PUSH src0=5 — index 5 is out of bounds (local_count=2).
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.PUSH, src0=5),  # src0=5 >= local_count=2
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS
        assert "Variable index" in exc_info.value.message

    # ------------------------------------------------------------------
    # Category 6: Stack underflow
    # ------------------------------------------------------------------

    def test_stack_underflow(self):
        """Test that RETURN with an out-of-bounds src0 register is caught.

        RETURN src0=0 with local_count=0: the initialization pass fires first
        (register 0 is uninitialized) and raises UNINITIALIZED_VARIABLE.
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.RETURN, src0=0),  # src0=0 but local_count=0 → out of bounds
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=0,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE
        assert "RETURN source register" in exc_info.value.message

    # ------------------------------------------------------------------
    # Category 7: Stack underflow in call
    # ------------------------------------------------------------------

    def test_stack_underflow_in_call(self):
        """Test that CALL with insufficient stack items is caught.

        Sequence (local_count=2, initial depth=0):
          LOAD_CONST dest=0, src0=0        — r0=42 (the "function"); stack: 0
          PUSH src0=0                      — stack: 0 → 1 (one arg pushed)
          CALL dest=1, src0=0, src1=3      — func=r0, arity=3; needs 3 args on stack, has 1 → STACK_UNDERFLOW
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),       # r0=42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                      # stack: 0 → 1 (one arg)
                Instruction(Opcode.CALL, dest=1, src0=0, src1=3),      # func=r0, arity=3; needs 3, has 1
                Instruction(Opcode.RETURN),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.STACK_UNDERFLOW

    # ------------------------------------------------------------------
    # Category 8: Consistent stack depth at merge point
    # ------------------------------------------------------------------

    def test_consistent_stack_depth_at_merge(self):
        """Test that two paths with equal depth at a merge point pass validation.

        Control flow (local_count=1, param_count=0):

          0: LOAD_TRUE dest=0                  — r0=#t
          1: JUMP_IF_FALSE src0=0, src1=5      — read r0; jump→5, fall→2

          Fall-through branch:
          2: LOAD_CONST dest=0, src0=0 — r0=42; stack: 0
          3: PUSH src0=0               — stack: 0 → 1
          4: JUMP src0=7               — stack: 1; jump to 7

          Jump branch:
          5: LOAD_CONST dest=0, src0=0 — r0=42; stack: 0
          6: PUSH src0=0               — stack: 0 → 1; falls to 7

          Merge point (both arrive with depth=1):
          7: RETURN                    — depth=1 ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=0),              # 0: r0=#t; stack: 0
                Instruction(Opcode.JUMP_IF_FALSE, src0=0, src1=5),  # 1: read r0; jump→5, fall→2
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 2: r0=42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                   # 3: stack: 0 → 1
                Instruction(Opcode.JUMP, src0=7),                   # 4: stack: 1; jump to 7
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 5: r0=42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                   # 6: stack: 0 → 1; falls to 7
                Instruction(Opcode.RETURN),                         # 7: depth=1 from both paths ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        # Should not raise — both paths arrive at instruction 8 with depth=1
        validate_bytecode(code)

    # ------------------------------------------------------------------
    # Category 9: Inconsistent stack depth at merge point
    # ------------------------------------------------------------------

    def test_inconsistent_stack_depth_at_merge(self):
        """Test that two paths with different depths at a merge point are caught.

        Control flow (local_count=1, param_count=0):

          0: LOAD_TRUE dest=0                  — r0=#t
          1: JUMP_IF_FALSE src0=0, src1=5      — read r0; jump→5, fall→2

          Fall-through branch:
          2: LOAD_CONST dest=0, src0=0 — r0=42; stack: 0
          3: PUSH src0=0               — stack: 0 → 1
          4: JUMP src0=6               — stack: 1; jump to 6

          Jump branch:
          5: JUMP src0=6               — stack: 0; jump to 6

          Merge point (fall-through depth=1, jump depth=0):
          6: RETURN                    — STACK_INCONSISTENT (or STACK_UNDERFLOW)
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=0),            # 0: r0=#t; stack: 0
                Instruction(Opcode.JUMP_IF_FALSE, src0=0, src1=5),  # 1: read r0; jump→5, fall→2
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 2: r0=42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                    # 3: stack: 0 → 1
                Instruction(Opcode.JUMP, src0=6),                    # 4: stack: 1; jump to 6
                Instruction(Opcode.JUMP, src0=6),                    # 5: stack: 0; jump to 6
                Instruction(Opcode.RETURN),                          # 6: depth=1 vs 0 → inconsistent
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        # Either STACK_INCONSISTENT or STACK_UNDERFLOW depending on traversal order
        assert exc_info.value.error_type in (
            ValidationErrorType.STACK_INCONSISTENT,
            ValidationErrorType.STACK_UNDERFLOW,
        )

    # ------------------------------------------------------------------
    # Category 10: Valid conditional jump
    # ------------------------------------------------------------------

    def test_valid_conditional_jump(self):
        """Test that a valid if/else (both branches return) passes validation.

        Control flow (local_count=1, param_count=0):

          0: LOAD_TRUE dest=0                  — r0=#t
          1: JUMP_IF_FALSE src0=0, src1=5      — read r0; jump→5, fall→2

          Then branch:
          2: LOAD_CONST dest=0, src0=0 — r0=1; stack: 0
          3: PUSH src0=0               — stack: 0 → 1
          4: RETURN                    — terminal ✓

          Else branch:
          5: LOAD_CONST dest=0, src0=1 — r0=2; stack: 0
          6: PUSH src0=0               — stack: 0 → 1
          7: RETURN                    — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=0),              # 0: r0=#t; stack: 0
                Instruction(Opcode.JUMP_IF_FALSE, src0=0, src1=5),  # 1: read r0; jump→5, fall→2
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 2: r0=1; stack: 0
                Instruction(Opcode.PUSH, src0=0),                   # 3: stack: 0 → 1
                Instruction(Opcode.RETURN),                         # 4: terminal ✓
                Instruction(Opcode.LOAD_CONST, dest=0, src0=1),     # 5: r0=2; stack: 0
                Instruction(Opcode.PUSH, src0=0),                   # 6: stack: 0 → 1
                Instruction(Opcode.RETURN),                         # 7: terminal ✓
            ],
            constants=[MenaiInteger(1), MenaiInteger(2)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        # Should not raise
        validate_bytecode(code)

    # ------------------------------------------------------------------
    # Category 11: Valid backward jump (loop)
    # ------------------------------------------------------------------

    def test_valid_loop(self):
        """Test that a valid loop with a consistent stack depth at the back-edge passes.

        The loop header is instruction 0.  Both the initial entry and
        the back-edge from instruction 2 arrive with the same depth,
        so the validator accepts the backward jump.

        Control flow (local_count=1, param_count=0):

          0: LOAD_TRUE dest=0                  — r0=#t  ← loop header
          1: JUMP_IF_FALSE src0=0, src1=3      — read r0; jump→3, fall→2
          2: JUMP src0=0                       — back to 0 ✓
          3: LOAD_CONST dest=0, src0=0         — r0=42; stack: 0
          4: PUSH src0=0                       — stack: 0 → 1
          5: RETURN                            — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=0),              # 0: r0=#t; stack: 0  ← loop header
                Instruction(Opcode.JUMP_IF_FALSE, src0=0, src1=3),  # 1: read r0; jump→3, fall→2
                Instruction(Opcode.JUMP, src0=0),                   # 2: back to 0 ✓
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 3: r0=42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                   # 4: stack: 0 → 1
                Instruction(Opcode.RETURN),                         # 5: terminal ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        # Should not raise
        validate_bytecode(code)

    # ------------------------------------------------------------------
    # Category 12: Valid MAKE_CLOSURE
    # ------------------------------------------------------------------

    def test_valid_make_closure(self):
        """Test that valid MAKE_CLOSURE passes validation.

        Lambda (param_count=1, local_count=1):
          Initial stack depth = param_count = 1 (arg pushed by caller).
          0: ENTER src0=1  — pops 1 arg from stack into slot 0; depth: 1 → 0; slot 0 initialized
          1: PUSH src0=0   — slot 0 initialized ✓; stack: 0 → 1
          2: RETURN        — terminal ✓

        Outer code (local_count=2, param_count=0, initial depth=0):
          0: LOAD_CONST dest=0, src0=0   — r0=42; stack: 0
          1: PUSH src0=0                 — stack: 0 → 1  (captured value)
          2: MAKE_CLOSURE dest=1, src0=0, src1=1 — pops 1 capture, writes closure to r1; depth: 1 → 0
          3: PUSH src0=1                 — push closure onto stack; depth: 0 → 1
          4: RETURN                      — terminal ✓
        """
        lambda_code = CodeObject(
            instructions=[
                Instruction(Opcode.ENTER, src0=1),   # 0: pops 1 arg; depth: 1 → 0; slot 0 initialized
                Instruction(Opcode.PUSH, src0=0),    # 1: stack: 0 → 1
                Instruction(Opcode.RETURN),          # 2: terminal ✓
            ],
            constants=[],
            names=[],
            code_objects=[],
            param_count=1,
            local_count=1,
        )

        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),       # 0: r0=42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                     # 1: stack: 0 → 1 (captured value)
                Instruction(Opcode.MAKE_CLOSURE, dest=1, src0=0, src1=1),  # 2: pops 1 capture, writes closure to r1; depth: 1 → 0
                Instruction(Opcode.PUSH, src0=1),                     # 3: push closure; depth: 0 → 1
                Instruction(Opcode.RETURN),                           # 4: terminal ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[lambda_code],
            local_count=2,
        )
        # Should not raise
        validate_bytecode(code)

    # ------------------------------------------------------------------
    # Category 13: Empty code object
    # ------------------------------------------------------------------

    def test_empty_code_object(self):
        """Test that an empty code object (no instructions) is caught."""
        code = CodeObject(
            instructions=[],
            constants=[],
            names=[],
            code_objects=[],
            local_count=0,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INVALID_OPCODE
        assert "no instructions" in exc_info.value.message

    # ------------------------------------------------------------------
    # Category 14: Missing RETURN
    # ------------------------------------------------------------------

    def test_missing_return(self):
        """Test that code that falls off the end without RETURN is caught.

        Sequence (local_count=1):
          0: LOAD_CONST dest=0, src0=0 — r0=42; stack: 0
          1: PUSH src0=0               — stack: 0 → 1
          (no RETURN — falls off end → MISSING_RETURN)
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),  # r0=42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                # stack: 0 → 1
                # Missing RETURN
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.MISSING_RETURN

    # ------------------------------------------------------------------
    # Category 15: TAIL_CALL as terminal
    # ------------------------------------------------------------------

    def test_tail_call_is_terminal(self):
        """Test that TAIL_CALL is treated as terminal (no successors needed).

        Unreachable instructions after TAIL_CALL are structurally valid.

        Sequence (local_count=2, names=["f"], constants=[42]):
          0: LOAD_NAME dest=0, src0=0   — r0=f; stack: 0
          1: LOAD_CONST dest=1, src0=0  — r1=42; stack: 0
          2: PUSH src0=1                — stack: 0 → 1 (one arg)
          3: TAIL_CALL src0=0, src1=1   — func=r0, arity=1; pops 1 arg; terminal ✓
          4: LOAD_CONST dest=0, src0=0  — unreachable; structurally valid
          5: PUSH src0=0                — unreachable; structurally valid
          6: RETURN                     — unreachable; structurally valid
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_NAME, dest=0, src0=0),    # 0: r0=f; stack: 0
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),   # 1: r1=42; stack: 0
                Instruction(Opcode.PUSH, src0=1),                  # 2: stack: 0 → 1 (one arg)
                Instruction(Opcode.TAIL_CALL, src0=0, src1=1),    # 3: func=r0, arity=1; terminal ✓
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),   # 4: unreachable
                Instruction(Opcode.PUSH, src0=0),                 # 5: unreachable
                Instruction(Opcode.RETURN),                       # 6: unreachable
            ],
            constants=[MenaiInteger(42)],
            names=["f"],
            code_objects=[],
            local_count=2,
        )
        # Should not raise
        validate_bytecode(code)

    # ------------------------------------------------------------------
    # Category 16: Nested code validation
    # ------------------------------------------------------------------

    def test_nested_code_validation(self):
        """Test that invalid nested code objects are caught recursively.

        The nested lambda has LOAD_CONST dest=0, src0=99 but an empty constant
        pool — this triggers INDEX_OUT_OF_BOUNDS during nested validation.

        The outer code is structurally valid (MAKE_CLOSURE with 0 captures,
        then RETURN), but validation of nested code objects runs first.
        """
        invalid_lambda = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=99),  # src0=99 out of bounds
                Instruction(Opcode.PUSH, src0=0),
                Instruction(Opcode.RETURN),
            ],
            constants=[],   # empty — src0=99 is invalid
            names=[],
            code_objects=[],
            local_count=1,
        )

        code = CodeObject(
            instructions=[
                # MAKE_CLOSURE with 0 captures: stack effect (0, 1)
                Instruction(Opcode.MAKE_CLOSURE, src0=0, src1=0),  # pushes closure; depth: 0 → 1
                Instruction(Opcode.RETURN),                         # depth: 1 → 0 (terminal) ✓
            ],
            constants=[],
            names=[],
            code_objects=[invalid_lambda],
            local_count=0,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS

    # ------------------------------------------------------------------
    # Category 17: Valid variable initialization
    # ------------------------------------------------------------------

    def test_valid_variable_initialization(self):
        """Test that LOAD to register followed by PUSH passes the initialization check.

        Sequence (local_count=1):
          0: LOAD_CONST dest=0, src0=0 — r0=42; r0 is now initialized
          1: PUSH src0=0               — r0 is initialized ✓; stack: 0 → 1
          2: RETURN                    — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),  # r0=42; r0 initialized
                Instruction(Opcode.PUSH, src0=0),                 # r0 initialized ✓; stack: 0 → 1
                Instruction(Opcode.RETURN),                       # terminal ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        # Should not raise
        validate_bytecode(code)

    # ------------------------------------------------------------------
    # Category 18: Uninitialized variable
    # ------------------------------------------------------------------

    def test_uninitialized_variable(self):
        """Test that PUSH of a slot that was never written is caught.

        Sequence (local_count=1):
          0: PUSH src0=0 — r0 was never written → UNINITIALIZED_VARIABLE
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.PUSH, src0=0),  # r0 never initialized → error
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE

    # ------------------------------------------------------------------
    # Category 19: Valid ENTER
    # ------------------------------------------------------------------

    def test_valid_enter(self):
        """Test that a function with parameters using ENTER passes validation.

        Function (param_count=1, local_count=1):
          Initial stack depth = param_count = 1 (arg pushed by caller).
          0: ENTER src0=1 — pops 1 arg from stack into slot 0; depth: 1 → 0; slot 0 initialized
          1: PUSH src0=0  — slot 0 initialized ✓; stack: 0 → 1
          2: RETURN       — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.ENTER, src0=1),  # 0: pops 1; depth: 1 → 0; slot 0 initialized
                Instruction(Opcode.PUSH, src0=0),   # 1: stack: 0 → 1
                Instruction(Opcode.RETURN),         # 2: terminal ✓
            ],
            constants=[],
            names=[],
            code_objects=[],
            param_count=1,
            local_count=1,
        )
        # Should not raise
        validate_bytecode(code)

    # ------------------------------------------------------------------
    # Category 20: Invalid ENTER count
    # ------------------------------------------------------------------

    def test_invalid_enter_count(self):
        """Test that ENTER n where n != param_count is caught.

        Function (param_count=1, local_count=2):
          ENTER src0=2 — n=2 does not match param_count=1 → INVALID_VARIABLE_ACCESS
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.ENTER, src0=2),  # n=2 != param_count=1 → error
                Instruction(Opcode.PUSH, src0=0),
                Instruction(Opcode.RETURN),
            ],
            constants=[],
            names=[],
            code_objects=[],
            param_count=1,
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS

    # ------------------------------------------------------------------
    # Additional initialization tests
    # ------------------------------------------------------------------

    def test_initialized_variable_via_pop(self):
        """Test that a variable initialized via POP (stack → register) passes.

        Sequence (local_count=1):
          0: LOAD_CONST dest=0, src0=0 — r0=42; stack: 0
          1: PUSH src0=0               — stack: 0 → 1
          2: POP dest=0                — pops into r0; r0 initialized; stack: 1 → 0
          3: PUSH src0=0               — r0 initialized ✓; stack: 0 → 1
          4: RETURN                    — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),   # r0=42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                 # stack: 0 → 1
                Instruction(Opcode.POP, dest=0),                  # r0 initialized; stack: 1 → 0
                Instruction(Opcode.PUSH, src0=0),                 # r0 initialized ✓; stack: 0 → 1
                Instruction(Opcode.RETURN),                       # terminal ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        # Should not raise
        validate_bytecode(code)

    def test_conditional_both_branches_initialize(self):
        """Test that a variable initialized in both branches is OK at the merge point.

        Control flow (local_count=1, param_count=0):

          0: LOAD_TRUE dest=0                  — r0=#t
          1: JUMP_IF_FALSE src0=0, src1=5      — read r0; jump→5, fall→2

          Then branch:
          2: LOAD_CONST dest=0, src0=0 — r0=42; stack: 0
          3: PUSH src0=0               — stack: 0 → 1
          4: JUMP src0=7               — stack: 1; jump to 7

          Else branch:
          5: LOAD_CONST dest=0, src0=0 — r0=42; stack: 0
          6: PUSH src0=0               — stack: 0 → 1; falls to 7

          Merge (depth=1, r0 initialized on both paths):
          7: RETURN                    — depth=1 ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=0),              # 0: r0=#t; stack: 0
                Instruction(Opcode.JUMP_IF_FALSE, src0=0, src1=5),  # 1: read r0; jump→5, fall→2
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 2: r0=42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                   # 3: stack: 0 → 1
                Instruction(Opcode.JUMP, src0=7),                   # 4: stack: 1; jump to 7
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 5: r0=42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                   # 6: stack: 0 → 1; falls to 7
                Instruction(Opcode.RETURN),                         # 7: depth=1 ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        # Should not raise
        validate_bytecode(code)

    def test_conditional_one_branch_initializes(self):
        """Test that PUSH of a variable initialized in only one branch is caught.

        r0 starts uninitialised; r1 holds the condition.
        The then-branch initializes r0; the else-branch does not.

        Control flow (local_count=2, param_count=0):

          0: LOAD_TRUE dest=1                  — r1=#t
          1: JUMP_IF_FALSE src0=1, src1=4      — read r1; jump→4, fall→2

          Then branch: initializes r0
          2: LOAD_CONST dest=0, src0=0 — r0=42; stack: 0
          3: JUMP src0=5               — stack: 0; jump to 5

          Else branch: does NOT initialize r0
          4: JUMP src0=5               — stack: 0; jump to 5

          Merge:
          5: PUSH src0=0               — r0 may be uninitialized → UNINITIALIZED_VARIABLE
          6: RETURN
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=1),              # 0: r1=#t; r0 untouched; stack: 0
                Instruction(Opcode.JUMP_IF_FALSE, src0=1, src1=4),  # 1: read r1; jump→4, fall→2
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 2: r0=42; stack: 0
                Instruction(Opcode.JUMP, src0=5),                   # 3: stack: 0; jump to 5
                Instruction(Opcode.JUMP, src0=5),                   # 4: stack: 0; jump to 5 (no init)
                Instruction(Opcode.PUSH, src0=0),                   # 5: r0 may be uninit → error
                Instruction(Opcode.RETURN),                         # 6
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE

    def test_loop_with_initialization_before_loop(self):
        """Test that a variable initialized at the loop header is accessible inside it.

        r0 is initialized by LOAD_TRUE at instruction 0 on every iteration.

        Control flow (local_count=1, param_count=0):

          0: LOAD_TRUE dest=0                  — r0=#t  ← loop header
          1: JUMP_IF_FALSE src0=0, src1=3      — read r0; jump→3, fall→2
          2: JUMP src0=0                       — back to 0 ✓
          3: LOAD_CONST dest=0, src0=0         — r0=42; stack: 0
          4: PUSH src0=0                       — stack: 0 → 1
          5: RETURN                            — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=0),              # 0: r0=#t; stack: 0  ← loop header
                Instruction(Opcode.JUMP_IF_FALSE, src0=0, src1=3),  # 1: read r0; jump→3, fall→2
                Instruction(Opcode.JUMP, src0=0),                   # 2: back to 0 ✓
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 3: r0=42; stack: 0
                Instruction(Opcode.PUSH, src0=0),                   # 4: stack: 0 → 1
                Instruction(Opcode.RETURN),                         # 5: terminal ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        # Should not raise
        validate_bytecode(code)


class TestPatchClosureValidation:
    """
    Tests for PATCH_CLOSURE validation in _validate_initialization.

    PATCH_CLOSURE src0=closure_reg, src1=value_reg, src2=capture_idx has three requirements:
      1. closure_reg (src0) must refer to an initialized slot holding a closure.
      2. value_reg (src1) must refer to an initialized slot.
      3. capture_idx (src2) must be < len(code_objects[code_index].free_vars).

    At merge points the closure map is intersected conservatively: a slot is
    only kept if both incoming paths agree on the same code_object index.

    Register allocation convention in these tests:
      Slot 0 — holds the closure (written directly by MAKE_CLOSURE dest=0).
      Slot 1 — scratch register for values being patched in (LOAD_CONST dest=1).
      local_count >= 2 whenever both slots are used.

    Using slot 1 for the patch value avoids clobbering the closure in slot 0.
    """

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _make_closure_code(n_free_vars: int, name: str = "<inner>") -> CodeObject:
        """Return a minimal closed CodeObject with n_free_vars free variable slots.

        The lambda has param_count=1 and uses ENTER to pop its argument.
        Captured-value slots are param_count .. param_count+n_free_vars-1.
        """
        return CodeObject(
            instructions=[
                Instruction(Opcode.ENTER, src0=1),   # pops 1 arg; depth: 1 → 0; slot 0 initialized
                Instruction(Opcode.PUSH, src0=0),    # stack: 0 → 1
                Instruction(Opcode.RETURN),           # terminal ✓
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
    # Category 21: Valid PATCH_CLOSURE
    # ------------------------------------------------------------------

    def test_valid_patch_closure(self):
        """PATCH_CLOSURE against a known closure slot with valid capture slots passes.

        Outer frame (local_count=2):
          Slot 0: holds the closure (written directly by MAKE_CLOSURE dest=0).
          Slot 1: scratch register for the values being patched in.

          0: MAKE_CLOSURE dest=0, src0=0, src1=0 — r0=closure; slot 0 tracked as closure
          1: LOAD_CONST dest=1, src0=0            — r1=42
          2: PATCH_CLOSURE src0=0, src1=1, src2=0 — r0 is closure ✓; capture_idx=0 < 2 ✓
          3: LOAD_CONST dest=1, src0=0            — r1=42
          4: PATCH_CLOSURE src0=0, src1=1, src2=1 — r0 is closure ✓; capture_idx=1 < 2 ✓
          5: PUSH src0=0                          — stack: 0 → 1
          6: RETURN                               — terminal ✓
        """
        inner = self._make_closure_code(2)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),    # 0: r0=closure
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),              # 1: r1=42
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=1, src2=0),   # 2: cap 0 < 2 ✓
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),              # 3: r1=42
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=1, src2=1),   # 4: cap 1 < 2 ✓
                Instruction(Opcode.PUSH, src0=0),                            # 5: stack: 0 → 1
                Instruction(Opcode.RETURN),                                  # 6: terminal ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[inner],
            local_count=2,
        )
        validate_bytecode(code)  # must not raise

    def test_valid_patch_closure_single_free_var(self):
        """PATCH_CLOSURE with exactly one free var and capture_slot=0 passes.

        Outer frame (local_count=2):
          0: MAKE_CLOSURE dest=0, src0=0, src1=0  — r0=closure
          1: LOAD_CONST dest=1, src0=0             — r1=1
          2: PATCH_CLOSURE src0=0, src1=1, src2=0  — capture_idx=0 < 1 ✓
          3: PUSH src0=0                           — stack: 0 → 1
          4: RETURN                                — terminal ✓
        """
        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),   # 0: r0=closure
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 1: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=1, src2=0),  # 2: cap 0 < 1 ✓
                Instruction(Opcode.PUSH, src0=0),                           # 3: stack: 0 → 1
                Instruction(Opcode.RETURN),                                 # 4: terminal ✓
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner],
            local_count=2,
        )
        validate_bytecode(code)  # must not raise

    # ------------------------------------------------------------------
    # Category 22: Invalid PATCH_CLOSURE — uninitialized slot
    # ------------------------------------------------------------------

    def test_patch_closure_uninitialized_slot(self):
        """PATCH_CLOSURE against an uninitialised slot is rejected.

        Slot 0 is never written before PATCH_CLOSURE src0=0.

        Sequence (local_count=2):
          0: LOAD_CONST dest=1, src0=0             — r1=1 (slot 0 never written)
          1: PATCH_CLOSURE src0=0, src1=1, src2=0  — slot 0 never initialized → UNINITIALIZED_VARIABLE
          2: LOAD_CONST dest=1, src0=0
          3: PUSH src0=1
          4: RETURN
        """
        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 0: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=1, src2=0),  # 1: slot 0 never init → error
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 2
                Instruction(Opcode.PUSH, src0=1),                           # 3
                Instruction(Opcode.RETURN),                                 # 4
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE
        assert "PATCH_CLOSURE" in exc_info.value.message

    def test_patch_closure_slot_initialized_only_on_one_branch(self):
        """PATCH_CLOSURE is rejected when the slot is only initialised on one branch.

        Branch A (instr 3-5): creates closure, stores to slot 0.
        Branch B (instr 6):   skips creation — slot 0 remains uninitialized.
        Merge:                PATCH_CLOSURE — slot 0 may be uninitialised → error.

        Control flow (local_count=2, initial depth=0):
          0: LOAD_TRUE dest=1            — r1=#t; stack: 0
          1: JUMP_IF_FALSE src0=1, src1=4 — read r1; jump→4, fall→2

          Branch A (depth=0):
          2: MAKE_CLOSURE dest=0, src0=0, src1=0 — r0=closure
          3: JUMP src0=5                         — jump to 5

          Branch B (depth=0):
          4: JUMP src0=5                         — jump to 5 (slot 0 not initialized)

          Merge (slot 0 initialized only on branch A):
          5: LOAD_CONST dest=1, src0=0            — r1=1
          6: PATCH_CLOSURE src0=0, src1=1, src2=0 — slot 0 may be uninit → UNINITIALIZED_VARIABLE
          7: LOAD_CONST dest=1, src0=0
          8: PUSH src0=1
          9: RETURN
        """
        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=1),               # 0: r1=#t; stack: 0
                Instruction(Opcode.JUMP_IF_FALSE, src0=1, src1=4),   # 1: jump→4, fall→2
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),  # 2: r0=closure
                Instruction(Opcode.JUMP, src0=5),                    # 3: jump to 5
                Instruction(Opcode.JUMP, src0=5),                    # 4: jump to 5 (no init)
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),      # 5: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=1, src2=0), # 6: may be uninit → error
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),      # 7
                Instruction(Opcode.PUSH, src0=1),                    # 8
                Instruction(Opcode.RETURN),                          # 9
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE

    def test_patch_closure_slot_holds_constant_not_closure(self):
        """PATCH_CLOSURE against a slot holding a plain constant is rejected.

        Slot 0 is written by LOAD_CONST (not MAKE_CLOSURE), so it is not
        tracked as a closure in the closure map.

        Sequence (local_count=2):
          0: LOAD_CONST dest=0, src0=0    — slot 0 = 42 (plain integer, not a closure)
          1: LOAD_CONST dest=1, src0=0             — r1=42
          2: PATCH_CLOSURE src0=0, src1=1, src2=0  — slot 0 not a closure → INVALID_VARIABLE_ACCESS
          3: LOAD_CONST dest=1, src0=0
          4: PUSH src0=1
          5: RETURN
        """
        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 0: slot 0 = 42 (not a closure)
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 1: r1=42
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=1, src2=0),  # 2: not closure → error
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 3
                Instruction(Opcode.PUSH, src0=1),                           # 4
                Instruction(Opcode.RETURN),                                 # 5
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[inner],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS
        assert "not known to hold a closure" in exc_info.value.message

    def test_patch_closure_slot_overwritten_after_make_closure(self):
        """PATCH_CLOSURE is rejected when a second write overwrites the closure slot."""

        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),   # 0: r0=closure
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),             # 1: r0=42 (overwrites closure)
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 2: r1=42
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=1, src2=0),  # 3: not closure → error
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 4
                Instruction(Opcode.PUSH, src0=1),                           # 5
                Instruction(Opcode.RETURN),                                 # 6
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[inner],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS
        assert "not known to hold a closure" in exc_info.value.message

    def test_patch_closure_slot_holds_different_closures_on_branches(self):
        """PATCH_CLOSURE is rejected when two branches store different closures in the slot.

        Branch A stores code_objects[0], branch B stores code_objects[1].
        At the merge point the validator cannot determine which closure is in
        slot 0 (the closure maps disagree), so PATCH_CLOSURE must be rejected.

        Control flow (local_count=2, initial depth=0):
          0: LOAD_TRUE dest=1            — r1=#t; stack: 0
          1: JUMP_IF_FALSE src0=1, src1=4 — read r1; jump→4, fall→2

          Branch A (depth=0):
          2: MAKE_CLOSURE dest=0, src0=0, src1=0 — r0=closure[0]
          3: JUMP src0=6                         — jump to 6

          Branch B (depth=0):
          4: MAKE_CLOSURE dest=0, src0=1, src1=0 — r0=closure[1]
          5: JUMP src0=6                         — falls to 6

          Merge (slot 0 holds different closures on each path):
          6: LOAD_CONST dest=1, src0=0            — r1=1
          7: PATCH_CLOSURE src0=0, src1=1, src2=0 — ambiguous closure → INVALID_VARIABLE_ACCESS
          8: LOAD_CONST dest=1, src0=0
          9: PUSH src0=1
          10: RETURN
        """
        inner_a = self._make_closure_code(1, name="<inner-a>")
        inner_b = self._make_closure_code(1, name="<inner-b>")
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=1),               # 0: r1=#t; stack: 0
                Instruction(Opcode.JUMP_IF_FALSE, src0=1, src1=4),   # 1: jump→4, fall→2
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),  # 2: r0=closure[0]
                Instruction(Opcode.JUMP, src0=6),                    # 3: jump to 6
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=1, src1=0),  # 4: r0=closure[1]
                Instruction(Opcode.JUMP, src0=6),                    # 5: falls to 6
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),      # 6: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=1, src2=0), # 7: ambiguous → error
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),      # 8
                Instruction(Opcode.PUSH, src0=1),                    # 9
                Instruction(Opcode.RETURN),                          # 10
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner_a, inner_b],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS
        assert "not known to hold a closure" in exc_info.value.message

    def test_patch_closure_capture_slot_too_large(self):
        """PATCH_CLOSURE with capture_slot >= n_free_vars is rejected.

        inner has 2 free vars (capture slots 0 and 1 are valid).
        capture_slot=2 is out of range.

        Sequence (local_count=2):
          0: MAKE_CLOSURE dest=0, src0=0, src1=0  — r0=closure
          1: LOAD_CONST dest=1, src0=0             — r1=1
          2: PATCH_CLOSURE src0=0, src1=1, src2=2  — capture_idx=2 >= n_free=2 → INDEX_OUT_OF_BOUNDS
          3: LOAD_CONST dest=1, src0=0
          4: PUSH src0=1
          5: RETURN
        """
        inner = self._make_closure_code(2)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),   # 0: r0=closure
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 1: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=1, src2=2),  # 2: cap 2 >= n_free=2 → error
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 3
                Instruction(Opcode.PUSH, src0=1),                           # 4
                Instruction(Opcode.RETURN),                                 # 5
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS
        assert "capture_slot" in exc_info.value.message
        assert "out of range" in exc_info.value.message

    def test_patch_closure_capture_slot_zero_free_vars(self):
        """PATCH_CLOSURE against a closure with no free vars is always out of range.

        inner has 0 free vars, so any capture_slot (including 0) is invalid.

        Sequence (local_count=2):
          0: MAKE_CLOSURE dest=0, src0=0, src1=0  — r0=closure
          1: LOAD_CONST dest=1, src0=0             — r1=1
          2: PATCH_CLOSURE src0=0, src1=1, src2=0  — capture_idx=0 >= n_free=0 → INDEX_OUT_OF_BOUNDS
          3: LOAD_CONST dest=1, src0=0
          4: PUSH src0=1
          5: RETURN
        """
        inner = self._make_closure_code(0)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),   # 0: r0=closure
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 1: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=1, src2=0),  # 2: cap 0 >= n_free=0 → error
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 3
                Instruction(Opcode.PUSH, src0=1),                           # 4
                Instruction(Opcode.RETURN),                                 # 5
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)
        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS
        assert "capture_slot" in exc_info.value.message


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
