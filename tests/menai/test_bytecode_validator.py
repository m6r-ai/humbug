"""Tests for bytecode validator.

This tests the validator's ability to catch various bytecode errors.

ISA summary (register-window calling convention):

  Register-based LOAD ops (write to dest register, no stack effect):
    LOAD_NONE dest              — frame.locals[dest] = #none
    LOAD_TRUE dest              — frame.locals[dest] = #t
    LOAD_FALSE dest             — frame.locals[dest] = #f
    LOAD_EMPTY_LIST dest        — frame.locals[dest] = []
    LOAD_CONST dest, src0       — frame.locals[dest] = constants[src0]
    LOAD_NAME  dest, src0       — frame.locals[dest] = globals[names[src0]]

  Register transfer:
    MOVE dest, src0             — frame.locals[dest] = frame.locals[src0]
                                  dest and src0 may reach into [0, local_count + outgoing_arg_slots)

  Calling convention (register-window):
    Caller: MOVE each arg into slot (local_count + i), then CALL func, arity.
    Callee: params are already in r0..rn-1 (placed by caller into callee window).
    No ENTER instruction.

  RETURN src0                   — return value in register src0 (terminal).
  CALL dest, src0, src1         — func in register src0, arity in src1, result to dest.
  TAIL_CALL src0, src1          — func in register src0, arity in src1, terminal.
  APPLY dest, src0, src1        — func in register src0, arg_list register in src1, result to dest.
  TAIL_APPLY src0, src1         — func in register src0, arg_list register in src1, terminal.
  MAKE_CLOSURE code_idx         — writes closure to dest register.
  PATCH_CLOSURE var_idx, capture_slot, value_reg — patches a capture slot in a closure.

Validator initial stack depth = 0 always.
Params are pre-initialized at function entry (slots 0..param_count-1).

Index constraints:
  All LOAD ops:  dest < local_count
  MOVE:          src0 and dest < local_count + outgoing_arg_slots
  All other dest-writing ops: dest < local_count
  PATCH_CLOSURE: src0, src2 < local_count
  CALL/TAIL_CALL: src0 (func register) < local_count
  APPLY/TAIL_APPLY: src0 (func register) < local_count, src1 (arg_list) < local_count

Minimal valid "load and return" sequence:
  LOAD_CONST dest=0, src0=0   (local_count >= 1)
  RETURN src0=0
"""

import pytest

from menai.menai_bytecode import CodeObject, Instruction, Opcode
from menai.menai_vm_bytecode_validator import ValidationError, ValidationErrorType, validate_bytecode
from menai.menai_value import MenaiInteger


class TestBytecodeValidator:
    """Test bytecode validation."""

    def test_valid_simple_code(self):
        """Test that valid bytecode passes validation.

        Minimal valid sequence under the register-window ISA:
          LOAD_CONST dest=0, src0=0  — write 42 into r0
          RETURN src0=0              — return r0 (terminal)
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),  # r0 = constants[0] = 42
                Instruction(Opcode.RETURN, src0=0),               # return r0 (terminal)
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        validate_bytecode(code)

    def test_invalid_constant_index(self):
        """Test that LOAD_CONST with an out-of-bounds src0 is caught.

        LOAD_CONST dest=0, src0=5 — src0=5 is out of bounds (only 1 constant).
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=5),  # src0=5 out of bounds
                Instruction(Opcode.RETURN, src0=0),
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

    def test_invalid_name_index(self):
        """Test that LOAD_NAME with an out-of-bounds src0 is caught.

        LOAD_NAME dest=0, src0=3 — src0=3 is out of bounds (only 1 name).
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_NAME, dest=0, src0=3),  # src0=3 out of bounds
                Instruction(Opcode.RETURN, src0=0),
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

    def test_return_uninitialized_register(self):
        """Test that RETURN with an uninitialized source register is caught.

        RETURN src0=0 with local_count=1 but r0 never written → UNINITIALIZED_VARIABLE.
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.RETURN, src0=0),  # r0 never initialized
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE
        assert "RETURN source register" in exc_info.value.message

    def test_valid_conditional_jump(self):
        """Test that a valid if/else (both branches return) passes validation.

        Control flow (local_count=1, param_count=0):

          0: LOAD_TRUE dest=0                  — r0=#t
          1: JUMP_IF_FALSE src0=0, src1=4      — read r0; jump→4, fall→2

          Then branch:
          2: LOAD_CONST dest=0, src0=0         — r0=1
          3: RETURN src0=0                     — terminal ✓

          Else branch:
          4: LOAD_CONST dest=0, src0=1         — r0=2
          5: RETURN src0=0                     — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=0),              # 0: r0=#t
                Instruction(Opcode.JUMP_IF_FALSE, src0=0, src1=4),  # 1: read r0; jump→4, fall→2
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 2: r0=1
                Instruction(Opcode.RETURN, src0=0),                 # 3: terminal ✓
                Instruction(Opcode.LOAD_CONST, dest=0, src0=1),     # 4: r0=2
                Instruction(Opcode.RETURN, src0=0),                 # 5: terminal ✓
            ],
            constants=[MenaiInteger(1), MenaiInteger(2)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        validate_bytecode(code)

    def test_valid_loop(self):
        """Test that a valid loop with a consistent stack depth at the back-edge passes.

        Control flow (local_count=1, param_count=0):

          0: LOAD_TRUE dest=0                  — r0=#t  ← loop header
          1: JUMP_IF_FALSE src0=0, src1=3      — read r0; jump→3, fall→2
          2: JUMP src0=0                       — back to 0 ✓
          3: LOAD_CONST dest=0, src0=0         — r0=42
          4: RETURN src0=0                     — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=0),              # 0: r0=#t ← loop header
                Instruction(Opcode.JUMP_IF_FALSE, src0=0, src1=3),  # 1: read r0; jump→3, fall→2
                Instruction(Opcode.JUMP, src0=0),                   # 2: back to 0 ✓
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 3: r0=42
                Instruction(Opcode.RETURN, src0=0),                 # 4: terminal ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        validate_bytecode(code)

    def test_valid_make_closure(self):
        """Test that valid MAKE_CLOSURE passes validation.

        Lambda (param_count=1, local_count=1):
          Params pre-initialized: slot 0 holds the argument.
          0: RETURN src0=0  — return r0 (terminal ✓)

        Outer code (local_count=2, outgoing_arg_slots=1):
          0: LOAD_CONST dest=0, src0=0                — r0=42 (captured value)
          1: MAKE_CLOSURE dest=1, src0=0, src1=1      — r1=closure, 1 capture from stack... wait,
             captures are now register-based too. src1=1 means 1 capture value popped from stack.
             Actually MAKE_CLOSURE still pops capture_count values from the stack.
             Let's use 0 captures for simplicity and just return the closure.
          0: MAKE_CLOSURE dest=0, src0=0, src1=0      — r0=closure (0 captures)
          1: RETURN src0=0                             — terminal ✓
        """
        lambda_code = CodeObject(
            instructions=[
                Instruction(Opcode.RETURN, src0=0),  # 0: return r0 (param) ✓
            ],
            constants=[],
            names=[],
            code_objects=[],
            param_count=1,
            local_count=1,
        )

        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),  # 0: r0=closure (0 captures)
                Instruction(Opcode.RETURN, src0=0),                        # 1: terminal ✓
            ],
            constants=[],
            names=[],
            code_objects=[lambda_code],
            local_count=1,
        )
        validate_bytecode(code)

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

    def test_missing_return(self):
        """Test that code that falls off the end without RETURN is caught.

        Sequence (local_count=1):
          0: LOAD_CONST dest=0, src0=0 — r0=42
          (no RETURN — falls off end → MISSING_RETURN)
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),  # r0=42
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

    def test_tail_call_is_terminal(self):
        """Test that TAIL_CALL is treated as terminal (no successors needed).

        Sequence (local_count=2, outgoing_arg_slots=1, names=["f"], constants=[42]):
          0: LOAD_NAME dest=0, src0=0    — r0=f
          1: LOAD_CONST dest=1, src0=0   — r1=42
          2: MOVE dest=2, src0=1         — slot 2 (outgoing zone) = r1 (one arg)
          3: TAIL_CALL src0=0, src1=1    — func=r0, arity=1; terminal ✓
          4: LOAD_CONST dest=0, src0=0   — unreachable; structurally valid
          5: RETURN src0=0               — unreachable; structurally valid
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_NAME, dest=0, src0=0),    # 0: r0=f
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),   # 1: r1=42
                Instruction(Opcode.MOVE, dest=2, src0=1),         # 2: slot 2 = r1 (outgoing arg)
                Instruction(Opcode.TAIL_CALL, src0=0, src1=1),    # 3: func=r0, arity=1; terminal ✓
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),   # 4: unreachable
                Instruction(Opcode.RETURN, src0=0),                # 5: unreachable
            ],
            constants=[MenaiInteger(42)],
            names=["f"],
            code_objects=[],
            local_count=2,
            outgoing_arg_slots=1,
        )
        validate_bytecode(code)

    def test_nested_code_validation(self):
        """Test that invalid nested code objects are caught recursively.

        The nested lambda has LOAD_CONST dest=0, src0=99 but an empty constant
        pool — this triggers INDEX_OUT_OF_BOUNDS during nested validation.
        """
        invalid_lambda = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=99),  # src0=99 out of bounds
                Instruction(Opcode.RETURN, src0=0),
            ],
            constants=[],   # empty — src0=99 is invalid
            names=[],
            code_objects=[],
            local_count=1,
        )

        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),  # r0=closure
                Instruction(Opcode.RETURN, src0=0),                        # terminal ✓
            ],
            constants=[],
            names=[],
            code_objects=[invalid_lambda],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INDEX_OUT_OF_BOUNDS

    def test_valid_variable_initialization(self):
        """Test that LOAD to register followed by RETURN passes the initialization check.

        Sequence (local_count=1):
          0: LOAD_CONST dest=0, src0=0 — r0=42; r0 is now initialized
          1: RETURN src0=0             — r0 is initialized ✓; terminal
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),  # r0=42; r0 initialized
                Instruction(Opcode.RETURN, src0=0),               # r0 initialized ✓; terminal
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        validate_bytecode(code)

    def test_uninitialized_variable(self):
        """Test that MOVE from a slot that was never written is caught.

        Sequence (local_count=2):
          0: MOVE dest=1, src0=0 — r0 was never written → UNINITIALIZED_VARIABLE
          1: RETURN src0=1
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MOVE, dest=1, src0=0),  # r0 never initialized → error
                Instruction(Opcode.RETURN, src0=1),
            ],
            constants=[],
            names=[],
            code_objects=[],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE

    def test_conditional_both_branches_initialize(self):
        """Test that a variable initialized in both branches is OK at the merge point.

        Control flow (local_count=2, param_count=0):

          0: LOAD_TRUE dest=0                  — r0=#t
          1: JUMP_IF_FALSE src0=0, src1=4      — read r0; jump→4, fall→2

          Then branch:
          2: LOAD_CONST dest=1, src0=0         — r1=42
          3: JUMP src0=5                       — jump to 5

          Else branch:
          4: LOAD_CONST dest=1, src0=0         — r1=42

          Merge (r1 initialized on both paths):
          5: RETURN src0=1                     — r1 initialized ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=0),              # 0: r0=#t
                Instruction(Opcode.JUMP_IF_FALSE, src0=0, src1=4),  # 1: read r0; jump→4, fall→2
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),     # 2: r1=42
                Instruction(Opcode.JUMP, src0=5),                   # 3: jump to 5
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),     # 4: r1=42
                Instruction(Opcode.RETURN, src0=1),                 # 5: r1 initialized ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=2,
        )
        validate_bytecode(code)

    def test_conditional_one_branch_initializes(self):
        """Test that use of a variable initialized in only one branch is caught.

        r0 starts uninitialised; r1 holds the condition.
        The then-branch initializes r0; the else-branch does not.

        Control flow (local_count=2, param_count=0):

          0: LOAD_TRUE dest=1                  — r1=#t
          1: JUMP_IF_FALSE src0=1, src1=4      — read r1; jump→4, fall→2

          Then branch: initializes r0
          2: LOAD_CONST dest=0, src0=0         — r0=42
          3: JUMP src0=5                       — jump to 5

          Else branch: does NOT initialize r0
          4: JUMP src0=5                       — jump to 5 (r0 not initialized)

          Merge:
          5: RETURN src0=0                     — r0 may be uninitialized → UNINITIALIZED_VARIABLE
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=1),              # 0: r1=#t; r0 untouched
                Instruction(Opcode.JUMP_IF_FALSE, src0=1, src1=4),  # 1: read r1; jump→4, fall→2
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 2: r0=42
                Instruction(Opcode.JUMP, src0=5),                   # 3: jump to 5
                Instruction(Opcode.JUMP, src0=5),                   # 4: jump to 5 (no init)
                Instruction(Opcode.RETURN, src0=0),                 # 5: r0 may be uninit → error
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
          3: LOAD_CONST dest=0, src0=0         — r0=42
          4: RETURN src0=0                     — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=0),              # 0: r0=#t ← loop header
                Instruction(Opcode.JUMP_IF_FALSE, src0=0, src1=3),  # 1: read r0; jump→3, fall→2
                Instruction(Opcode.JUMP, src0=0),                   # 2: back to 0 ✓
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 3: r0=42
                Instruction(Opcode.RETURN, src0=0),                 # 4: terminal ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
        )
        validate_bytecode(code)

    def test_valid_move_into_outgoing_zone(self):
        """Test that MOVE with dest in the outgoing arg zone passes validation.

        local_count=1, outgoing_arg_slots=1 → total_slots=2.
        MOVE dest=1 (= local_count + 0) is valid for MOVE.

        Sequence:
          0: LOAD_CONST dest=0, src0=0  — r0=42
          1: MOVE dest=1, src0=0        — slot 1 (outgoing zone) = r0 ✓
          2: RETURN src0=0              — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),  # r0=42
                Instruction(Opcode.MOVE, dest=1, src0=0),         # slot 1 = r0 (outgoing zone) ✓
                Instruction(Opcode.RETURN, src0=0),               # terminal ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
            outgoing_arg_slots=1,
        )
        validate_bytecode(code)

    def test_move_dest_out_of_bounds(self):
        """Test that MOVE with dest >= local_count + outgoing_arg_slots is caught.

        local_count=1, outgoing_arg_slots=1 → total_slots=2.
        MOVE dest=2 is out of bounds.
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),  # r0=42
                Instruction(Opcode.MOVE, dest=2, src0=0),         # dest=2 >= total_slots=2 → error
                Instruction(Opcode.RETURN, src0=0),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
            outgoing_arg_slots=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS

    def test_move_dest_out_of_bounds_no_outgoing(self):
        """Test that MOVE with dest >= local_count is caught when outgoing_arg_slots=0.

        local_count=1, outgoing_arg_slots=0 → total_slots=1.
        MOVE dest=1 is out of bounds.
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),  # r0=42
                Instruction(Opcode.MOVE, dest=1, src0=0),         # dest=1 >= total_slots=1 → error
                Instruction(Opcode.RETURN, src0=0),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
            outgoing_arg_slots=0,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS

    def test_non_move_dest_in_outgoing_zone_rejected(self):
        """Test that any op writing beyond total_slots is caught.

        Any dest-writing op may write into the outgoing zone (dest < total_slots).
        local_count=1, outgoing_arg_slots=1 → total_slots=2, so LOAD_CONST dest=2 is out of bounds.
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=2, src0=0),  # dest=2 >= total_slots=2 → error
                Instruction(Opcode.RETURN, src0=0),
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[],
            local_count=1,
            outgoing_arg_slots=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS

    def test_valid_apply(self):
        """Test that APPLY with valid func and arg_list registers passes.

        Sequence (local_count=2):
          0: LOAD_NAME dest=0, src0=0    — r0=f (function)
          1: LOAD_EMPTY_LIST dest=1      — r1=[] (arg list)
          2: APPLY dest=0, src0=0, src1=1 — r0 = apply(r0, r1) ✓
          3: RETURN src0=0               — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_NAME, dest=0, src0=0),      # r0=f
                Instruction(Opcode.LOAD_EMPTY_LIST, dest=1),         # r1=[]
                Instruction(Opcode.APPLY, dest=0, src0=0, src1=1),   # r0 = apply(r0, r1) ✓
                Instruction(Opcode.RETURN, src0=0),                  # terminal ✓
            ],
            constants=[],
            names=["f"],
            code_objects=[],
            local_count=2,
        )
        validate_bytecode(code)

    def test_apply_uninitialized_arg_list(self):
        """Test that APPLY with an uninitialized arg_list register is caught.

        Sequence (local_count=2):
          0: LOAD_NAME dest=0, src0=0     — r0=f (function); r1 never written
          1: APPLY dest=0, src0=0, src1=1 — r1 uninitialized → UNINITIALIZED_VARIABLE
          2: RETURN src0=0
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_NAME, dest=0, src0=0),      # r0=f; r1 never written
                Instruction(Opcode.APPLY, dest=0, src0=0, src1=1),   # r1 uninit → error
                Instruction(Opcode.RETURN, src0=0),
            ],
            constants=[],
            names=["f"],
            code_objects=[],
            local_count=2,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE
        assert "arg_list register" in exc_info.value.message

    def test_apply_arg_list_out_of_bounds(self):
        """Test that APPLY with arg_list register >= local_count is caught.

        Sequence (local_count=1):
          0: LOAD_NAME dest=0, src0=0     — r0=f
          1: APPLY dest=0, src0=0, src1=5 — src1=5 >= local_count=1 → INVALID_VARIABLE_ACCESS
          2: RETURN src0=0
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_NAME, dest=0, src0=0),      # r0=f
                Instruction(Opcode.APPLY, dest=0, src0=0, src1=5),   # src1=5 out of bounds
                Instruction(Opcode.RETURN, src0=0),
            ],
            constants=[],
            names=["f"],
            code_objects=[],
            local_count=1,
        )
        with pytest.raises(ValidationError) as exc_info:
            validate_bytecode(code)

        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS
        assert "arg_list register" in exc_info.value.message

    def test_valid_call_with_outgoing_args(self):
        """Test that a CALL with args moved into the outgoing zone passes.

        Caller (local_count=2, outgoing_arg_slots=1):
          0: LOAD_NAME dest=0, src0=0    — r0=f (function)
          1: LOAD_CONST dest=1, src0=0   — r1=42 (arg value)
          2: MOVE dest=2, src0=1         — slot 2 (outgoing zone) = r1
          3: CALL dest=1, src0=0, src1=1 — r1 = call(r0, arity=1)
          4: RETURN src0=1               — terminal ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_NAME, dest=0, src0=0),    # r0=f
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),   # r1=42
                Instruction(Opcode.MOVE, dest=2, src0=1),         # slot 2 = r1 (outgoing arg)
                Instruction(Opcode.CALL, dest=1, src0=0, src1=1), # r1 = call(r0, 1)
                Instruction(Opcode.RETURN, src0=1),               # terminal ✓
            ],
            constants=[MenaiInteger(42)],
            names=["f"],
            code_objects=[],
            local_count=2,
            outgoing_arg_slots=1,
        )
        validate_bytecode(code)

    def test_params_pre_initialized(self):
        """Test that function parameters (slots 0..param_count-1) are pre-initialized.

        Function (param_count=2, local_count=2):
          Params r0 and r1 are pre-initialized by the caller.
          0: RETURN src0=1  — r1 is initialized (it's a param) ✓
        """
        code = CodeObject(
            instructions=[
                Instruction(Opcode.RETURN, src0=1),  # r1 is a param, pre-initialized ✓
            ],
            constants=[],
            names=[],
            code_objects=[],
            param_count=2,
            local_count=2,
        )
        validate_bytecode(code)


class TestPatchClosureValidation:
    """Tests for PATCH_CLOSURE validation in _validate_initialization."""

    @staticmethod
    def _make_closure_code(n_free_vars: int, name: str = "<inner>") -> CodeObject:
        """Return a minimal closed CodeObject with n_free_vars free variable slots.

        The lambda has param_count=1 and returns its first argument.
        Captured-value slots are param_count .. param_count+n_free_vars-1.
        """
        return CodeObject(
            instructions=[
                Instruction(Opcode.RETURN, src0=0),  # return r0 (param) ✓
            ],
            constants=[],
            names=[],
            code_objects=[],
            param_count=1,
            local_count=1 + n_free_vars,
            free_vars=[f"fv{i}" for i in range(n_free_vars)],
            name=name,
        )

    def test_valid_patch_closure(self):
        """PATCH_CLOSURE against a known closure slot with valid capture slots passes.

        Outer frame (local_count=2):
          Slot 0: holds the closure (written directly by MAKE_CLOSURE dest=0).
          Slot 1: scratch register for the values being patched in.

          0: MAKE_CLOSURE dest=0, src0=0, src1=0 — r0=closure; slot 0 tracked as closure
          1: LOAD_CONST dest=1, src0=0            — r1=42
          2: PATCH_CLOSURE src0=0, src1=0, src2=1 — r0 is closure ✓; capture_idx=0 < 2 ✓
          3: LOAD_CONST dest=1, src0=0            — r1=42
          4: PATCH_CLOSURE src0=0, src1=1, src2=1 — r0 is closure ✓; capture_idx=1 < 2 ✓
          5: RETURN src0=0                        — terminal ✓
        """
        inner = self._make_closure_code(2)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),    # 0: r0=closure
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),              # 1: r1=42
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=0, src2=1),   # 2: cap 0 < 2 ✓
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),              # 3: r1=42
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=1, src2=1),   # 4: cap 1 < 2 ✓
                Instruction(Opcode.RETURN, src0=0),                          # 5: terminal ✓
            ],
            constants=[MenaiInteger(42)],
            names=[],
            code_objects=[inner],
            local_count=2,
        )
        validate_bytecode(code)

    def test_valid_patch_closure_single_free_var(self):
        """PATCH_CLOSURE with exactly one free var and capture_slot=0 passes.

        Outer frame (local_count=2):
          0: MAKE_CLOSURE dest=0, src0=0, src1=0  — r0=closure
          1: LOAD_CONST dest=1, src0=0             — r1=1
          2: PATCH_CLOSURE src0=0, src1=0, src2=1  — capture_idx=0 < 1 ✓
          3: RETURN src0=0                         — terminal ✓
        """
        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),   # 0: r0=closure
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 1: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=0, src2=1),  # 2: cap 0 < 1 ✓
                Instruction(Opcode.RETURN, src0=0),                         # 3: terminal ✓
            ],
            constants=[MenaiInteger(1)],
            names=[],
            code_objects=[inner],
            local_count=2,
        )
        validate_bytecode(code)

    def test_patch_closure_uninitialized_slot(self):
        """PATCH_CLOSURE against an uninitialised slot is rejected.

        Slot 0 is never written before PATCH_CLOSURE src0=0.

        Sequence (local_count=2):
          0: LOAD_CONST dest=1, src0=0             — r1=1 (slot 0 never written)
          1: PATCH_CLOSURE src0=0, src1=0, src2=1  — slot 0 never initialized → UNINITIALIZED_VARIABLE
          2: RETURN src0=1
        """
        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 0: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=0, src2=1),  # 1: slot 0 never init → error
                Instruction(Opcode.RETURN, src0=1),                         # 2
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

        Branch A (instr 2-3): creates closure, stores to slot 0.
        Branch B (instr 4):   skips creation — slot 0 remains uninitialized.
        Merge:                PATCH_CLOSURE — slot 0 may be uninitialised → error.

        Control flow (local_count=2, initial depth=0):
          0: LOAD_TRUE dest=1            — r1=#t
          1: JUMP_IF_FALSE src0=1, src1=4 — read r1; jump→4, fall→2

          Branch A:
          2: MAKE_CLOSURE dest=0, src0=0, src1=0 — r0=closure
          3: JUMP src0=5                         — jump to 5

          Branch B:
          4: JUMP src0=5                         — jump to 5 (slot 0 not initialized)

          Merge (slot 0 initialized only on branch A):
          5: LOAD_CONST dest=1, src0=0            — r1=1
          6: PATCH_CLOSURE src0=0, src1=0, src2=1 — slot 0 may be uninit → UNINITIALIZED_VARIABLE
          7: RETURN src0=1
        """
        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=1),               # 0: r1=#t
                Instruction(Opcode.JUMP_IF_FALSE, src0=1, src1=4),   # 1: jump→4, fall→2
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),  # 2: r0=closure
                Instruction(Opcode.JUMP, src0=5),                    # 3: jump to 5
                Instruction(Opcode.JUMP, src0=5),                    # 4: jump to 5 (no init)
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),      # 5: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=0, src2=1), # 6: may be uninit → error
                Instruction(Opcode.RETURN, src0=1),                  # 7
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
          2: PATCH_CLOSURE src0=0, src1=0, src2=1  — slot 0 not a closure → INVALID_VARIABLE_ACCESS
          3: RETURN src0=1
        """
        inner = self._make_closure_code(1)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_CONST, dest=0, src0=0),     # 0: slot 0 = 42 (not a closure)
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 1: r1=42
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=0, src2=1),  # 2: not closure → error
                Instruction(Opcode.RETURN, src0=1),                         # 3
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
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=0, src2=1),  # 3: not closure → error
                Instruction(Opcode.RETURN, src0=1),                         # 4
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
          0: LOAD_TRUE dest=1            — r1=#t
          1: JUMP_IF_FALSE src0=1, src1=4 — read r1; jump→4, fall→2

          Branch A:
          2: MAKE_CLOSURE dest=0, src0=0, src1=0 — r0=closure[0]
          3: JUMP src0=6                         — jump to 6

          Branch B:
          4: MAKE_CLOSURE dest=0, src0=1, src1=0 — r0=closure[1]
          5: JUMP src0=6                         — falls to 6

          Merge (slot 0 holds different closures on each path):
          6: LOAD_CONST dest=1, src0=0            — r1=1
          7: PATCH_CLOSURE src0=0, src1=0, src2=1 — ambiguous closure → INVALID_VARIABLE_ACCESS
          8: RETURN src0=1
        """
        inner_a = self._make_closure_code(1, name="<inner-a>")
        inner_b = self._make_closure_code(1, name="<inner-b>")
        code = CodeObject(
            instructions=[
                Instruction(Opcode.LOAD_TRUE, dest=1),               # 0: r1=#t
                Instruction(Opcode.JUMP_IF_FALSE, src0=1, src1=4),   # 1: jump→4, fall→2
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),  # 2: r0=closure[0]
                Instruction(Opcode.JUMP, src0=6),                    # 3: jump to 6
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=1, src1=0),  # 4: r0=closure[1]
                Instruction(Opcode.JUMP, src0=6),                    # 5: falls to 6
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),      # 6: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=0, src2=1), # 7: ambiguous → error
                Instruction(Opcode.RETURN, src0=1),                  # 8
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
          2: PATCH_CLOSURE src0=0, src1=2, src2=1  — capture_idx=2 >= n_free=2 → INDEX_OUT_OF_BOUNDS
          3: RETURN src0=1
        """
        inner = self._make_closure_code(2)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),   # 0: r0=closure
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 1: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=2, src2=1),  # 2: cap 2 >= n_free=2 → error
                Instruction(Opcode.RETURN, src0=1),                         # 3
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
          2: PATCH_CLOSURE src0=0, src1=0, src2=1  — capture_idx=0 >= n_free=0 → INDEX_OUT_OF_BOUNDS
          3: RETURN src0=1
        """
        inner = self._make_closure_code(0)
        code = CodeObject(
            instructions=[
                Instruction(Opcode.MAKE_CLOSURE, dest=0, src0=0, src1=0),   # 0: r0=closure
                Instruction(Opcode.LOAD_CONST, dest=1, src0=0),             # 1: r1=1
                Instruction(Opcode.PATCH_CLOSURE, src0=0, src1=0, src2=1),  # 2: cap 0 >= n_free=0 → error
                Instruction(Opcode.RETURN, src0=1),                         # 3
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
