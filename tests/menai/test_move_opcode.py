"""Tests for the MOVE opcode."""

import pytest

from menai.menai_bytecode import CodeObject, Instruction, Opcode
try:
    from menai.menai_value_fast import MenaiInteger, MenaiString, MenaiBoolean, MenaiNone, Menai_NONE
except ImportError:
    from menai.menai_value import MenaiInteger, MenaiString, MenaiBoolean, MenaiNone, Menai_NONE  # type: ignore[assignment]
from menai.menai_vm import MenaiVM
from menai.menai_vm_bytecode_validator import BytecodeValidator, ValidationError, ValidationErrorType


def _make_code(instructions, local_count, constants=None, names=None, code_objects=None):
    """Build a minimal CodeObject for VM / validator tests."""
    return CodeObject(
        instructions=instructions,
        constants=constants or [],
        names=names or [],
        code_objects=code_objects or [],
        param_count=0,
        local_count=local_count,
    )


class TestMoveOpcode:
    """Tests for the MOVE opcode in the VM."""

    def setup_method(self):
        self.vm = MenaiVM(validate=False)

    def _run(self, code):
        return self.vm.execute(code, {})

    def test_move_integer(self):
        """MOVE copies an integer from one register to another."""
        code = _make_code([
            Instruction(Opcode.LOAD_CONST, dest=0, src0=0),   # r0 = 42
            Instruction(Opcode.MOVE, dest=1, src0=0),          # r1 = r0
            Instruction(Opcode.RETURN, src0=1),                # return r1
        ], local_count=2, constants=[MenaiInteger(42)])
        assert self._run(code) == MenaiInteger(42)

    def test_move_string(self):
        """MOVE copies a string value."""
        code = _make_code([
            Instruction(Opcode.LOAD_CONST, dest=0, src0=0),
            Instruction(Opcode.MOVE, dest=1, src0=0),
            Instruction(Opcode.RETURN, src0=1),
        ], local_count=2, constants=[MenaiString("hello")])
        assert self._run(code) == MenaiString("hello")

    def test_move_boolean(self):
        """MOVE copies a boolean value."""
        code = _make_code([
            Instruction(Opcode.LOAD_TRUE, dest=0),
            Instruction(Opcode.MOVE, dest=1, src0=0),
            Instruction(Opcode.RETURN, src0=1),
        ], local_count=2)
        assert self._run(code) == MenaiBoolean(True)

    def test_move_none(self):
        """MOVE copies a none value."""
        code = _make_code([
            Instruction(Opcode.LOAD_NONE, dest=0),
            Instruction(Opcode.MOVE, dest=1, src0=0),
            Instruction(Opcode.RETURN, src0=1),
        ], local_count=2)
        assert self._run(code) == Menai_NONE

    def test_move_same_register(self):
        """MOVE with dest == src0 is a no-op."""
        code = _make_code([
            Instruction(Opcode.LOAD_CONST, dest=0, src0=0),
            Instruction(Opcode.MOVE, dest=0, src0=0),
            Instruction(Opcode.RETURN, src0=0),
        ], local_count=1, constants=[MenaiInteger(7)])
        assert self._run(code) == MenaiInteger(7)

    def test_move_chain(self):
        """A chain of MOVEs propagates the value correctly."""
        code = _make_code([
            Instruction(Opcode.LOAD_CONST, dest=0, src0=0),
            Instruction(Opcode.MOVE, dest=1, src0=0),
            Instruction(Opcode.MOVE, dest=2, src0=1),
            Instruction(Opcode.MOVE, dest=3, src0=2),
            Instruction(Opcode.RETURN, src0=3),
        ], local_count=4, constants=[MenaiInteger(99)])
        assert self._run(code) == MenaiInteger(99)

    def test_move_does_not_alias(self):
        """Overwriting the source register after MOVE does not affect the dest."""
        code = _make_code([
            Instruction(Opcode.LOAD_CONST, dest=0, src0=0),   # r0 = 1
            Instruction(Opcode.MOVE, dest=1, src0=0),          # r1 = r0  (1)
            Instruction(Opcode.LOAD_CONST, dest=0, src0=1),   # r0 = 2  (overwrite source)
            Instruction(Opcode.RETURN, src0=1),                # return r1 → still 1
        ], local_count=2, constants=[MenaiInteger(1), MenaiInteger(2)])
        assert self._run(code) == MenaiInteger(1)


class TestMoveDisassembly:
    """Tests for the MOVE opcode's disassembly representation."""

    def test_repr(self):
        instr = Instruction(Opcode.MOVE, dest=3, src0=7)
        assert repr(instr) == "r3 = MOVE r7"

    def test_repr_same_register(self):
        instr = Instruction(Opcode.MOVE, dest=0, src0=0)
        assert repr(instr) == "r0 = MOVE r0"


class TestMoveOpcodeArgCount:
    """MOVE must declare 1 instruction-stream argument."""

    def test_arg_count(self):
        assert Opcode.MOVE.arg_count() == 1


class TestMoveValidation:
    """Tests for MOVE validation in the bytecode validator."""

    def setup_method(self):
        self.validator = BytecodeValidator()

    def test_valid_move(self):
        """A well-formed MOVE passes validation."""
        code = _make_code([
            Instruction(Opcode.LOAD_CONST, dest=0, src0=0),
            Instruction(Opcode.MOVE, dest=1, src0=0),
            Instruction(Opcode.RETURN, src0=1),
        ], local_count=2, constants=[MenaiInteger(1)])
        self.validator.validate(code)  # must not raise

    def test_move_src_out_of_bounds(self):
        """MOVE with src0 >= local_count must fail index validation."""
        code = _make_code([
            Instruction(Opcode.LOAD_CONST, dest=0, src0=0),
            Instruction(Opcode.MOVE, dest=1, src0=99),   # 99 is out of bounds
            Instruction(Opcode.RETURN, src0=1),
        ], local_count=2, constants=[MenaiInteger(1)])
        with pytest.raises(ValidationError) as exc_info:
            self.validator.validate(code)
        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS

    def test_move_dest_out_of_bounds(self):
        """MOVE with dest >= local_count must fail index validation."""
        code = _make_code([
            Instruction(Opcode.LOAD_CONST, dest=0, src0=0),
            Instruction(Opcode.MOVE, dest=99, src0=0),   # 99 is out of bounds
            Instruction(Opcode.RETURN, src0=0),
        ], local_count=2, constants=[MenaiInteger(1)])
        with pytest.raises(ValidationError) as exc_info:
            self.validator.validate(code)
        assert exc_info.value.error_type == ValidationErrorType.INVALID_VARIABLE_ACCESS

    def test_move_uninitialized_src(self):
        """MOVE from an uninitialised register must fail initialization validation."""
        code = _make_code([
            Instruction(Opcode.MOVE, dest=1, src0=0),   # r0 never written
            Instruction(Opcode.RETURN, src0=1),
        ], local_count=2)
        with pytest.raises(ValidationError) as exc_info:
            self.validator.validate(code)
        assert exc_info.value.error_type == ValidationErrorType.UNINITIALIZED_VARIABLE

    def test_move_initializes_dest(self):
        """MOVE must mark dest as initialized so a subsequent RETURN is valid."""
        code = _make_code([
            Instruction(Opcode.LOAD_CONST, dest=0, src0=0),
            Instruction(Opcode.MOVE, dest=1, src0=0),
            Instruction(Opcode.RETURN, src0=1),
        ], local_count=2, constants=[MenaiInteger(0)])
        self.validator.validate(code)  # must not raise
