"""Thin wrapper around the C VM execution engine."""

from collections.abc import Callable
from typing import cast

from menai.menai_bytecode import CodeObject
from menai.menai_value import MenaiValue
from menai.menai_vm_bytecode_validator import validate_bytecode

from menai.menai_vm_c import execute as _c_vm_execute  # type: ignore[import-not-found]
from menai.menai_vm_c import cancel as _c_vm_cancel    # type: ignore[import-not-found]


class MenaiVM:
    """Wrapper around the C VM, exposing execute() and cancel()."""

    def __init__(self, validate: bool = True) -> None:
        self.validate_bytecode = validate

    def execute(
        self,
        code: CodeObject,
        globals_dict: dict[str, MenaiValue] | CodeObject | None = None,
        extra_bindings: dict[str, MenaiValue] | None = None
    ) -> MenaiValue:
        """Execute a code object and return the result."""
        if self.validate_bytecode:
            validate_bytecode(code)

        return cast(Callable[..., MenaiValue], _c_vm_execute)(
            code, globals_dict or {}, extra_bindings or {}
        )

    def cancel(self) -> None:
        """Request cancellation of the currently executing code.

        Thread-safe: may be called from a different thread than the one
        executing the VM.  The flag is checked at the next cancellation
        check point in the C execution loop.
        """
        _c_vm_cancel()
