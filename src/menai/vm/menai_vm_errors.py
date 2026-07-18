"""
VM error codes and the mapping from structured codes to Python exceptions.

The C VM reports errors via a MenaiVMError struct containing a granular
integer code plus diagnostic context (opcode, ip, call_depth).  The C bridge
packages this into a _MenaiVMRuntimeError sentinel exception.  This module
provides the Python-side table that maps each error code to the appropriate
exception class, message, and optional suggestion/context.

This is the single source of truth for error-code-to-message mapping.
The C bridge no longer contains any string formatting logic.
"""

from enum import IntEnum

from menai.menai_error import MenaiCancelledException, MenaiEvalError


class VMErrorCode(IntEnum):
    """
    Integer error codes produced by the C VM.

    These mirror the MENAI_ERR_* #define constants in menai_vm_c.h.
    Keeping them in sync is an explicit invariant: the C header is the
    authoritative source, and this enum must match it exactly.
    """

    OK = 0
    NOMEM = -1
    VALUE = -2
    OVERFLOW = -3
    DIVISION_BY_ZERO = -4
    TYPE = -5
    EVAL = -6
    CANCELLED = -7
    TYPE_MISMATCH = -8
    NOT_SYMBOL = -9
    NOT_SYMBOL_PAIR = -10
    IF_NOT_BOOLEAN = -11
    ERROR_MSG_NOT_STRING = -12
    NOT_CALLABLE = -13
    APPLY_SECOND_NOT_LIST = -14
    APPLY_FIRST_NOT_FUNCTION = -15
    PATCH_CLOSURE_NOT_FUNCTION = -16
    INDEX_NOT_INTEGER = -17
    SLICE_INDICES_NOT_INTEGER = -18
    NOT_SINGLE_CHAR_STRING = -19
    RADIX_NOT_INTEGER = -20
    OFFSET_NOT_INTEGER = -21
    VALUE_NOT_INTEGER = -22
    LIST_ELEMENTS_NOT_INTEGERS = -23
    SLICE_START_NOT_INTEGER = -24
    SLICE_END_NOT_INTEGER = -25
    BYTE_NOT_INTEGER = -26
    LIST_TO_STRING_NOT_STRINGS = -27
    RANGE_NOT_INTEGER = -28
    STRUCT_FIRST_NOT_TYPE = -29
    INDEX_OUT_OF_RANGE = -30
    SLICE_START_OUT_OF_RANGE = -31
    SLICE_END_OUT_OF_RANGE = -32
    OFFSET_OUT_OF_BOUNDS = -33
    MODULO_BY_ZERO = -34
    INVALID_RADIX = -35
    VALUE_OUT_OF_RANGE = -36
    INVALID_CODEPOINT = -37
    NEGATIVE_SLICE_INDEX = -38
    NEGATIVE_EXPONENT = -39
    NEGATIVE_SHIFT = -40
    NEGATIVE_ARGUMENT = -41
    SHIFT_TOO_LARGE = -42
    INVALID_LOG_BASE = -43
    SLICE_START_AFTER_END = -44
    ARITY_MISMATCH = -45
    STRUCT_ARITY_MISMATCH = -46
    UNDEFINED_VARIABLE = -47
    STRUCT_FIELD_NOT_FOUND = -48
    EMPTY_LIST = -49
    CALL_DEPTH_EXCEEDED = -50
    UNHASHABLE_KEY = -51
    INVALID_UTF8 = -52
    HEX_EVEN_LENGTH = -53
    INVALID_HEX_CHAR = -54
    TRUNCATED_LEB128 = -55
    RANGE_ZERO_STEP = -56
    CLOSURE_INDEX_OUT_OF_RANGE = -57
    MISSING_RETURN = -58
    UNIMPLEMENTED_OPCODE = -59
    USER_ERROR = -60


class _MenaiVMRuntimeError(Exception):
    """
    Sentinel exception raised by the C bridge carrying structured error data.

    This is internal to the menai_vm module.  It is caught by MenaiVM.execute
    and translated into a proper MenaiEvalError (or Python built-in exception)
    using the error table below.  External code should never see this class.

    Attributes:
        code: VMErrorCode integer from the C VM.
        opcode: Opcode that was executing (0 if unknown).
        ip: Instruction pointer at time of error (0 if unknown).
        call_depth: Call stack depth at time of error.
        user_message: User-supplied error string (only for USER_ERROR).
    """

    def __init__(
        self,
        code: int,
        opcode: int = 0,
        ip: int = 0,
        call_depth: int = 0,
        user_message: str | None = None
    ) -> None:
        self.code = code
        self.opcode = opcode
        self.ip = ip
        self.call_depth = call_depth
        self.user_message = user_message
        super().__init__(f"VM error {code}")


# Type alias for the error table entries.
# Each entry maps a VMErrorCode to:
#   exception_class: The exception class to raise
#   message: The error message string
#   suggestion: Optional suggestion string
_ErrorTableEntry = tuple[type[Exception], str, str | None]


def _eval_error_entry(message: str, suggestion: str | None = None) -> _ErrorTableEntry:
    """Build an error table entry that raises MenaiEvalError."""
    return (MenaiEvalError, message, suggestion)


# The error table.  Each VMErrorCode maps to (exception_class, message, suggestion).
# USER_ERROR is handled specially — its message comes from user_message, not the table.
_ERROR_TABLE: dict[VMErrorCode, _ErrorTableEntry] = {
    VMErrorCode.NOMEM: (MemoryError, "out of memory", None),
    VMErrorCode.OVERFLOW: (OverflowError, "integer overflow", None),
    VMErrorCode.VALUE: (ValueError, "invalid value", None),
    VMErrorCode.TYPE: (TypeError, "type error", None),
    VMErrorCode.CANCELLED: _eval_error_entry("Execution was cancelled"),

    VMErrorCode.TYPE_MISMATCH: _eval_error_entry("type mismatch"),
    VMErrorCode.NOT_SYMBOL: _eval_error_entry("argument must be a symbol"),
    VMErrorCode.NOT_SYMBOL_PAIR: _eval_error_entry("arguments must be symbols"),
    VMErrorCode.IF_NOT_BOOLEAN: _eval_error_entry("if condition must be boolean"),
    VMErrorCode.ERROR_MSG_NOT_STRING: _eval_error_entry("error: message must be a string"),
    VMErrorCode.NOT_CALLABLE: _eval_error_entry("cannot call non-function value"),
    VMErrorCode.APPLY_SECOND_NOT_LIST: _eval_error_entry("apply: second argument must be a list"),
    VMErrorCode.APPLY_FIRST_NOT_FUNCTION: _eval_error_entry("apply: first argument must be a function"),
    VMErrorCode.PATCH_CLOSURE_NOT_FUNCTION: _eval_error_entry("patch-closure requires a function"),
    VMErrorCode.INDEX_NOT_INTEGER: _eval_error_entry("index must be an integer"),
    VMErrorCode.SLICE_INDICES_NOT_INTEGER: _eval_error_entry("slice indices must be integers"),
    VMErrorCode.NOT_SINGLE_CHAR_STRING: _eval_error_entry("requires a single-character string"),
    VMErrorCode.RADIX_NOT_INTEGER: _eval_error_entry("radix must be an integer"),
    VMErrorCode.OFFSET_NOT_INTEGER: _eval_error_entry("offset must be an integer"),
    VMErrorCode.VALUE_NOT_INTEGER: _eval_error_entry("value must be an integer"),
    VMErrorCode.LIST_ELEMENTS_NOT_INTEGERS: _eval_error_entry("list elements must be integers"),
    VMErrorCode.SLICE_START_NOT_INTEGER: _eval_error_entry("slice start must be an integer"),
    VMErrorCode.SLICE_END_NOT_INTEGER: _eval_error_entry("slice end must be an integer"),
    VMErrorCode.BYTE_NOT_INTEGER: _eval_error_entry("byte must be an integer"),
    VMErrorCode.LIST_TO_STRING_NOT_STRINGS: _eval_error_entry("list->string: all elements must be strings"),
    VMErrorCode.RANGE_NOT_INTEGER: _eval_error_entry("range requires integer arguments"),
    VMErrorCode.STRUCT_FIRST_NOT_TYPE: _eval_error_entry(
        "struct constructor: first argument must be a struct type"
    ),
    VMErrorCode.INDEX_OUT_OF_RANGE: _eval_error_entry("index out of range"),
    VMErrorCode.SLICE_START_OUT_OF_RANGE: _eval_error_entry("slice start index out of range"),
    VMErrorCode.SLICE_END_OUT_OF_RANGE: _eval_error_entry("slice end index out of range"),
    VMErrorCode.OFFSET_OUT_OF_BOUNDS: _eval_error_entry("offset out of bounds"),
    VMErrorCode.DIVISION_BY_ZERO: (ZeroDivisionError, "division by zero", None),
    VMErrorCode.MODULO_BY_ZERO: (ZeroDivisionError, "modulo by zero", None),
    VMErrorCode.INVALID_RADIX: _eval_error_entry("radix must be 2, 8, 10, or 16"),
    VMErrorCode.VALUE_OUT_OF_RANGE: _eval_error_entry("value out of range"),
    VMErrorCode.INVALID_CODEPOINT: _eval_error_entry("invalid Unicode scalar value"),
    VMErrorCode.NEGATIVE_SLICE_INDEX: _eval_error_entry("slice index cannot be negative"),
    VMErrorCode.NEGATIVE_EXPONENT: _eval_error_entry("exponent must be non-negative"),
    VMErrorCode.NEGATIVE_SHIFT: _eval_error_entry("shift amount must be non-negative"),
    VMErrorCode.NEGATIVE_ARGUMENT: _eval_error_entry("argument must be non-negative"),
    VMErrorCode.SHIFT_TOO_LARGE: _eval_error_entry("shift amount too large"),
    VMErrorCode.INVALID_LOG_BASE: _eval_error_entry("invalid log base"),
    VMErrorCode.SLICE_START_AFTER_END: _eval_error_entry(
        "slice start index cannot be greater than end index"
    ),
    VMErrorCode.ARITY_MISMATCH: _eval_error_entry("arity mismatch"),
    VMErrorCode.STRUCT_ARITY_MISMATCH: _eval_error_entry(
        "struct constructor called with wrong number of arguments"
    ),
    VMErrorCode.UNDEFINED_VARIABLE: _eval_error_entry("undefined variable"),
    VMErrorCode.STRUCT_FIELD_NOT_FOUND: _eval_error_entry("struct has no such field"),
    VMErrorCode.EMPTY_LIST: _eval_error_entry("requires a non-empty list"),
    VMErrorCode.CALL_DEPTH_EXCEEDED: _eval_error_entry("maximum call depth exceeded"),
    VMErrorCode.UNHASHABLE_KEY: _eval_error_entry("unhashable type for dict/set key"),
    VMErrorCode.INVALID_UTF8: _eval_error_entry("invalid UTF-8 sequence"),
    VMErrorCode.HEX_EVEN_LENGTH: _eval_error_entry("hex string must have even length"),
    VMErrorCode.INVALID_HEX_CHAR: _eval_error_entry("invalid hex character"),
    VMErrorCode.TRUNCATED_LEB128: _eval_error_entry("truncated LEB128"),
    VMErrorCode.RANGE_ZERO_STEP: _eval_error_entry("range: step cannot be zero"),
    VMErrorCode.CLOSURE_INDEX_OUT_OF_RANGE: _eval_error_entry("closure index out of range"),
    VMErrorCode.MISSING_RETURN: _eval_error_entry(
        "frame execution ended without RETURN instruction"
    ),
    VMErrorCode.UNIMPLEMENTED_OPCODE: _eval_error_entry("unimplemented opcode"),
}


def translate_vm_error(
    code: int,
    opcode: int = 0,
    ip: int = 0,
    call_depth: int = 0,
    user_message: str | None = None
) -> Exception:
    """
    Translate a structured VM error into the appropriate Python exception.

    This is called by MenaiVM.execute when it catches a _MenaiVMRuntimeError.
    It looks up the error code in the table and constructs the appropriate
    exception instance.  For MenaiEvalError subclasses, the structured
    diagnostic fields (opcode, ip, call_depth) are attached as attributes.

    For USER_ERROR, the user-supplied message becomes the exception message.

    Args:
        code: The VMErrorCode integer from the C VM.
        opcode: The opcode that was executing.
        ip: The instruction pointer at time of error.
        call_depth: The call stack depth at time of error.
        user_message: User-supplied error string (only for USER_ERROR).

    Returns:
        An exception instance ready to be raised.
    """
    # USER_ERROR is special — the message comes from the user's code.
    if code == VMErrorCode.USER_ERROR:
        msg = user_message if user_message is not None else "user error"
        return MenaiEvalError(
            msg,
            error_code=code,
            vm_opcode=opcode,
            vm_ip=ip,
            vm_call_depth=call_depth,
        )

    # CANCELLED maps to MenaiCancelledException.
    if code == VMErrorCode.CANCELLED:
        return MenaiCancelledException(
            error_code=code,
            vm_opcode=opcode,
            vm_ip=ip,
            vm_call_depth=call_depth,
        )

    # Look up in the error table.
    try:
        vm_code = VMErrorCode(code)

    except ValueError:
        return RuntimeError(f"unknown VM error code: {code}")

    entry = _ERROR_TABLE.get(vm_code)
    if entry is None:
        return RuntimeError(f"unhandled VM error code: {code}")

    exc_class, message, suggestion = entry

    if issubclass(exc_class, MenaiEvalError):
        exc: Exception = exc_class(
            message,
            suggestion=suggestion,
            error_code=code,
            vm_opcode=opcode,
            vm_ip=ip,
            vm_call_depth=call_depth,
        )

    else:
        # Python built-in exceptions (OverflowError, ZeroDivisionError, etc.)
        exc = exc_class(message)

        # Attach structured diagnostic fields as attributes for built-in
        # exception types that don't accept them in the constructor.
        exc.error_code = code  # type: ignore[attr-defined]
        exc.vm_opcode = opcode  # type: ignore[attr-defined]
        exc.vm_ip = ip  # type: ignore[attr-defined]
        exc.vm_call_depth = call_depth  # type: ignore[attr-defined]

    return exc
