"""Menai Virtual Machine - executes bytecode."""

import cmath
import difflib
import math
from typing import List, Dict, Any, cast, Protocol

from menai.menai_bytecode import CodeObject, Opcode, Instruction
from menai.menai_error import MenaiEvalError, MenaiCancelledException
from menai.menai_value import (
    MenaiValue, MenaiBoolean, MenaiString, MenaiList, MenaiDict, MenaiFunction,
    MenaiInteger, MenaiComplex, MenaiFloat, MenaiSymbol, MenaiNone, Menai_NONE
)
from menai.menai_vm_bytecode_validator import validate_bytecode


class MenaiTraceWatcher(Protocol):
    """Protocol for Menai trace watchers."""
    def on_trace(self, message: str) -> None:
        """
        Called when a trace message is emitted.

        Args:
            message: The trace message as a string (Menai formatted)
        """


# Sentinel returned by _op_call, _op_apply, and _op_return (non-top-level) to
# signal that the active frame has changed and the loop should re-sync from
# self.frames[-1].  Using a module-level singleton avoids per-call allocation.
class _FrameChange:
    pass

_FRAME_CHANGE = _FrameChange()


class Frame:
    """
    Execution frame for function calls.

    Each frame has its own locals and instruction pointer.
    """
    __slots__ = ('code', 'code_len', 'ip', 'locals', 'return_dest')

    def __init__(self, code: CodeObject) -> None:
        self.code = code
        self.code_len = len(code.instructions)
        self.ip = 0
        self.locals = cast(List[MenaiValue], [None] * code.local_count)
        self.return_dest: int = 0


class MenaiVM:
    """
    Virtual machine for executing Menai bytecode.

    Uses a stack-based architecture with lexically-scoped frames.
    """

    def __init__(self, validate: bool = True) -> None:
        self.stack: List[MenaiValue] = []

        # Sentinel frame so there's always a current frame.
        main_frame = Frame(CodeObject(
            name="<main>", instructions=[], constants=[], names=[], code_objects=[], local_count=0, param_count=0, is_variadic=False
        ))
        self.frames: List[Frame] = [main_frame]
        self.current_frame: Frame = main_frame
        self.globals: Dict[str, MenaiValue] = {}
        self.validate_bytecode = validate  # Whether to validate bytecode before execution

        # Trace watcher for debugging support
        self.trace_watcher: MenaiTraceWatcher | None = None

        # Cancellation support for non-blocking execution
        self._cancelled: bool = False

        # Check cancellation every N instructions (balance between responsiveness and performance)
        self._cancellation_check_interval: int = 1000

        # Build dispatch table for fast opcode execution
        self._dispatch_table = self._build_dispatch_table()

    def set_trace_watcher(self, watcher: MenaiTraceWatcher | None) -> None:
        """
        Set the trace watcher (replaces any existing watcher).

        Args:
            watcher: MenaiTraceWatcher instance or None to disable tracing
        """
        self.trace_watcher = watcher

    def _emit_trace(self, message: MenaiValue) -> None:
        """
        Emit a trace event to the watcher.

        Args:
            message: The Menai value to trace
        """
        if self.trace_watcher is None:
            return  # Fast path: no watcher, no work

        # Convert message to string using describe() and notify watcher
        message_str = message.describe()
        self.trace_watcher.on_trace(message_str)

    def cancel(self) -> None:
        """
        Request cancellation of the currently executing code.

        This sets a flag that will be checked periodically during execution.
        The cancellation is not immediate - it will be honored at the next
        cancellation check point (every ~1000 instructions by default).

        This method is thread-safe and can be called from a different thread
        than the one executing the VM.
        """
        self._cancelled = True

    def _build_dispatch_table(self) -> List[Any]:
        """
        Build jump table for opcode dispatch.

        This replaces the if/elif chain with direct array indexing,
        significantly improving performance in the hot execution loop.
        """
        table: List[Any] = [None] * 384
        table[Opcode.LOAD_NONE] = self._op_load_none
        table[Opcode.LOAD_TRUE] = self._op_load_true
        table[Opcode.LOAD_FALSE] = self._op_load_false
        table[Opcode.LOAD_EMPTY_LIST] = self._op_load_empty_list
        table[Opcode.LOAD_EMPTY_DICT] = self._op_load_empty_dict
        table[Opcode.LOAD_CONST] = self._op_load_const
        table[Opcode.LOAD_NAME] = self._op_load_name
        table[Opcode.PUSH] = self._op_push
        table[Opcode.POP] = self._op_pop
        table[Opcode.MOVE] = self._op_move
        table[Opcode.JUMP] = self._op_jump
        table[Opcode.JUMP_IF_FALSE] = self._op_jump_if_false
        table[Opcode.JUMP_IF_TRUE] = self._op_jump_if_true
        table[Opcode.RAISE_ERROR] = self._op_raise_error
        table[Opcode.MAKE_CLOSURE] = self._op_make_closure
        table[Opcode.PATCH_CLOSURE] = self._op_patch_closure
        table[Opcode.CALL] = self._op_call
        table[Opcode.TAIL_CALL] = self._op_tail_call
        table[Opcode.APPLY] = self._op_apply
        table[Opcode.TAIL_APPLY] = self._op_tail_apply
        table[Opcode.ENTER] = self._op_enter
        table[Opcode.RETURN] = self._op_return
        table[Opcode.EMIT_TRACE] = self._op_emit_trace
        table[Opcode.FUNCTION_P] = self._op_function_p
        table[Opcode.FUNCTION_EQ_P] = self._op_function_eq_p
        table[Opcode.FUNCTION_NEQ_P] = self._op_function_neq_p
        table[Opcode.FUNCTION_MIN_ARITY] = self._op_function_min_arity
        table[Opcode.FUNCTION_VARIADIC_P] = self._op_function_variadic_p
        table[Opcode.FUNCTION_ACCEPTS_P] = self._op_function_accepts_p
        table[Opcode.SYMBOL_P] = self._op_symbol_p
        table[Opcode.SYMBOL_EQ_P] = self._op_symbol_eq_p
        table[Opcode.SYMBOL_NEQ_P] = self._op_symbol_neq_p
        table[Opcode.SYMBOL_TO_STRING] = self._op_symbol_to_string
        table[Opcode.NONE_P] = self._op_none_p
        table[Opcode.BOOLEAN_P] = self._op_boolean_p
        table[Opcode.BOOLEAN_EQ_P] = self._op_boolean_eq_p
        table[Opcode.BOOLEAN_NEQ_P] = self._op_boolean_neq_p
        table[Opcode.BOOLEAN_NOT] = self._op_boolean_not
        table[Opcode.INTEGER_P] = self._op_integer_p
        table[Opcode.INTEGER_EQ_P] = self._op_integer_eq_p
        table[Opcode.INTEGER_NEQ_P] = self._op_integer_neq_p
        table[Opcode.INTEGER_LT_P] = self._op_integer_lt_p
        table[Opcode.INTEGER_GT_P] = self._op_integer_gt_p
        table[Opcode.INTEGER_LTE_P] = self._op_integer_lte_p
        table[Opcode.INTEGER_GTE_P] = self._op_integer_gte_p
        table[Opcode.INTEGER_ABS] = self._op_integer_abs
        table[Opcode.INTEGER_ADD] = self._op_integer_add
        table[Opcode.INTEGER_SUB] = self._op_integer_sub
        table[Opcode.INTEGER_MUL] = self._op_integer_mul
        table[Opcode.INTEGER_DIV] = self._op_integer_div
        table[Opcode.INTEGER_MOD] = self._op_integer_mod
        table[Opcode.INTEGER_NEG] = self._op_integer_neg
        table[Opcode.INTEGER_EXPN] = self._op_integer_expn
        table[Opcode.INTEGER_BIT_NOT] = self._op_integer_bit_not
        table[Opcode.INTEGER_BIT_SHIFT_LEFT] = self._op_integer_bit_shift_left
        table[Opcode.INTEGER_BIT_SHIFT_RIGHT] = self._op_integer_bit_shift_right
        table[Opcode.INTEGER_BIT_OR] = self._op_integer_bit_or
        table[Opcode.INTEGER_BIT_AND] = self._op_integer_bit_and
        table[Opcode.INTEGER_BIT_XOR] = self._op_integer_bit_xor
        table[Opcode.INTEGER_MIN] = self._op_integer_min
        table[Opcode.INTEGER_MAX] = self._op_integer_max
        table[Opcode.INTEGER_TO_STRING] = self._op_integer_to_string
        table[Opcode.INTEGER_TO_FLOAT] = self._op_integer_to_float
        table[Opcode.INTEGER_TO_COMPLEX] = self._op_integer_to_complex
        table[Opcode.INTEGER_CODEPOINT_TO_STRING] = self._op_integer_codepoint_to_string
        table[Opcode.FLOAT_P] = self._op_float_p
        table[Opcode.FLOAT_EQ_P] = self._op_float_eq_p
        table[Opcode.FLOAT_NEQ_P] = self._op_float_neq_p
        table[Opcode.FLOAT_LT_P] = self._op_float_lt_p
        table[Opcode.FLOAT_GT_P] = self._op_float_gt_p
        table[Opcode.FLOAT_LTE_P] = self._op_float_lte_p
        table[Opcode.FLOAT_GTE_P] = self._op_float_gte_p
        table[Opcode.FLOAT_ABS] = self._op_float_abs
        table[Opcode.FLOAT_ADD] = self._op_float_add
        table[Opcode.FLOAT_SUB] = self._op_float_sub
        table[Opcode.FLOAT_MUL] = self._op_float_mul
        table[Opcode.FLOAT_DIV] = self._op_float_div
        table[Opcode.FLOAT_FLOOR_DIV] = self._op_float_floor_div
        table[Opcode.FLOAT_MOD] = self._op_float_mod
        table[Opcode.FLOAT_NEG] = self._op_float_neg
        table[Opcode.FLOAT_EXP] = self._op_float_exp
        table[Opcode.FLOAT_EXPN] = self._op_float_expn
        table[Opcode.FLOAT_LOG] = self._op_float_log
        table[Opcode.FLOAT_LOG10] = self._op_float_log10
        table[Opcode.FLOAT_LOG2] = self._op_float_log2
        table[Opcode.FLOAT_LOGN] = self._op_float_logn
        table[Opcode.FLOAT_SIN] = self._op_float_sin
        table[Opcode.FLOAT_COS] = self._op_float_cos
        table[Opcode.FLOAT_TAN] = self._op_float_tan
        table[Opcode.FLOAT_SQRT] = self._op_float_sqrt
        table[Opcode.FLOAT_TO_INTEGER] = self._op_float_to_integer
        table[Opcode.FLOAT_TO_STRING] = self._op_float_to_string
        table[Opcode.FLOAT_FLOOR] = self._op_float_floor
        table[Opcode.FLOAT_CEIL] = self._op_float_ceil
        table[Opcode.FLOAT_ROUND] = self._op_float_round
        table[Opcode.FLOAT_MIN] = self._op_float_min
        table[Opcode.FLOAT_MAX] = self._op_float_max
        table[Opcode.FLOAT_TO_COMPLEX] = self._op_float_to_complex
        table[Opcode.COMPLEX_P] = self._op_complex_p
        table[Opcode.COMPLEX_EQ_P] = self._op_complex_eq_p
        table[Opcode.COMPLEX_NEQ_P] = self._op_complex_neq_p
        table[Opcode.COMPLEX_REAL] = self._op_complex_real
        table[Opcode.COMPLEX_IMAG] = self._op_complex_imag
        table[Opcode.COMPLEX_ABS] = self._op_complex_abs
        table[Opcode.COMPLEX_ADD] = self._op_complex_add
        table[Opcode.COMPLEX_SUB] = self._op_complex_sub
        table[Opcode.COMPLEX_MUL] = self._op_complex_mul
        table[Opcode.COMPLEX_DIV] = self._op_complex_div
        table[Opcode.COMPLEX_NEG] = self._op_complex_neg
        table[Opcode.COMPLEX_EXP] = self._op_complex_exp
        table[Opcode.COMPLEX_EXPN] = self._op_complex_expn
        table[Opcode.COMPLEX_LOG] = self._op_complex_log
        table[Opcode.COMPLEX_LOG10] = self._op_complex_log10
        table[Opcode.COMPLEX_LOGN] = self._op_complex_logn
        table[Opcode.COMPLEX_SIN] = self._op_complex_sin
        table[Opcode.COMPLEX_COS] = self._op_complex_cos
        table[Opcode.COMPLEX_TAN] = self._op_complex_tan
        table[Opcode.COMPLEX_SQRT] = self._op_complex_sqrt
        table[Opcode.COMPLEX_TO_STRING] = self._op_complex_to_string
        table[Opcode.STRING_P] = self._op_string_p
        table[Opcode.STRING_EQ_P] = self._op_string_eq_p
        table[Opcode.STRING_NEQ_P] = self._op_string_neq_p
        table[Opcode.STRING_LT_P] = self._op_string_lt_p
        table[Opcode.STRING_GT_P] = self._op_string_gt_p
        table[Opcode.STRING_LTE_P] = self._op_string_lte_p
        table[Opcode.STRING_GTE_P] = self._op_string_gte_p
        table[Opcode.STRING_LENGTH] = self._op_string_length
        table[Opcode.STRING_UPCASE] = self._op_string_upcase
        table[Opcode.STRING_DOWNCASE] = self._op_string_downcase
        table[Opcode.STRING_TRIM] = self._op_string_trim
        table[Opcode.STRING_TRIM_LEFT] = self._op_string_trim_left
        table[Opcode.STRING_TRIM_RIGHT] = self._op_string_trim_right
        table[Opcode.STRING_TO_INTEGER] = self._op_string_to_integer
        table[Opcode.STRING_TO_NUMBER] = self._op_string_to_number
        table[Opcode.STRING_TO_LIST] = self._op_string_to_list
        table[Opcode.STRING_REF] = self._op_string_ref
        table[Opcode.STRING_PREFIX_P] = self._op_string_prefix_p
        table[Opcode.STRING_SUFFIX_P] = self._op_string_suffix_p
        table[Opcode.STRING_CONCAT] = self._op_string_concat
        table[Opcode.STRING_SLICE] = self._op_string_slice
        table[Opcode.STRING_REPLACE] = self._op_string_replace
        table[Opcode.STRING_INDEX] = self._op_string_index
        table[Opcode.STRING_TO_INTEGER_CODEPOINT] = self._op_string_to_integer_codepoint
        table[Opcode.DICT_P] = self._op_dict_p
        table[Opcode.DICT_EQ_P] = self._op_dict_eq_p
        table[Opcode.DICT_NEQ_P] = self._op_dict_neq_p
        table[Opcode.DICT_KEYS] = self._op_dict_keys
        table[Opcode.DICT_VALUES] = self._op_dict_values
        table[Opcode.DICT_LENGTH] = self._op_dict_length
        table[Opcode.DICT_HAS_P] = self._op_dict_has_p
        table[Opcode.DICT_REMOVE] = self._op_dict_remove
        table[Opcode.DICT_MERGE] = self._op_dict_merge
        table[Opcode.DICT_SET] = self._op_dict_set
        table[Opcode.DICT_GET] = self._op_dict_get
        table[Opcode.LIST_P] = self._op_list_p
        table[Opcode.LIST_EQ_P] = self._op_list_eq_p
        table[Opcode.LIST_NEQ_P] = self._op_list_neq_p
        table[Opcode.LIST_PREPEND] = self._op_list_prepend
        table[Opcode.LIST_APPEND] = self._op_list_append
        table[Opcode.LIST_LENGTH] = self._op_list_length
        table[Opcode.LIST_REVERSE] = self._op_list_reverse
        table[Opcode.LIST_FIRST] = self._op_list_first
        table[Opcode.LIST_REST] = self._op_list_rest
        table[Opcode.LIST_LAST] = self._op_list_last
        table[Opcode.LIST_REF] = self._op_list_ref
        table[Opcode.LIST_NULL_P] = self._op_list_null_p
        table[Opcode.LIST_MEMBER_P] = self._op_list_member_p
        table[Opcode.LIST_INDEX] = self._op_list_index
        table[Opcode.LIST_SLICE] = self._op_list_slice
        table[Opcode.LIST_REMOVE] = self._op_list_remove
        table[Opcode.LIST_CONCAT] = self._op_list_concat
        table[Opcode.LIST_TO_STRING] = self._op_list_to_string
        table[Opcode.RANGE] = self._op_range
        return table

    def _check_and_pack_args(self, func: MenaiFunction, arity: int) -> None:
        """
        Shared arity-check and variadic-pack logic for CALL, TAIL_CALL, APPLY, TAIL_APPLY.

        Verifies that `arity` satisfies `func`'s parameter requirements and, for
        variadic functions, packs excess arguments into a list on the stack.
        Raises MenaiEvalError on arity mismatch.
        """
        expected_arity = func.bytecode.param_count
        if func.bytecode.is_variadic:
            # Variadic: must have at least (param_count - 1) fixed args.
            # The last local receives all remaining args packed into a list.
            min_arity = expected_arity - 1
            if arity < min_arity:
                func_name = func.name or "<lambda>"
                raise MenaiEvalError(
                    message=f"Function '{func_name}' expects at least {min_arity} arguments, got {arity}",
                    suggestion=f"Provide at least {min_arity} argument{'s' if min_arity != 1 else ''}"
                )

            rest_count = arity - min_arity
            if rest_count == 0:
                self.stack.append(MenaiList(()))
                return

            rest_elements = tuple(self.stack[-rest_count:])
            del self.stack[-rest_count:]
            self.stack.append(MenaiList(rest_elements))
            return

        if arity != expected_arity:
            func_name = func.name or "<lambda>"
            raise MenaiEvalError(
                message=f"Function '{func_name}' expects {expected_arity} arguments, got {arity}",
                suggestion=f"Provide exactly {expected_arity} argument{'s' if expected_arity != 1 else ''}"
            )

    def execute(
        self,
        code: CodeObject,
        constants: Dict[str, MenaiValue],
        prelude_functions: Dict[str, MenaiFunction] | None = None
    ) -> MenaiValue:
        """
        Execute a code object and return the result.

        Args:
            code: Compiled code object to execute
            constants: Dictionary of constant values (e.g., pi, e, j)
            prelude_functions: Optional dictionary of prelude functions

        Returns:
            Result value
        """
        # Validate bytecode before execution (if enabled)
        if self.validate_bytecode:
            validate_bytecode(code)

        self.globals = constants.copy()
        if prelude_functions:
            self.globals.update(prelude_functions)

        # Reset state
        self._cancelled = False

        # Reset execution state
        self.stack = []
        self.frames = self.frames[:1]  # Keep only the main sentinel frame
        frame = Frame(code)
        self.frames.append(frame)
        self.current_frame = frame

        # Cache dispatch table in local variable for faster access
        dispatch = self._dispatch_table

        # Cache cancellation check interval for performance
        check_interval = self._cancellation_check_interval

        instruction_count = 0
        instructions = frame.code.instructions
        instructions_len = frame.code_len

        while True:
            # Periodically check for cancellation
            # This adds minimal overhead while allowing timely cancellation
            instruction_count += 1
            if instruction_count >= check_interval:
                if self._cancelled:
                    raise MenaiCancelledException()

                instruction_count = 0

            if frame.ip >= instructions_len:
                # Frame finished without explicit return
                raise MenaiEvalError("Frame execution ended without RETURN instruction")

            instr = instructions[frame.ip]

            # Increment IP before executing (so jumps can override)
            frame.ip += 1

            handler = dispatch[instr.opcode]
            if handler is None:
                raise MenaiEvalError(f"Unimplemented opcode: {instr.opcode}")

            # Call the handler
            result = handler(frame, instr)
            if result is None:
                # Common fast path: handler completed normally.
                continue

            if result is _FRAME_CHANGE:
                # Frame changed (call, return, or tail call); re-sync.
                frame = self.frames[-1]
                instructions = frame.code.instructions
                instructions_len = frame.code_len
                continue

            # MenaiValue returned only by _op_return when the sentinel frame is
            # the sole remaining frame — this is the top-level result.
            return cast(MenaiValue, result)

    def _op_load_none(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LOAD_NONE dest: Write #none into register dest."""
        frame.locals[instr.dest] = Menai_NONE
        return None

    def _op_load_true(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LOAD_TRUE dest: Write boolean true into register dest."""
        frame.locals[instr.dest] = MenaiBoolean(True)
        return None

    def _op_load_false(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LOAD_FALSE dest: Write boolean false into register dest."""
        frame.locals[instr.dest] = MenaiBoolean(False)
        return None

    def _op_load_empty_list(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LOAD_EMPTY_LIST dest: Write empty list into register dest."""
        frame.locals[instr.dest] = MenaiList(())
        return None

    def _op_load_empty_dict(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LOAD_EMPTY_DICT dest: Write empty dict into register dest."""
        frame.locals[instr.dest] = MenaiDict(())
        return None

    def _op_load_const(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LOAD_CONST dest, src0: Write constant[src0] into register dest."""
        frame.locals[instr.dest] = frame.code.constants[instr.src0]
        return None

    def _op_load_name(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LOAD_NAME dest, src0: Load global[names[src0]] into register dest."""
        name = frame.code.names[instr.src0]

        # Load from globals
        if name not in self.globals:
            # Not found - generate helpful error
            available_vars = list(self.globals.keys())
            similar = difflib.get_close_matches(name, available_vars, n=3, cutoff=0.6)

            suggestion_text = (
                f"Did you mean: {', '.join(similar)}?" if similar
                else "Check spelling or define it in a let binding"
            )

            raise MenaiEvalError(
                message=f"Undefined variable: '{name}'",
                context=(
                    f"Available variables: "
                    f"{', '.join(sorted(available_vars)[:10])}{'...' if len(available_vars) > 10 else ''}"
                ),
                suggestion=suggestion_text,
                example=f"(let (({name} some-value)) ...)"
            )

        frame.locals[instr.dest] = self.globals[name]
        return None

    def _op_push(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """PUSH src0: Push the value in register src0 onto the call stack."""
        # Validator guarantees src0 is in bounds AND variable is initialized
        value = frame.locals[instr.src0]
        self.stack.append(cast(MenaiValue, value))
        return None

    def _op_pop(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """POP dest: Pop the call stack top into register dest."""
        # Validator guarantees dest is in bounds and stack has value
        value = self.stack.pop()
        frame.locals[instr.dest] = value
        return None

    def _op_move(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """MOVE dest, src0: Copy the value in register src0 into register dest."""
        # Validator guarantees src0 is in bounds and initialized, dest is in bounds
        frame.locals[instr.dest] = frame.locals[instr.src0]
        return None

    def _op_enter(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """
        ENTER n: Pop n arguments from stack into locals 0..n-1.

        Arguments are pushed left-to-right by the caller, so the last parameter
        is on top of the stack. We pop in reverse order so that param 0 ends up
        in locals[0], param 1 in locals[1], etc.
        """
        # Validator guarantees n >= 1 and stack has at least n values
        for i in range(instr.src0 - 1, -1, -1):
            frame.locals[i] = self.stack.pop()

        return None

    def _op_jump(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """JUMP: Unconditional jump to instruction."""
        frame.ip = instr.src0
        return None

    def _op_jump_if_false(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """JUMP_IF_FALSE r_src0, @target: Read condition from register src0, jump if false."""
        # Validator guarantees src0 is in bounds and target is valid
        # Must keep type check (runtime-dependent)
        condition = frame.locals[instr.src0]
        if not isinstance(condition, MenaiBoolean):
            raise MenaiEvalError("If condition must be boolean")

        if not condition.value:
            frame.ip = instr.src1

        return None

    def _op_jump_if_true(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """JUMP_IF_TRUE r_src0, @target: Read condition from register src0, jump if true."""
        # Validator guarantees src0 is in bounds and target is valid
        # Must keep type check (runtime-dependent)
        condition = frame.locals[instr.src0]
        if not isinstance(condition, MenaiBoolean):
            raise MenaiEvalError("If condition must be boolean")

        if condition.value:
            frame.ip = instr.src1

        return None

    def _op_raise_error(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """RAISE_ERROR: Raise error with message from constant pool."""
        src0 = instr.src0
        # Validator guarantees src0 is in bounds
        # Type check could be removed if we validate constant types, but keep for now
        error_msg = frame.code.constants[src0]
        if not isinstance(error_msg, MenaiString):
            raise MenaiEvalError("RAISE_ERROR requires a string constant")

        raise MenaiEvalError(error_msg.value)

    def _op_make_closure(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """MAKE_CLOSURE dest, src0, 0: Create closure with all capture slots pre-set to None.

        capture_count is always 0: all capture wiring is done by subsequent PATCH_CLOSURE
        instructions (both for letrec mutual-recursion and for ordinary non-letrec closures).
        """
        closure_code = frame.code.code_objects[instr.src0]
        closure = MenaiFunction(
            parameters=tuple(closure_code.param_names),
            name=closure_code.name,
            bytecode=closure_code,
            is_variadic=closure_code.is_variadic,
        )

        # Pre-allocate all free-var slots as None.  PATCH_CLOSURE fills them.
        closure.captured_values = [None] * len(closure_code.free_vars)
        frame.locals[instr.dest] = closure
        return None

    def _op_patch_closure(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """
        PATCH_CLOSURE closure_reg, capture_idx, r_value_reg: Fill a free-var slot on a closure.

        Used in Phase 2 of letrec two-phase initialisation to wire sibling
        closures together after all have been created in Phase 1.

        Args:
            src0 - register holding the closure to patch
            src1 - which captured-values slot to fill
            src2 - register holding the value to store into the capture slot
        """
        closure = frame.locals[instr.src0]
        assert isinstance(closure, MenaiFunction)
        assert isinstance(closure.captured_values, list), "PATCH_CLOSURE: captured_values must be a list (set by MAKE_CLOSURE)"
        closure.captured_values[instr.src1] = frame.locals[instr.src2]
        return None

    def _op_call(
        self, frame: Frame, instr: Instruction
    ) -> _FrameChange:
        """CALL dest, src0, src1: Call func in register src0 with src1 args on stack; result to dest."""
        func = frame.locals[instr.src0]
        if not isinstance(func, MenaiFunction):
            raise MenaiEvalError(
                message="Cannot call non-function value",
                received=f"Attempted to call: {func.describe()} ({func.type_name()})",
                expected="Function (lambda or builtin)",
                suggestion="Only functions can be called"
            )

        self._check_and_pack_args(func, instr.src1)
        code = func.bytecode

        new_frame = Frame(code)
        new_frame.return_dest = instr.dest

        if func.captured_values:
            for i, captured_val in enumerate(func.captured_values):
                new_frame.locals[code.param_count + i] = captured_val

        self.frames.append(new_frame)
        self.current_frame = new_frame
        return _FRAME_CHANGE

    def _op_tail_call(
        self, frame: Frame, instr: Instruction
    ) -> _FrameChange:
        """TAIL_CALL src0, src1: Replace current frame with func in src0; args on stack."""
        func = frame.locals[instr.src0]
        if not isinstance(func, MenaiFunction):
            raise MenaiEvalError(
                message="Cannot call non-function value",
                received=f"Attempted to call: {func.describe()} ({func.type_name()})",
                expected="Function (lambda or builtin)",
                suggestion="Only functions can be called"
            )

        self._check_and_pack_args(func, instr.src1)
        code = func.bytecode
        frame.code = code
        frame.code_len = len(code.instructions)
        frame.ip = 0
        frame.locals = cast(List[MenaiValue], [None] * code.local_count)
        if func.captured_values:
            for i, captured_val in enumerate(func.captured_values):
                frame.locals[code.param_count + i] = captured_val

        return _FRAME_CHANGE

    def _op_apply(
        self, frame: Frame, instr: Instruction
    ) -> _FrameChange:
        """APPLY dest, src0: Apply func in register src0 to arg_list on stack top; result to dest."""
        arg_list = self.stack.pop()
        func = frame.locals[instr.src0]
        if not isinstance(func, MenaiFunction):
            raise MenaiEvalError(
                message="apply: first argument must be a function",
                received=f"Got: {func.describe()} ({func.type_name()})",
                suggestion="Use (apply f args) where f is a lambda or builtin"
            )

        if not isinstance(arg_list, MenaiList):
            raise MenaiEvalError(
                message="apply: second argument must be a list",
                received=f"Got: {arg_list.describe()} ({arg_list.type_name()})",
                suggestion="Use (apply f (list arg1 arg2 ...))"
            )

        for element in arg_list.elements:
            self.stack.append(element)

        arity = len(arg_list.elements)
        self._check_and_pack_args(func, arity)
        code = func.bytecode
        new_frame = Frame(code)
        new_frame.return_dest = instr.dest
        if func.captured_values:
            for i, captured_val in enumerate(func.captured_values):
                new_frame.locals[code.param_count + i] = captured_val

        self.frames.append(new_frame)
        self.current_frame = new_frame
        return _FRAME_CHANGE

    def _op_tail_apply(
        self, frame: Frame, instr: Instruction
    ) -> _FrameChange:
        """TAIL_APPLY src0: Tail apply func in register src0 to arg_list on stack top."""
        arg_list = self.stack.pop()
        func = frame.locals[instr.src0]
        if not isinstance(func, MenaiFunction):
            raise MenaiEvalError(
                message="apply: first argument must be a function",
                received=f"Got: {func.describe()} ({func.type_name()})",
                suggestion="Use (apply f args) where f is a lambda or builtin"
            )

        if not isinstance(arg_list, MenaiList):
            raise MenaiEvalError(
                message="apply: second argument must be a list",
                received=f"Got: {arg_list.describe()} ({arg_list.type_name()})",
                suggestion="Use (apply f (list arg1 arg2 ...))"
            )

        for element in arg_list.elements:
            self.stack.append(element)

        arity = len(arg_list.elements)
        self._check_and_pack_args(func, arity)
        code = func.bytecode
        frame.code = code
        frame.code_len = len(code.instructions)
        frame.ip = 0
        frame.locals = cast(List[MenaiValue], [None] * code.local_count)
        if func.captured_values:
            for i, captured_val in enumerate(func.captured_values):
                frame.locals[code.param_count + i] = captured_val

        return _FRAME_CHANGE

    def _op_return(
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | _FrameChange:
        """RETURN src0: Pop frame, write return value into caller's dest register.

        If the caller is the sentinel frame, return the value to the loop so it
        can exit.  Otherwise write into caller.locals[return_dest] and return
        _FRAME_CHANGE so the loop re-syncs frame from self.frames[-1].
        """
        return_value = cast(MenaiValue, frame.locals[instr.src0])
        self.frames.pop()
        caller = self.frames[-1]
        self.current_frame = caller

        if caller.code.name == "<main>":
            # Returning to the sentinel: this is the final top-level result.
            return return_value

        # Returning to a real caller: store result, signal the loop to re-sync frame.
        caller.locals[frame.return_dest] = return_value
        return _FRAME_CHANGE

    def _op_emit_trace(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """EMIT_TRACE src0: Read value from register src0 and emit to trace watcher."""
        message = frame.locals[instr.src0]

        # Emit trace if watcher is available
        if self.trace_watcher:
            self._emit_trace(cast(MenaiValue, message))

        # Continue execution (no return value)
        return None

    def _op_function_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FUNCTION_P dest, src0: r_dest = (function? r_src0)"""
        value = frame.locals[instr.src0]
        frame.locals[instr.dest] = MenaiBoolean(isinstance(value, MenaiFunction))
        return None

    def _op_function_eq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FUNCTION_EQ_P dest, src0, src1: r_dest = (function=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFunction):
            raise MenaiEvalError(
                message="function=?: arguments must be functions",
                received=f"First argument: {a.describe()} ({a.type_name()})"
            )

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFunction):
            raise MenaiEvalError(
                message="function=?: arguments must be functions",
                received=f"Second argument: {b.describe()} ({b.type_name()})"
            )

        frame.locals[instr.dest] = MenaiBoolean(a is b)
        return None

    def _op_function_neq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FUNCTION_NEQ_P dest, src0, src1: r_dest = (function!=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFunction):
            raise MenaiEvalError(
                message="function!=?: arguments must be functions",
                received=f"First argument: {a.describe()} ({a.type_name()})"
            )

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFunction):
            raise MenaiEvalError(
                message="function!=?: arguments must be functions",
                received=f"Second argument: {b.describe()} ({b.type_name()})"
            )

        frame.locals[instr.dest] = MenaiBoolean(a is not b)
        return None

    def _op_function_min_arity(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FUNCTION_MIN_ARITY dest, src0: r_dest = (function-min-arity r_src0)"""
        func = frame.locals[instr.src0]
        if not isinstance(func, MenaiFunction):
            raise MenaiEvalError(
                message="function-min-arity: argument must be a function",
                received=f"Got: {func.describe()} ({func.type_name()})"
            )

        code = func.bytecode
        min_arity = (code.param_count - 1) if code.is_variadic else code.param_count
        frame.locals[instr.dest] = MenaiInteger(min_arity)
        return None

    def _op_function_variadic_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FUNCTION_VARIADIC_P dest, src0: r_dest = (function-variadic? r_src0)"""
        func = frame.locals[instr.src0]
        if not isinstance(func, MenaiFunction):
            raise MenaiEvalError(
                message="function-variadic?: argument must be a function",
                received=f"Got: {func.describe()} ({func.type_name()})"
            )

        frame.locals[instr.dest] = MenaiBoolean(func.bytecode.is_variadic)
        return None

    def _op_function_accepts_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FUNCTION_ACCEPTS_P dest, src0, src1: r_dest = (function-accepts? r_src0 r_src1)"""
        func = frame.locals[instr.src0]
        if not isinstance(func, MenaiFunction):
            raise MenaiEvalError(
                message="function-accepts?: first argument must be a function",
                received=f"Got: {func.describe()} ({func.type_name()})"
            )

        n = frame.locals[instr.src1]
        if not isinstance(n, MenaiInteger):
            raise MenaiEvalError(
                message="function-accepts?: second argument must be an integer",
                received=f"Got: {n.describe()} ({n.type_name()})"
            )

        code = func.bytecode
        if code.is_variadic:
            min_arity = code.param_count - 1
            result = n.value >= min_arity

        else:
            result = n.value == code.param_count

        frame.locals[instr.dest] = MenaiBoolean(result)
        return None

    def _op_symbol_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """SYMBOL_P dest, src0: r_dest = (symbol? r_src0)"""
        value = frame.locals[instr.src0]
        frame.locals[instr.dest] = MenaiBoolean(isinstance(value, MenaiSymbol))
        return None

    def _op_symbol_eq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """SYMBOL_EQ_P dest, src0, src1: r_dest = (symbol=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiSymbol):
            raise MenaiEvalError(
                message="symbol=?: arguments must be symbols",
                received=f"First argument: {a.describe()} ({a.type_name()})"
            )

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiSymbol):
            raise MenaiEvalError(
                message="symbol=?: arguments must be symbols",
                received=f"Second argument: {b.describe()} ({b.type_name()})"
            )

        frame.locals[instr.dest] = MenaiBoolean(a.name == b.name)
        return None

    def _op_symbol_neq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """SYMBOL_NEQ_P dest, src0, src1: r_dest = (symbol!=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiSymbol):
            raise MenaiEvalError(
                message="symbol!=?: arguments must be symbols",
                received=f"First argument: {a.describe()} ({a.type_name()})"
            )

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiSymbol):
            raise MenaiEvalError(
                message="symbol!=?: arguments must be symbols",
                received=f"Second argument: {b.describe()} ({b.type_name()})"
            )

        frame.locals[instr.dest] = MenaiBoolean(a.name != b.name)
        return None

    def _op_symbol_to_string(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """SYMBOL_TO_STRING dest, src0: r_dest = (symbol->string r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiSymbol):
            raise MenaiEvalError(
                message="symbol->string: argument must be a symbol",
                received=f"Got: {a.describe()} ({a.type_name()})"
            )

        frame.locals[instr.dest] = MenaiString(a.name)
        return None

    def _op_none_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """NONE_P dest, src0: r_dest = (none? r_src0)"""
        value = frame.locals[instr.src0]
        frame.locals[instr.dest] = MenaiBoolean(isinstance(value, MenaiNone))
        return None

    def _op_boolean_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """BOOLEAN_P dest, src0: r_dest = (boolean? r_src0)"""
        value = frame.locals[instr.src0]
        frame.locals[instr.dest] = MenaiBoolean(isinstance(value, MenaiBoolean))
        return None

    def _op_boolean_eq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """BOOLEAN_EQ_P dest, src0, src1: r_dest = (boolean=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiBoolean):
            raise MenaiEvalError(f"Function 'boolean=?' requires boolean arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiBoolean):
            raise MenaiEvalError(f"Function 'boolean=?' requires boolean arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value == b.value)
        return None

    def _op_boolean_neq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """BOOLEAN_NEQ_P dest, src0, src1: r_dest = (boolean!=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiBoolean):
            raise MenaiEvalError(f"Function 'boolean!=?' requires boolean arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiBoolean):
            raise MenaiEvalError(f"Function 'boolean!=?' requires boolean arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value != b.value)
        return None

    def _op_boolean_not(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """BOOLEAN_NOT dest, src0: r_dest = (boolean-not r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiBoolean):
            raise MenaiEvalError(f"Function 'boolean-not' requires boolean arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(not a.value)
        return None

    def _op_integer_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_P dest, src0: r_dest = (integer? r_src0)"""
        frame.locals[instr.dest] = MenaiBoolean(isinstance(frame.locals[instr.src0], MenaiInteger))
        return None

    def _op_integer_eq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_EQ_P dest, src0, src1: r_dest = (integer=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer=?' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer=?' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value == b.value)
        return None

    def _op_integer_neq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_NEQ_P dest, src0, src1: r_dest = (integer!=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer!=?' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer!=?' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value != b.value)
        return None

    def _op_integer_lt_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_LT_P dest, src0, src1: r_dest = (integer<? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer<?' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer<?' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value < b.value)
        return None

    def _op_integer_gt_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_GT_P dest, src0, src1: r_dest = (integer>? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer>?' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer>?' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value > b.value)
        return None

    def _op_integer_lte_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_LTE_P dest, src0, src1: r_dest = (integer<=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer<=?' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer<=?' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value <= b.value)
        return None

    def _op_integer_gte_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_GTE_P dest, src0, src1: r_dest = (integer>=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer>=?' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer>=?' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value >= b.value)
        return None

    def _op_integer_abs(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_ABS dest, src0: r_dest = (integer-abs r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-abs' requires integer arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(abs(a.value))
        return None

    def _op_integer_add(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_ADD dest, src0, src1: r_dest = (integer+ r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer+' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer+' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(a.value + b.value)
        return None

    def _op_integer_sub(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_SUB dest, src0, src1: r_dest = (integer- r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(a.value - b.value)
        return None

    def _op_integer_mul(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_MUL dest, src0, src1: r_dest = (integer* r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer*' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer*' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(a.value * b.value)
        return None

    def _op_integer_div(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_DIV dest, src0, src1: r_dest = (integer/ r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer/' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer/' requires integer arguments, got {b.type_name()}")

        if b.value == 0:
            raise MenaiEvalError("Division by zero in 'integer/'")
        frame.locals[instr.dest] = MenaiInteger(a.value // b.value)
        return None

    def _op_integer_mod(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_MOD dest, src0, src1: r_dest = (integer% r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer%' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer%' requires integer arguments, got {b.type_name()}")

        if b.value == 0:
            raise MenaiEvalError("Modulo by zero in 'integer%'")

        frame.locals[instr.dest] = MenaiInteger(a.value % b.value)
        return None

    def _op_integer_neg(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_NEG dest, src0: r_dest = (integer-neg r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-neg' requires integer arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(-a.value)
        return None

    def _op_integer_expn(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_EXPN dest, src0, src1: r_dest = (integer-expn r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-expn' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-expn' requires integer arguments, got {b.type_name()}")

        if b.value < 0:
            raise MenaiEvalError("Function 'integer-expn' requires a non-negative exponent")

        frame.locals[instr.dest] = MenaiInteger(a.value ** b.value)
        return None

    def _op_integer_bit_not(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_BIT_NOT dest, src0: r_dest = (integer-bit-not r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-bit-not' requires integer arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(~a.value)
        return None

    def _op_integer_bit_shift_left(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_BIT_SHIFT_LEFT dest, src0, src1: r_dest = (integer-bit-shift-left r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-bit-shift-left' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-bit-shift-left' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(a.value << b.value)
        return None

    def _op_integer_bit_shift_right(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_BIT_SHIFT_RIGHT dest, src0, src1: r_dest = (integer-bit-shift-right r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-bit-shift-right' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-bit-shift-right' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(a.value >> b.value)
        return None

    def _op_integer_bit_or(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_BIT_OR dest, src0, src1: r_dest = (integer-bit-or r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-bit-or' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-bit-or' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(a.value | b.value)
        return None

    def _op_integer_bit_and(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_BIT_AND dest, src0, src1: r_dest = (integer-bit-and r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-bit-and' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-bit-and' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(a.value & b.value)
        return None

    def _op_integer_bit_xor(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_BIT_XOR dest, src0, src1: r_dest = (integer-bit-xor r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-bit-xor' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-bit-xor' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(a.value ^ b.value)
        return None

    def _op_integer_min(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_MIN dest, src0, src1: r_dest = (integer-min r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-min' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-min' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(a.value if a.value <= b.value else b.value)
        return None

    def _op_integer_max(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_MAX dest, src0, src1: r_dest = (integer-max r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-max' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer-max' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(a.value if a.value >= b.value else b.value)
        return None

    def _op_integer_to_float(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_TO_FLOAT dest, src0: r_dest = (integer->float r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer->float' requires integer arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(float(a.value))
        return None

    def _op_integer_to_complex(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_TO_COMPLEX dest, src0, src1: r_dest = (integer->complex r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer->complex' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer->complex' requires integer arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(complex(float(a.value), float(b.value)))
        return None

    def _op_integer_to_string(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_TO_STRING dest, src0, src1: r_dest = (integer->string r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer->string' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'integer->string' requires integer arguments, got {b.type_name()}")

        a_val = a.value
        radix = b.value
        if radix not in (2, 8, 10, 16):
            raise MenaiEvalError(f"integer->string radix must be 2, 8, 10, or 16, got {radix}")

        if radix == 10:
            frame.locals[instr.dest] = MenaiString(str(a_val))
            return None

        if radix == 2:
            sign = "-" if a_val < 0 else ""
            frame.locals[instr.dest] = MenaiString(f"{sign}{bin(abs(a_val))[2:]}")
            return None

        if radix == 8:
            sign = "-" if a_val < 0 else ""
            frame.locals[instr.dest] = MenaiString(f"{sign}{oct(abs(a_val))[2:]}")
            return None

        if radix == 16:
            sign = "-" if a_val < 0 else ""
            frame.locals[instr.dest] = MenaiString(f"{sign}{hex(abs(a_val))[2:]}")

        return None

    def _op_integer_codepoint_to_string(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """INTEGER_CODEPOINT_TO_STRING dest, src0: r_dest = (integer-codepoint->string r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(
                f"Function 'integer-codepoint->string' requires an integer argument, got {a.type_name()}"
            )

        cp = a.value
        if not (0 <= cp <= 0x10FFFF) or (0xD800 <= cp <= 0xDFFF):
            raise MenaiEvalError(
                f"Function 'integer-codepoint->string' requires a valid Unicode scalar value, got {cp}"
            )

        frame.locals[instr.dest] = MenaiString(chr(cp))
        return None

    def _op_float_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_P dest, src0: r_dest = (float? r_src0)"""
        frame.locals[instr.dest] = MenaiBoolean(isinstance(frame.locals[instr.src0], MenaiFloat))
        return None

    def _op_float_eq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_EQ_P dest, src0, src1: r_dest = (float=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float=?' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float=?' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value == b.value)
        return None

    def _op_float_neq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_NEQ_P dest, src0, src1: r_dest = (float!=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float!=?' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float!=?' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value != b.value)
        return None

    def _op_float_lt_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_LT_P dest, src0, src1: r_dest = (float<? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float<?' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float<?' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value < b.value)
        return None

    def _op_float_gt_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_GT_P dest, src0, src1: r_dest = (float>? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float>?' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float>?' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value > b.value)
        return None

    def _op_float_lte_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_LTE_P dest, src0, src1: r_dest = (float<=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float<=?' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float<=?' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value <= b.value)
        return None

    def _op_float_gte_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_GTE_P dest, src0, src1: r_dest = (float>=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float>=?' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float>=?' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value >= b.value)
        return None

    def _op_float_abs(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_ABS dest, src0: r_dest = (float-abs r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-abs' requires float arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(abs(a.value))
        return None

    def _op_float_add(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_ADD dest, src0, src1: r_dest = (float+ r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float+' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float+' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(a.value + b.value)
        return None

    def _op_float_sub(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_SUB dest, src0, src1: r_dest = (float- r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(a.value - b.value)
        return None

    def _op_float_mul(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_MUL dest, src0, src1: r_dest = (float* r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float*' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float*' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(a.value * b.value)
        return None

    def _op_float_div(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_DIV dest, src0, src1: r_dest = (float/ r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float/' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float/' requires float arguments, got {b.type_name()}")

        if b.value == 0.0:
            raise MenaiEvalError("Division by zero in 'float/'")

        frame.locals[instr.dest] = MenaiFloat(a.value / b.value)
        return None

    def _op_float_floor_div(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_FLOOR_DIV dest, src0, src1: r_dest = (float// r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float//' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float//' requires float arguments, got {b.type_name()}")

        if b.value == 0:
            raise MenaiEvalError("Division by zero")

        frame.locals[instr.dest] = MenaiFloat(float(a.value // b.value))
        return None

    def _op_float_mod(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_MOD dest, src0, src1: r_dest = (float% r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float%' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float%' requires float arguments, got {b.type_name()}")

        if b.value == 0:
            raise MenaiEvalError("Modulo by zero")

        frame.locals[instr.dest] = MenaiFloat(a.value % b.value)
        return None

    def _op_float_neg(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_NEG dest, src0: r_dest = (float-neg r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-neg' requires float arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(-a.value)
        return None

    def _op_float_exp(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_EXP dest, src0: r_dest = (float-exp r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-exp' requires float arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(math.exp(a.value))
        return None

    def _op_float_expn(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_EXPN dest, src0, src1: r_dest = (float-expn r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-expn' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-expn' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(a.value ** b.value)
        return None

    def _op_float_log(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_LOG dest, src0: r_dest = (float-log r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-log' requires float arguments, got {a.type_name()}")

        if a.value == 0.0:
            frame.locals[instr.dest] = MenaiFloat(float('-inf'))
            return None

        if a.value < 0.0:
            raise MenaiEvalError("Function 'float-log' requires a non-negative argument")

        frame.locals[instr.dest] = MenaiFloat(math.log(a.value))
        return None

    def _op_float_log10(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_LOG10 dest, src0: r_dest = (float-log10 r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-log10' requires float arguments, got {a.type_name()}")

        if a.value == 0.0:
            frame.locals[instr.dest] = MenaiFloat(float('-inf'))
            return None

        if a.value < 0.0:
            raise MenaiEvalError("Function 'float-log10' requires a non-negative argument")

        frame.locals[instr.dest] = MenaiFloat(math.log10(a.value))
        return None

    def _op_float_log2(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_LOG2 dest, src0: r_dest = (float-log2 r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-log2' requires float arguments, got {a.type_name()}")

        if a.value == 0.0:
            frame.locals[instr.dest] = MenaiFloat(float('-inf'))
            return None

        if a.value < 0.0:
            raise MenaiEvalError("Function 'float-log2' requires a non-negative argument")

        frame.locals[instr.dest] = MenaiFloat(math.log2(a.value))
        return None

    def _op_float_logn(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_LOGN dest, src0, src1: r_dest = (float-logn r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-logn' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-logn' requires float arguments, got {b.type_name()}")

        if b.value <= 0.0 or b.value == 1.0:
            raise MenaiEvalError("Function 'float-logn' requires a positive base not equal to 1")

        if a.value == 0.0:
            frame.locals[instr.dest] = MenaiFloat(float('-inf'))
            return None

        if a.value < 0.0:
            raise MenaiEvalError("Function 'float-logn' requires a non-negative argument")

        frame.locals[instr.dest] = MenaiFloat(math.log(a.value, b.value))
        return None

    def _op_float_sin(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_SIN dest, src0: r_dest = (float-sin r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-sin' requires float arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(math.sin(a.value))
        return None

    def _op_float_cos(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_COS dest, src0: r_dest = (float-cos r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-cos' requires float arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(math.cos(a.value))
        return None

    def _op_float_tan(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_TAN dest, src0: r_dest = (float-tan r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-tan' requires float arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(math.tan(a.value))
        return None

    def _op_float_sqrt(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_SQRT dest, src0: r_dest = (float-sqrt r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-sqrt' requires float arguments, got {a.type_name()}")

        if a.value < 0.0:
            raise MenaiEvalError("Function 'float-sqrt' requires a non-negative argument")

        frame.locals[instr.dest] = MenaiFloat(math.sqrt(a.value))
        return None

    def _op_float_to_integer(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_TO_INTEGER dest, src0: r_dest = (float->integer r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float->integer' requires float arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(int(a.value))
        return None

    def _op_float_to_complex(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_TO_COMPLEX dest, src0, src1: r_dest = (float->complex r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float->complex' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float->complex' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(complex(a.value, b.value))
        return None

    def _op_float_to_string(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_TO_STRING dest, src0: r_dest = (float->string r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float->string' requires float arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiString(str(a.value))
        return None

    def _op_float_floor(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_FLOOR dest, src0: r_dest = (float-floor r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-floor' requires float arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(float(math.floor(a.value)))
        return None

    def _op_float_ceil(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_CEIL dest, src0: r_dest = (float-ceil r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-ceil' requires float arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(float(math.ceil(a.value)))
        return None

    def _op_float_round(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_ROUND dest, src0: r_dest = (float-round r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-round' requires float arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(float(round(a.value)))
        return None

    def _op_float_min(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_MIN dest, src0, src1: r_dest = (float-min r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-min' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-min' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(a.value if a.value <= b.value else b.value)
        return None

    def _op_float_max(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """FLOAT_MAX dest, src0, src1: r_dest = (float-max r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-max' requires float arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiFloat):
            raise MenaiEvalError(f"Function 'float-max' requires float arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(a.value if a.value >= b.value else b.value)
        return None

    def _op_complex_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_P dest, src0: r_dest = (complex? r_src0)"""
        frame.locals[instr.dest] = MenaiBoolean(isinstance(frame.locals[instr.src0], MenaiComplex))
        return None

    def _op_complex_eq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_EQ_P dest, src0, src1: r_dest = (complex=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex=?' requires complex arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex=?' requires complex arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value == b.value)
        return None

    def _op_complex_neq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_NEQ_P dest, src0, src1: r_dest = (complex!=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex!=?' requires complex arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex!=?' requires complex arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value != b.value)
        return None

    def _op_complex_real(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_REAL dest, src0: r_dest = (complex-real r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-real' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(a.value.real)
        return None

    def _op_complex_imag(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_IMAG dest, src0: r_dest = (complex-imag r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-imag' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(a.value.imag)
        return None

    def _op_complex_abs(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_ABS dest, src0: r_dest = (complex-abs r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-abs' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiFloat(abs(a.value))
        return None

    def _op_complex_add(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_ADD dest, src0, src1: r_dest = (complex+ r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex+' requires complex arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex+' requires complex arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(a.value + b.value)
        return None

    def _op_complex_sub(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_SUB dest, src0, src1: r_dest = (complex- r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-' requires complex arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-' requires complex arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(a.value - b.value)
        return None

    def _op_complex_mul(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_MUL dest, src0, src1: r_dest = (complex* r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex*' requires complex arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex*' requires complex arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(a.value * b.value)
        return None

    def _op_complex_div(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_DIV dest, src0, src1: r_dest = (complex/ r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex/' requires complex arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex/' requires complex arguments, got {b.type_name()}")

        if b.value == 0:
            raise MenaiEvalError("Division by zero in 'complex/'")

        frame.locals[instr.dest] = MenaiComplex(a.value / b.value)
        return None

    def _op_complex_neg(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_NEG dest, src0: r_dest = (complex-neg r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-neg' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(-a.value)
        return None

    def _op_complex_exp(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_EXP dest, src0: r_dest = (complex-exp r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-exp' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(cmath.exp(a.value))
        return None

    def _op_complex_expn(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_EXPN dest, src0, src1: r_dest = (complex-expn r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-expn' requires complex arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-expn' requires complex arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(a.value ** b.value)
        return None

    def _op_complex_log(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_LOG dest, src0: r_dest = (complex-log r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-log' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(cmath.log(a.value))
        return None

    def _op_complex_log10(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_LOG10 dest, src0: r_dest = (complex-log10 r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-log10' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(cmath.log10(a.value))
        return None

    def _op_complex_logn(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_LOGN dest, src0, src1: r_dest = (complex-logn r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-logn' requires complex arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-logn' requires complex arguments, got {b.type_name()}")

        if b.value == 0j:
            raise MenaiEvalError("Function 'complex-logn' requires a non-zero base")

        frame.locals[instr.dest] = MenaiComplex(cmath.log(a.value, b.value))
        return None

    def _op_complex_sin(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_SIN dest, src0: r_dest = (complex-sin r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-sin' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(cmath.sin(a.value))
        return None

    def _op_complex_cos(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_COS dest, src0: r_dest = (complex-cos r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-cos' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(cmath.cos(a.value))
        return None

    def _op_complex_tan(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_TAN dest, src0: r_dest = (complex-tan r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-tan' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(cmath.tan(a.value))
        return None

    def _op_complex_sqrt(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_SQRT dest, src0: r_dest = (complex-sqrt r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex-sqrt' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiComplex(cmath.sqrt(a.value))
        return None

    def _op_complex_to_string(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """COMPLEX_TO_STRING dest, src0: r_dest = (complex->string r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiComplex):
            raise MenaiEvalError(f"Function 'complex->string' requires complex arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiString(str(a.value).strip('()'))
        return None

    def _op_string_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_P dest, src0: r_dest = (string? r_src0)"""
        frame.locals[instr.dest] = MenaiBoolean(isinstance(frame.locals[instr.src0], MenaiString))
        return None

    def _op_string_eq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_EQ_P dest, src0, src1: r_dest = (string=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string=?' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string=?' requires string arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value == b.value)
        return None

    def _op_string_neq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_NEQ_P dest, src0, src1: r_dest = (string!=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string!=?' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string!=?' requires string arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value != b.value)
        return None

    def _op_string_lt_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_LT_P dest, src0, src1: r_dest = (string<? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string<?' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string<?' requires string arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value < b.value)
        return None

    def _op_string_gt_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_GT_P dest, src0, src1: r_dest = (string>? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string>?' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string>?' requires string arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value > b.value)
        return None

    def _op_string_lte_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_LTE_P dest, src0, src1: r_dest = (string<=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string<=?' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string<=?' requires string arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value <= b.value)
        return None

    def _op_string_gte_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_GTE_P dest, src0, src1: r_dest = (string>=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string>=?' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string>=?' requires string arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value >= b.value)
        return None

    def _op_string_length(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_LENGTH dest, src0: r_dest = (string-length r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-length' requires string arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(len(a.value))
        return None

    def _op_string_upcase(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_UPCASE dest, src0: r_dest = (string-upcase r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-upcase' requires string arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiString(a.value.upper())
        return None

    def _op_string_downcase(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_DOWNCASE dest, src0: r_dest = (string-downcase r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-downcase' requires string arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiString(a.value.lower())
        return None

    def _op_string_trim(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_TRIM dest, src0: r_dest = (string-trim r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-trim' requires string arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiString(a.value.strip())
        return None

    def _op_string_trim_left(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_TRIM_LEFT dest, src0: r_dest = (string-trim-left r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-trim-left' requires string arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiString(a.value.lstrip())
        return None

    def _op_string_trim_right(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_TRIM_RIGHT dest, src0: r_dest = (string-trim-right r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-trim-right' requires string arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiString(a.value.rstrip())
        return None

    def _op_string_to_integer(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_TO_INTEGER dest, src0, src1: r_dest = (string->integer r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string->integer' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'string->integer' requires integer arguments, got {b.type_name()}")

        s = a.value
        radix = b.value
        if radix not in (2, 8, 10, 16):
            raise MenaiEvalError(f"string->integer radix must be 2, 8, 10, or 16, got {radix}")

        try:
            frame.locals[instr.dest] = MenaiInteger(int(s, radix))
            return None

        except ValueError:
            frame.locals[instr.dest] = Menai_NONE
            return None

    def _op_string_to_number(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_TO_NUMBER dest, src0: r_dest = (string->number r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string->number' requires string arguments, got {a.type_name()}")

        s = a.value

        try:
            if '.' not in s and 'e' not in s.lower() and 'j' not in s.lower():
                frame.locals[instr.dest] = MenaiInteger(int(s))
                return None

            if 'j' in s.lower():
                frame.locals[instr.dest] = MenaiComplex(complex(s))
                return None

            frame.locals[instr.dest] = MenaiFloat(float(s))
            return None

        except ValueError:
            frame.locals[instr.dest] = Menai_NONE
            return None

    def _op_string_to_list(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_TO_LIST dest, src0, src1: r_dest = (string->list r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string->list' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string->list' requires string arguments, got {b.type_name()}")

        if b.value == "":
            frame.locals[instr.dest] = MenaiList(tuple(MenaiString(ch) for ch in a.value))
            return None

        frame.locals[instr.dest] = MenaiList(tuple(MenaiString(part) for part in a.value.split(b.value)))
        return None

    def _op_string_ref(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_REF dest, src0, src1: r_dest = (string-ref r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-ref' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'string-ref' requires integer arguments, got {b.type_name()}")

        s = a.value
        index = b.value
        if index < 0 or index >= len(s):
            raise MenaiEvalError(f"string-ref index out of range: {index}")

        frame.locals[instr.dest] = MenaiString(s[index])
        return None

    def _op_string_prefix_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_PREFIX_P dest, src0, src1: r_dest = (string-prefix? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-prefix?' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string-prefix?' requires string arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value.startswith(b.value))
        return None

    def _op_string_suffix_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_SUFFIX_P dest, src0, src1: r_dest = (string-suffix? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-suffix?' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string-suffix?' requires string arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a.value.endswith(b.value))
        return None

    def _op_string_slice(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_SLICE dest, src0, src1, src2: r_dest = (string-slice r_src0 r_src1 r_src2)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-slice' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'string-slice' requires integer arguments, got {b.type_name()}")

        c = frame.locals[instr.src2]
        if not isinstance(c, MenaiInteger):
            raise MenaiEvalError(f"Function 'string-slice' requires integer arguments, got {c.type_name()}")

        s = a.value
        start = b.value
        end = c.value
        n = len(s)
        if start < 0:
            raise MenaiEvalError(f"string-slice start index cannot be negative: {start}")

        if end < 0:
            raise MenaiEvalError(f"string-slice end index cannot be negative: {end}")

        if start > n:
            raise MenaiEvalError(f"string-slice start index out of range: {start} (string length: {n})")

        if end > n:
            raise MenaiEvalError(f"string-slice end index out of range: {end} (string length: {n})")

        if start > end:
            raise MenaiEvalError(f"string-slice start index ({start}) cannot be greater than end index ({end})")

        frame.locals[instr.dest] = MenaiString(s[start:end])
        return None

    def _op_string_replace(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_REPLACE dest, src0, src1, src2: r_dest = (string-replace r_src0 r_src1 r_src2)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-replace' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string-replace' requires string arguments, got {b.type_name()}")

        c = frame.locals[instr.src2]
        if not isinstance(c, MenaiString):
            raise MenaiEvalError(f"Function 'string-replace' requires string arguments, got {c.type_name()}")

        frame.locals[instr.dest] = MenaiString(a.value.replace(b.value, c.value))
        return None

    def _op_string_index(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_INDEX dest, src0, src1: r_dest = (string-index r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-index' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string-index' requires string arguments, got {b.type_name()}")

        idx = a.value.find(b.value)
        frame.locals[instr.dest] = Menai_NONE if idx == -1 else MenaiInteger(idx)
        return None

    def _op_string_to_integer_codepoint(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_TO_INTEGER_CODEPOINT dest, src0: r_dest = (string->integer-codepoint r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(
                f"Function 'string->integer-codepoint' requires a string argument, got {a.type_name()}"
            )

        if len(a.value) != 1:
            raise MenaiEvalError(
                f"Function 'string->integer-codepoint' requires a single-character string, "
                f"got string of length {len(a.value)}"
            )

        frame.locals[instr.dest] = MenaiInteger(ord(a.value))
        return None

    def _op_string_concat(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """STRING_CONCAT dest, src0, src1: r_dest = (string-concat r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiString):
            raise MenaiEvalError(f"Function 'string-concat' requires string arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'string-concat' requires string arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiString(a.value + b.value)
        return None

    def _op_dict_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """DICT_P dest, src0: r_dest = (dict? r_src0)"""
        frame.locals[instr.dest] = MenaiBoolean(isinstance(frame.locals[instr.src0], MenaiDict))
        return None

    def _op_dict_eq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """DICT_EQ_P dest, src0, src1: r_dest = (dict=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiDict):
            raise MenaiEvalError(f"Function 'dict=?' requires dict arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiDict):
            raise MenaiEvalError(f"Function 'dict=?' requires dict arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a == b)
        return None

    def _op_dict_neq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """DICT_NEQ_P dest, src0, src1: r_dest = (dict!=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiDict):
            raise MenaiEvalError(f"Function 'dict!=?' requires dict arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiDict):
            raise MenaiEvalError(f"Function 'dict!=?' requires dict arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a != b)
        return None

    def _op_dict_keys(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """DICT_KEYS dest, src0: r_dest = (dict-keys r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiDict):
            raise MenaiEvalError(f"Function 'dict-keys' requires dict arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiList(tuple(k for k, _ in a.pairs))
        return None

    def _op_dict_values(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """DICT_VALUES dest, src0: r_dest = (dict-values r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiDict):
            raise MenaiEvalError(f"Function 'dict-values' requires dict arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiList(tuple(v for _, v in a.pairs))
        return None

    def _op_dict_length(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """DICT_LENGTH dest, src0: r_dest = (dict-length r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiDict):
            raise MenaiEvalError(f"Function 'dict-length' requires dict arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiInteger(len(a.pairs))
        return None

    def _op_dict_has_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """DICT_HAS_P dest, src0, src1: r_dest = (dict-has? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiDict):
            raise MenaiEvalError(f"Function 'dict-has?' requires dict arguments, got {a.type_name()}")

        try:
            hashable_key = a.to_hashable_key(cast(MenaiValue, frame.locals[instr.src1]))
            frame.locals[instr.dest] = MenaiBoolean(hashable_key in a.lookup)

        except MenaiEvalError as e:
            raise MenaiEvalError(f"Function 'dict-has?' invalid key: {e.message}") from e

        return None

    def _op_dict_remove(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """DICT_REMOVE dest, src0, src1: r_dest = (dict-remove r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiDict):
            raise MenaiEvalError(f"Function 'dict-remove' requires dict arguments, got {a.type_name()}")

        try:
            hashable_key = a.to_hashable_key(cast(MenaiValue, frame.locals[instr.src1]))
            new_pairs = tuple(
                (k, v) for k, v in a.pairs
                if a.to_hashable_key(k) != hashable_key
            )
            frame.locals[instr.dest] = MenaiDict(new_pairs)

        except MenaiEvalError as e:
            raise MenaiEvalError(f"Function 'dict-remove' invalid key: {e.message}") from e

        return None

    def _op_dict_merge(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """DICT_MERGE dest, src0, src1: r_dest = (dict-merge r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiDict):
            raise MenaiEvalError(f"Function 'dict-merge' requires dict arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiDict):
            raise MenaiEvalError(f"Function 'dict-merge' requires dict arguments, got {b.type_name()}")

        # Start with self's pairs
        result_dict = {}
        for k, v in a.pairs:
            hashable_key = a.to_hashable_key(k)
            result_dict[hashable_key] = (k, v)

        # Override/add from other
        for k, v in b.pairs:
            hashable_key = b.to_hashable_key(k)
            result_dict[hashable_key] = (k, v)

        # Preserve insertion order: self's keys first, then other's new keys
        new_pairs = []
        seen = set()

        # Add all of self's keys (with potentially updated values)
        for k, _ in a.pairs:
            hashable_key = a.to_hashable_key(k)
            new_pairs.append(result_dict[hashable_key])
            seen.add(hashable_key)

        # Add other's keys that weren't in self
        for k, v in b.pairs:
            hashable_key = b.to_hashable_key(k)
            if hashable_key not in seen:
                new_pairs.append((k, v))

        frame.locals[instr.dest] = MenaiDict(tuple(new_pairs))
        return None

    def _op_dict_set(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """DICT_SET dest, src0, src1, src2: r_dest = (dict-set r_src0 r_src1 r_src2)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiDict):
            raise MenaiEvalError(f"Function 'dict-set' requires dict arguments, got {a.type_name()}")

        try:
            hashable_key = a.to_hashable_key(cast(MenaiValue, frame.locals[instr.src1]))

            # Build new pairs list, replacing or appending
            new_pairs = []
            found = False

            key = cast(MenaiValue, frame.locals[instr.src1])
            value = cast(MenaiValue, frame.locals[instr.src2])

            for k, v in a.pairs:
                if a.to_hashable_key(k) == hashable_key:
                    new_pairs.append((key, value))  # Replace with new value
                    found = True

                else:
                    new_pairs.append((k, v))

            if not found:
                new_pairs.append((key, value))  # Append new pair

            frame.locals[instr.dest] = MenaiDict(tuple(new_pairs))

        except MenaiEvalError as e:
            raise MenaiEvalError(f"Function 'dict-set' invalid key: {e.message}") from e

        return None

    def _op_dict_get(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """DICT_GET dest, src0, src1, src2: r_dest = (dict-get r_src0 r_src1 r_src2)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiDict):
            raise MenaiEvalError(f"Function 'dict-get' requires dict arguments, got {a.type_name()}")

        try:
            hashable_key = a.to_hashable_key(cast(MenaiValue, frame.locals[instr.src1]))
            if hashable_key in a.lookup:
                _, value = a.lookup[hashable_key]
                frame.locals[instr.dest] = value
                return None

            frame.locals[instr.dest] = cast(MenaiValue, frame.locals[instr.src2])

        except MenaiEvalError as e:
            raise MenaiEvalError(f"Function 'dict-get' invalid key: {e.message}") from e

        return None

    def _op_list_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_P dest, src0: r_dest = (list? r_src0)"""
        frame.locals[instr.dest] = MenaiBoolean(isinstance(frame.locals[instr.src0], MenaiList))
        return None

    def _op_list_eq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_EQ_P dest, src0, src1: r_dest = (list=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list=?' requires list arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiList):
            raise MenaiEvalError(f"Function 'list=?' requires list arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a == b)
        return None

    def _op_list_neq_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_NEQ_P dest, src0, src1: r_dest = (list!=? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list!=?' requires list arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiList):
            raise MenaiEvalError(f"Function 'list!=?' requires list arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(a != b)
        return None

    def _op_list_prepend(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_PREPEND dest, src0, src1: r_dest = (list-prepend r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-prepend' requires list arguments, got {a.type_name()}")

        item = frame.locals[instr.src1]
        frame.locals[instr.dest] = MenaiList((cast(MenaiValue, item),) + a.elements)
        return None

    def _op_list_append(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_APPEND dest, src0, src1: r_dest = (list-append r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-append' requires list arguments, got {a.type_name()}")

        item = frame.locals[instr.src1]
        frame.locals[instr.dest] = MenaiList(a.elements + (cast(MenaiValue, item),))
        return None

    def _op_list_reverse(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_REVERSE dest, src0: r_dest = (list-reverse r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-reverse' requires list arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiList(tuple(reversed(a.elements)))
        return None

    def _op_list_first(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_FIRST dest, src0: r_dest = (list-first r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-first' requires list arguments, got {a.type_name()}")

        if not a.elements:
            raise MenaiEvalError("Function 'list-first' requires a non-empty list")

        frame.locals[instr.dest] = a.elements[0]
        return None

    def _op_list_rest(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_REST dest, src0: r_dest = (list-rest r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-rest' requires list arguments, got {a.type_name()}")

        if not a.elements:
            raise MenaiEvalError("Function 'list-rest' requires a non-empty list")

        frame.locals[instr.dest] = MenaiList(a.elements[1:])
        return None

    def _op_list_last(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_LAST dest, src0: r_dest = (list-last r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-last' requires list arguments, got {a.type_name()}")

        if not a.elements:
            raise MenaiEvalError("Function 'list-last' requires a non-empty list")

        frame.locals[instr.dest] = a.elements[-1]
        return None

    def _op_list_length(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_LENGTH dest, src0: r_dest = (list-length r_src0)"""
        value = frame.locals[instr.src0]
        if not isinstance(value, MenaiList):
            raise MenaiEvalError(
                f"Function 'list-length' requires list argument, got {value.type_name()}"
            )

        frame.locals[instr.dest] = MenaiInteger(len(value.elements))
        return None

    def _op_list_ref(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_REF dest, src0, src1: r_dest = (list-ref r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-ref' requires list arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(
                f"Function 'list-ref' requires integer index, got {b.type_name()}"
            )

        index = b.value
        if index < 0:
            raise MenaiEvalError(f"list-ref index out of range: {index}")

        try:
            frame.locals[instr.dest] = a.elements[index]

        except IndexError as e:
            raise MenaiEvalError(f"list-ref index out of range: {index}") from e

        return None

    def _op_list_null_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_NULL_P dest, src0: r_dest = (list-null? r_src0)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-null?' requires list arguments, got {a.type_name()}")

        frame.locals[instr.dest] = MenaiBoolean(len(a.elements) == 0)
        return None

    def _op_list_member_p(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_MEMBER_P dest, src0, src1: r_dest = (list-member? r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-member?' requires list arguments, got {a.type_name()}")

        item = cast(MenaiValue, frame.locals[instr.src1])
        frame.locals[instr.dest] = MenaiBoolean(item in a.elements)
        return None

    def _op_list_index(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_INDEX dest, src0, src1: r_dest = (list-index r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-index' requires list arguments, got {a.type_name()}")

        item = cast(MenaiValue, frame.locals[instr.src1])
        for i, elem in enumerate(a.elements):
            if elem == item:
                frame.locals[instr.dest] = MenaiInteger(i)
                return None

        frame.locals[instr.dest] = Menai_NONE
        return None

    def _op_list_slice(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_SLICE dest, src0, src1, src2: r_dest = (list-slice r_src0 r_src1 r_src2)"""
        a = frame.locals[instr.src0]

        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-slice' requires list arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'list-slice' requires integer arguments, got {b.type_name()}")

        c = frame.locals[instr.src2]
        if not isinstance(c, MenaiInteger):
            raise MenaiEvalError(f"Function 'list-slice' requires integer arguments, got {c.type_name()}")

        list_val = a
        start = b.value
        end = c.value
        n = len(list_val.elements)
        if start < 0:
            raise MenaiEvalError(f"list-slice start index cannot be negative: {start}")

        if end < 0:
            raise MenaiEvalError(f"list-slice end index cannot be negative: {end}")

        if start > n:
            raise MenaiEvalError(f"list-slice start index out of range: {start} (list length: {n})")

        if end > n:
            raise MenaiEvalError(f"list-slice end index out of range: {end} (list length: {n})")

        if start > end:
            raise MenaiEvalError(f"list-slice start index ({start}) cannot be greater than end index ({end})")

        frame.locals[instr.dest] = MenaiList(list_val.elements[start:end])
        return None

    def _op_list_remove(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_REMOVE dest, src0, src1: r_dest = (list-remove r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-remove' requires list arguments, got {a.type_name()}")

        item = cast(MenaiValue, frame.locals[instr.src1])
        new_elements = tuple(elem for elem in a.elements if elem != item)
        frame.locals[instr.dest] = MenaiList(new_elements)
        return None

    def _op_list_concat(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_CONCAT dest, src0, src1: r_dest = (list-concat r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list-concat' requires list arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiList):
            raise MenaiEvalError(f"Function 'list-concat' requires list arguments, got {b.type_name()}")

        frame.locals[instr.dest] = MenaiList(a.elements + b.elements)
        return None

    def _op_list_to_string(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """LIST_TO_STRING dest, src0, src1: r_dest = (list->string r_src0 r_src1)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiList):
            raise MenaiEvalError(f"Function 'list->string' requires list arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiString):
            raise MenaiEvalError(f"Function 'list->string' requires string arguments, got {b.type_name()}")

        parts = []
        for item in a.elements:
            if not isinstance(item, MenaiString):
                raise MenaiEvalError(f"list->string requires list of strings, found {item.type_name()}")

            parts.append(item.value)

        frame.locals[instr.dest] = MenaiString(b.value.join(parts))
        return None

    def _op_range(  # pylint: disable=useless-return
        self, frame: Frame, instr: Instruction
    ) -> MenaiValue | None:
        """RANGE dest, src0, src1, src2: r_dest = (range r_src0 r_src1 r_src2)"""
        a = frame.locals[instr.src0]
        if not isinstance(a, MenaiInteger):
            raise MenaiEvalError(f"Function 'range' requires integer arguments, got {a.type_name()}")

        b = frame.locals[instr.src1]
        if not isinstance(b, MenaiInteger):
            raise MenaiEvalError(f"Function 'range' requires integer arguments, got {b.type_name()}")

        c = frame.locals[instr.src2]
        if not isinstance(c, MenaiInteger):
            raise MenaiEvalError(f"Function 'range' requires integer arguments, got {c.type_name()}")

        start = a.value
        end = b.value
        step = c.value
        if step == 0:
            raise MenaiEvalError("Range step cannot be zero")

        frame.locals[instr.dest] = MenaiList(tuple(MenaiInteger(v) for v in range(start, end, step)))
        return None
