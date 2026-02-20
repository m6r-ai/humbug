"""AIFPL Virtual Machine - executes bytecode."""

import cmath
import difflib
from dataclasses import dataclass, field
import math
from typing import List, Dict, Any, cast, Optional, Protocol

from aifpl.aifpl_builtin_registry import AIFPLBuiltinRegistry
from aifpl.aifpl_bytecode import CodeObject, Opcode
from aifpl.aifpl_bytecode_validator import validate_bytecode
from aifpl.aifpl_error import AIFPLEvalError, AIFPLCancelledException
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLBoolean, AIFPLString, AIFPLList, AIFPLAList, AIFPLFunction,
    AIFPLInteger, AIFPLComplex, AIFPLFloat
)


class AIFPLTraceWatcher(Protocol):
    """Protocol for AIFPL trace watchers."""
    def on_trace(self, message: str) -> None:
        """
        Called when a trace message is emitted.

        Args:
            message: The trace message as a string (AIFPL formatted)
        """


@dataclass
class TailCall:
    """
    Marker for tail call optimization.

    When a handler returns this, the execution loop will replace the current
    frame with a new frame for the target function, achieving true tail call
    optimization with constant stack space.
    """
    func: AIFPLFunction


@dataclass
class Frame:
    """
    Execution frame for function calls.

    Each frame has its own locals and instruction pointer.
    """
    code: CodeObject
    ip: int = 0  # Instruction pointer
    locals: List[AIFPLValue | None] = field(init=False)  # Local variables
    parent_frame: 'Frame | None' = None  # Parent frame for LOAD_PARENT_VAR (lexical parent)

    def __post_init__(self) -> None:
        """Initialize locals array based on code object."""
        self.locals = [None] * self.code.local_count


class AIFPLVM:
    """
    Virtual machine for executing AIFPL bytecode.

    Uses a stack-based architecture with lexically-scoped frames.
    """

    def __init__(self, validate: bool = True) -> None:
        self.stack: List[AIFPLValue] = []
        self.frames: List[Frame] = []
        self.globals: Dict[str, AIFPLValue] = {}
        self.validate_bytecode = validate  # Whether to validate bytecode before execution

        # Trace watcher for debugging support
        self.trace_watcher: Optional[AIFPLTraceWatcher] = None

        # Cancellation support for non-blocking execution
        self._cancelled: bool = False
        self._instruction_count: int = 0

        # Check cancellation every N instructions (balance between responsiveness and performance)
        self._cancellation_check_interval: int = 1000

        # Create builtin registry to build first-class function objects
        builtin_registry = AIFPLBuiltinRegistry()

        # Create builtin function objects for first-class function support (e.g., passed to map)
        self._builtin_functions = builtin_registry.create_builtin_function_objects()

        # Build dispatch table for fast opcode execution
        # This is a critical optimization: jump table dispatch is 2-3x faster than if/elif chains
        self._dispatch_table = self._build_dispatch_table()

    def set_trace_watcher(self, watcher: Optional[AIFPLTraceWatcher]) -> None:
        """
        Set the trace watcher (replaces any existing watcher).

        Args:
            watcher: AIFPLTraceWatcher instance or None to disable tracing
        """
        self.trace_watcher = watcher

    def _emit_trace(self, message: AIFPLValue) -> None:
        """
        Emit a trace event to the watcher.

        Args:
            message: The AIFPL value to trace
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

    def reset_cancellation(self) -> None:
        """
        Reset the cancellation flag.

        This should be called before starting a new execution to ensure
        the cancellation state from a previous execution doesn't affect
        the new one.
        """
        self._cancelled = False
        self._instruction_count = 0

    def _check_cancellation(self) -> None:
        """
        Check if execution has been cancelled and raise exception if so.

        This is called periodically during execution (every N instructions)
        to allow long-running computations to be interrupted.

        Raises:
            AIFPLCancelledException: If execution has been cancelled
        """
        if self._cancelled:
            raise AIFPLCancelledException()

    def _build_dispatch_table(self) -> List[Any]:
        """
        Build jump table for opcode dispatch.

        This replaces the if/elif chain with direct array indexing,
        significantly improving performance in the hot execution loop.
        """
        table: List[Any] = [None] * 256
        table[Opcode.LOAD_CONST] = self._op_load_const
        table[Opcode.LOAD_TRUE] = self._op_load_true
        table[Opcode.LOAD_FALSE] = self._op_load_false
        table[Opcode.LOAD_EMPTY_LIST] = self._op_load_empty_list
        table[Opcode.LOAD_VAR] = self._op_load_var
        table[Opcode.STORE_VAR] = self._op_store_var
        table[Opcode.LOAD_NAME] = self._op_load_name
        table[Opcode.LOAD_PARENT_VAR] = self._op_load_parent_var
        table[Opcode.JUMP] = self._op_jump
        table[Opcode.JUMP_IF_FALSE] = self._op_jump_if_false
        table[Opcode.JUMP_IF_TRUE] = self._op_jump_if_true
        table[Opcode.RAISE_ERROR] = self._op_raise_error
        table[Opcode.MAKE_CLOSURE] = self._op_make_closure
        table[Opcode.CALL_FUNCTION] = self._op_call_function
        table[Opcode.TAIL_CALL_FUNCTION] = self._op_tail_call_function
        table[Opcode.RETURN] = self._op_return
        table[Opcode.EMIT_TRACE] = self._op_emit_trace
        table[Opcode.ADD] = self._op_add
        table[Opcode.SUB] = self._op_sub
        table[Opcode.MUL] = self._op_mul
        table[Opcode.DIV] = self._op_div
        table[Opcode.NUMBER_P] = self._op_number_p
        table[Opcode.INTEGER_P] = self._op_integer_p
        table[Opcode.FLOAT_P] = self._op_float_p
        table[Opcode.COMPLEX_P] = self._op_complex_p
        table[Opcode.STRING_P] = self._op_string_p
        table[Opcode.BOOLEAN_P] = self._op_boolean_p
        table[Opcode.LIST_P] = self._op_list_p
        table[Opcode.ALIST_P] = self._op_alist_p
        table[Opcode.FUNCTION_P] = self._op_function_p
        table[Opcode.NOT] = self._op_not
        table[Opcode.SIN] = self._op_sin
        table[Opcode.COS] = self._op_cos
        table[Opcode.TAN] = self._op_tan
        table[Opcode.LOG] = self._op_log
        table[Opcode.LOG10] = self._op_log10
        table[Opcode.EXP] = self._op_exp
        table[Opcode.POW] = self._op_pow
        table[Opcode.SQRT] = self._op_sqrt
        table[Opcode.ABS] = self._op_abs
        table[Opcode.CEIL] = self._op_ceil
        table[Opcode.FLOOR] = self._op_floor
        table[Opcode.CONS] = self._op_cons
        table[Opcode.LENGTH] = self._op_length
        table[Opcode.REVERSE] = self._op_reverse
        table[Opcode.FIRST] = self._op_first
        table[Opcode.REST] = self._op_rest
        table[Opcode.LAST] = self._op_last
        table[Opcode.LIST_REF] = self._op_list_ref
        # Arithmetic
        table[Opcode.FLOOR_DIV] = self._op_floor_div
        table[Opcode.MOD] = self._op_mod
        table[Opcode.STAR_STAR] = self._op_star_star
        # Bitwise
        table[Opcode.BIT_NOT] = self._op_bit_not
        table[Opcode.BIT_SHIFT_LEFT] = self._op_bit_shift_left
        table[Opcode.BIT_SHIFT_RIGHT] = self._op_bit_shift_right
        # Numeric conversion
        table[Opcode.ROUND] = self._op_round
        table[Opcode.TO_INTEGER] = self._op_to_integer
        table[Opcode.TO_FLOAT] = self._op_to_float
        table[Opcode.REAL] = self._op_real
        table[Opcode.IMAG] = self._op_imag
        table[Opcode.MAKE_COMPLEX] = self._op_make_complex
        table[Opcode.BIN] = self._op_bin
        table[Opcode.HEX] = self._op_hex
        table[Opcode.OCT] = self._op_oct
        # List
        table[Opcode.NULL_P] = self._op_null_p
        table[Opcode.MEMBER_P] = self._op_member_p
        table[Opcode.POSITION] = self._op_position
        table[Opcode.TAKE] = self._op_take
        table[Opcode.DROP] = self._op_drop
        table[Opcode.REMOVE] = self._op_remove
        # String
        table[Opcode.STRING_LENGTH] = self._op_string_length
        table[Opcode.STRING_UPCASE] = self._op_string_upcase
        table[Opcode.STRING_DOWNCASE] = self._op_string_downcase
        table[Opcode.STRING_TRIM] = self._op_string_trim
        table[Opcode.STRING_TO_NUMBER] = self._op_string_to_number
        table[Opcode.NUMBER_TO_STRING] = self._op_number_to_string
        table[Opcode.STRING_TO_LIST] = self._op_string_to_list
        table[Opcode.LIST_TO_STRING] = self._op_list_to_string
        table[Opcode.STRING_REF] = self._op_string_ref
        table[Opcode.STRING_CONTAINS_P] = self._op_string_contains_p
        table[Opcode.STRING_PREFIX_P] = self._op_string_prefix_p
        table[Opcode.STRING_SUFFIX_P] = self._op_string_suffix_p
        table[Opcode.STRING_SPLIT] = self._op_string_split
        table[Opcode.STRING_JOIN] = self._op_string_join
        table[Opcode.SUBSTRING] = self._op_substring
        table[Opcode.STRING_REPLACE] = self._op_string_replace
        # Alist
        table[Opcode.ALIST_KEYS] = self._op_alist_keys
        table[Opcode.ALIST_VALUES] = self._op_alist_values
        table[Opcode.ALIST_LENGTH] = self._op_alist_length
        table[Opcode.ALIST_HAS_P] = self._op_alist_has_p
        table[Opcode.ALIST_REMOVE] = self._op_alist_remove
        table[Opcode.ALIST_MERGE] = self._op_alist_merge
        table[Opcode.ALIST_SET] = self._op_alist_set
        table[Opcode.ALIST_GET] = self._op_alist_get
        table[Opcode.RANGE] = self._op_range
        # Fold-reducible variadic ops (binary forms)
        table[Opcode.BIT_OR] = self._op_bit_or
        table[Opcode.BIT_AND] = self._op_bit_and
        table[Opcode.BIT_XOR] = self._op_bit_xor
        table[Opcode.APPEND] = self._op_append
        table[Opcode.STRING_APPEND] = self._op_string_append
        table[Opcode.MIN] = self._op_min
        table[Opcode.MAX] = self._op_max
        # Comparison / equality ops (binary forms)
        table[Opcode.EQ] = self._op_eq
        table[Opcode.NEQ] = self._op_neq
        table[Opcode.LT] = self._op_lt
        table[Opcode.GT] = self._op_gt
        table[Opcode.LTE] = self._op_lte
        table[Opcode.GTE] = self._op_gte
        table[Opcode.STRING_EQ_P] = self._op_string_eq_p
        table[Opcode.NUMBER_EQ_P] = self._op_number_eq_p
        table[Opcode.INTEGER_EQ_P] = self._op_integer_eq_p
        table[Opcode.FLOAT_EQ_P] = self._op_float_eq_p
        table[Opcode.COMPLEX_EQ_P] = self._op_complex_eq_p
        table[Opcode.BOOLEAN_EQ_P] = self._op_boolean_eq_p
        table[Opcode.LIST_EQ_P] = self._op_list_eq_p
        table[Opcode.ALIST_EQ_P] = self._op_alist_eq_p
        table[Opcode.BUILD_LIST] = self._op_build_list
        table[Opcode.BUILD_ALIST] = self._op_build_alist
        return table

    def execute(
        self,
        code: CodeObject,
        constants: Dict[str, AIFPLValue],
        prelude_functions: Dict[str, AIFPLFunction] | None = None
    ) -> AIFPLValue:
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
        self.globals.update(self._builtin_functions)
        if prelude_functions:
            self.globals.update(prelude_functions)

        # Reset state
        self.reset_cancellation()

        # Reset execution state
        self.stack = []
        self.frames = [Frame(code)]

        # Execute until we return
        return self._execute_frame()

    def _setup_call_frame(self, func: AIFPLFunction) -> None:
        """
        Setup a new frame for calling a function.

        Creates a new frame, initializes it with the function's closure environment
        and captured values, and pushes it onto the frame stack.

        Arguments are assumed to be already on the stack in correct order.
        The function prologue (STORE_VAR instructions at start of bytecode)
        will pop these arguments into locals.

        Args:
            func: Function to call
        """
        code = func.bytecode

        # Create new frame
        new_frame = Frame(code)
        new_frame.parent_frame = func.parent_frame  # Set parent frame for LOAD_PARENT_VAR

        # Store captured values in locals (after parameters)
        if func.captured_values:
            for i, captured_val in enumerate(func.captured_values):
                new_frame.locals[code.param_count + i] = captured_val

        # Push frame onto stack
        self.frames.append(new_frame)

    def _execute_frame(self) -> AIFPLValue:
        """
        Execute frames using jump table dispatch with tail call optimization.

        This method implements a trampoline pattern: when a handler returns a
        TailCall marker, we replace the current frame with the target frame
        and continue execution, achieving true tail call optimization with
        constant stack space.

        Returns:
            Result value when frame returns
        """
        frame = self.frames[-1]

        # Cache dispatch table in local variable for faster access
        dispatch = self._dispatch_table

        # Cache cancellation check interval for performance
        check_interval = self._cancellation_check_interval

        # Local instruction counter for cancellation checking
        # Using a local variable is faster than accessing self._instruction_count
        instruction_count = 0

        while True:
            # Re-fetch code and instructions each iteration in case frame.code changes (mutual recursion TCO)
            code = frame.code
            instructions = code.instructions
            if frame.ip >= len(instructions):
                break

            # Periodically check for cancellation
            # This adds minimal overhead while allowing timely cancellation
            instruction_count += 1
            if instruction_count >= check_interval:
                self._check_cancellation()
                instruction_count = 0

            instr = instructions[frame.ip]
            opcode = instr.opcode

            # Increment IP before executing (so jumps can override)
            frame.ip += 1

            # Jump table dispatch - this is the key optimization!
            # Direct array indexing is much faster than if/elif chain
            handler = dispatch[opcode]
            if handler is None:
                raise AIFPLEvalError(f"Unimplemented opcode: {opcode}")

            # Call the handler
            result = handler(frame, code, instr.arg1, instr.arg2)
            if result is None:
                # Fast path: continue execution
                continue

            # Check if it's a tail call
            if isinstance(result, TailCall):
                # Optimization: reuse frame for self-recursion
                if result.func.bytecode == frame.code:
                    frame.ip = 0
                    continue

                # Replace frame for general tail call
                self.frames.pop()
                self._setup_call_frame(result.func)
                frame = self.frames[-1]  # Update frame reference
                continue

            # Otherwise it's a return value (from RETURN opcode)
            return result

        # Frame finished without explicit return
        raise AIFPLEvalError("Frame execution ended without RETURN instruction")

    def _op_load_const(  # pylint: disable=useless-return
        self, _frame: Frame, code: CodeObject, arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_CONST: Push constant from pool onto stack."""
        # Validator guarantees arg1 is in bounds
        # No bounds check needed - direct access for maximum performance
        self.stack.append(code.constants[arg1])
        return None

    def _op_load_true(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_TRUE: Push boolean true onto stack."""
        self.stack.append(AIFPLBoolean(True))
        return None

    def _op_load_false(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_FALSE: Push boolean false onto stack."""
        self.stack.append(AIFPLBoolean(False))
        return None

    def _op_load_empty_list(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_EMPTY_LIST: Push empty list onto stack."""
        self.stack.append(AIFPLList(()))
        return None

    def _op_load_var(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, index: int, _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_VAR: Load variable from current frame at index."""
        # Validator guarantees index is in bounds AND variable is initialized
        current_frame = self.frames[-1]
        value = current_frame.locals[index]
        self.stack.append(cast(AIFPLValue, value))
        return None

    def _op_store_var(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, index: int, _arg2: int
    ) -> AIFPLValue | None:
        """STORE_VAR: Store top of stack to variable in current frame at index."""
        # Validator guarantees index is in bounds and stack has value
        value = self.stack.pop()
        self.frames[-1].locals[index] = value
        return None

    def _op_load_parent_var(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, index: int, depth: int
    ) -> AIFPLValue | None:
        """
        LOAD_PARENT_VAR: Load variable from parent frame.

        This is used for recursive closures in letrec - the closure references
        a binding from its parent frame rather than capturing it.

        Args:
            index - variable index in the target parent frame
            depth - how many parent frames to walk up
        """
        # Validator guarantees depth >= 1
        # Walk up parent frame chain by depth
        current_frame = self.frames[-1]
        parent_frame = current_frame.parent_frame

        # Walk up the chain (validator guarantees this won't be None)
        for _ in range(depth - 1):
            assert parent_frame is not None  # Validator guarantees
            parent_frame = parent_frame.parent_frame

        assert parent_frame is not None  # Validator guarantees

        # Validator guarantees index is in bounds AND variable is initialized
        value = parent_frame.locals[index]
        self.stack.append(cast(AIFPLValue, value))
        return None

    def _op_load_name(  # pylint: disable=useless-return
        self, _frame: Frame, code: CodeObject, arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_NAME: Load global variable by name."""
        name = code.names[arg1]

        # Load from globals (LOAD_PARENT_VAR handles parent scope access)
        if name in self.globals:
            self.stack.append(self.globals[name])
            return None

        # Not found - generate helpful error
        available_vars = list(self.globals.keys())
        similar = difflib.get_close_matches(name, available_vars, n=3, cutoff=0.6)

        suggestion_text = (
            f"Did you mean: {', '.join(similar)}?" if similar
            else "Check spelling or define it in a let binding"
        )

        raise AIFPLEvalError(
            message=f"Undefined variable: '{name}'",
            context=(
                f"Available variables: "
                f"{', '.join(sorted(available_vars)[:10])}{'...' if len(available_vars) > 10 else ''}"
            ),
            suggestion=suggestion_text,
            example=f"(let (({name} some-value)) ...)"
        )

    def _op_jump(  # pylint: disable=useless-return
        self, frame: Frame, _code: CodeObject, target: int, _arg2: int
    ) -> AIFPLValue | None:
        """JUMP: Unconditional jump to instruction."""
        frame.ip = target
        return None

    def _op_jump_if_false(  # pylint: disable=useless-return
        self, frame: Frame, _code: CodeObject, target: int, _arg2: int
    ) -> AIFPLValue | None:
        """JUMP_IF_FALSE: Pop stack, jump if false."""
        # Validator guarantees target is valid and stack has value
        # Must keep type check (runtime-dependent)
        condition = self.stack.pop()
        if not isinstance(condition, AIFPLBoolean):
            raise AIFPLEvalError("If condition must be boolean")

        if not condition.value:
            frame.ip = target

        return None

    def _op_jump_if_true(  # pylint: disable=useless-return
        self, frame: Frame, _code: CodeObject, target: int, _arg2: int
    ) -> AIFPLValue | None:
        """JUMP_IF_TRUE: Pop stack, jump if true."""
        # Validator guarantees target is valid and stack has value
        # Must keep type check (runtime-dependent)
        condition = self.stack.pop()
        if not isinstance(condition, AIFPLBoolean):
            raise AIFPLEvalError("If condition must be boolean")

        if condition.value:
            frame.ip = target

        return None

    def _op_raise_error(  # pylint: disable=useless-return
        self, _frame: Frame, code: CodeObject, arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """RAISE_ERROR: Raise error with message from constant pool."""
        # Validator guarantees arg1 is in bounds
        # Type check could be removed if we validate constant types, but keep for now
        error_msg = code.constants[arg1]
        if not isinstance(error_msg, AIFPLString):
            raise AIFPLEvalError("RAISE_ERROR requires a string constant")

        raise AIFPLEvalError(error_msg.value)

    def _op_make_closure(  # pylint: disable=useless-return
        self, _frame: Frame, code: CodeObject, arg1: int, capture_count: int
    ) -> AIFPLValue | None:
        """MAKE_CLOSURE: Create closure from code object and captured values."""
        # Validator guarantees arg1 is in bounds and stack has enough values
        # Direct access without bounds checking
        closure_code = code.code_objects[arg1]

        # Pop captured values from stack (in reverse order)
        captured_values = []
        for _ in range(capture_count):
            captured_values.append(self.stack.pop())

        captured_values.reverse()

        # Create closure with captured values and parent frame reference
        # Parent frame is used by LOAD_PARENT_VAR for recursive bindings
        current_frame = self.frames[-1] if self.frames else None

        closure = AIFPLFunction(
            parameters=tuple(f"param{i}" for i in range(closure_code.param_count)),
            name=closure_code.name,
            bytecode=closure_code,
            captured_values=tuple(captured_values),
            is_variadic=closure_code.is_variadic,
            parent_frame=current_frame  # Store parent frame for LOAD_PARENT_VAR
        )
        self.stack.append(closure)
        return None

    def _op_call_function(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, arity: int, _arg2: int
    ) -> AIFPLValue | None:
        """CALL_FUNCTION: Call function with arguments from stack."""
        # Validator guarantees stack has enough values (arity + 1)

        # Get function from under the arguments
        # Must keep type check (runtime-dependent - could be any value)
        func = self.stack[-(arity + 1)]

        if not isinstance(func, AIFPLFunction):
            raise AIFPLEvalError(
                message="Cannot call non-function value",
                received=f"Attempted to call: {func.describe()} ({func.type_name()})",
                expected="Function (lambda or builtin)",
                suggestion="Only functions can be called"
            )

        # Handle builtin functions
        if func.is_native:
            args = [self.stack.pop() for _ in range(arity)]
            args.reverse()
            self.stack.pop()  # Pop function
            assert func.native_impl is not None, f"Function {func.name} has no native implementation"
            result = func.native_impl(args)
            self.stack.append(result)
            return None

        # Check arity for bytecode functions
        # Must keep: arity check (runtime-dependent - depends on what function is called)
        expected_arity = func.bytecode.param_count
        if func.bytecode.is_variadic:
            # Variadic: must have at least (param_count - 1) fixed args.
            # The last local receives all remaining args packed into a list.
            min_arity = expected_arity - 1
            if arity < min_arity:
                func_name = func.name or "<lambda>"
                raise AIFPLEvalError(
                    message=f"Function '{func_name}' expects at least {min_arity} arguments, got {arity}",
                    suggestion=f"Provide at least {min_arity} argument{'s' if min_arity != 1 else ''}"
                )
            # Pack excess args into a list and replace them on the stack with the list.
            # Stack currently has: [fixed_args..., rest_args...]  (arity values total)
            rest_count = arity - min_arity
            if rest_count == 0:
                self.stack.append(AIFPLList(()))
            else:
                rest_elements = tuple(self.stack[-rest_count:])
                del self.stack[-rest_count:]
                self.stack.append(AIFPLList(rest_elements))
        elif arity != expected_arity:
            func_name = func.name or "<lambda>"
            raise AIFPLEvalError(
                message=f"Function '{func_name}' expects {expected_arity} arguments, got {arity}",
                suggestion=f"Provide exactly {expected_arity} argument{'s' if expected_arity != 1 else ''}"
            )

        # Remove function from stack
        # After variadic packing the stack has param_count args; for fixed-arity
        # it still has arity (== param_count) args.  Either way the function sits
        # immediately below param_count values.
        del self.stack[-(expected_arity + 1)]

        self._setup_call_frame(func)

        result = self._execute_frame()
        self.stack.append(result)
        return None

    def _op_tail_call_function(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, arity: int, _arg2: int
    ) -> TailCall | None:
        """
        TAIL_CALL_FUNCTION: Perform tail call with optimization.

        Returns a TailCall marker that the execution loop will handle by
        replacing the current frame with the target frame, achieving true
        tail call optimization with constant stack space for all tail calls.
        """
        # Validator guarantees stack has enough values (arity + 1)

        # Get function from stack (below arguments)
        # Must keep type check (runtime-dependent)
        func = self.stack[-(arity + 1)]

        if not isinstance(func, AIFPLFunction):
            raise AIFPLEvalError(
                message="Cannot call non-function value",
                received=f"Attempted to call: {func.describe()} ({func.type_name()})",
                expected="Function (lambda or builtin)",
                suggestion="Only functions can be called"
            )

        # Handle native functions (builtins/wrappers)
        # Native functions can't be tail-call optimized (no bytecode frames),
        # but we can call them directly without building up frames
        if func.is_native:
            # Pop arguments from stack
            args = [self.stack.pop() for _ in range(arity)]
            args.reverse()
            self.stack.pop()  # Pop function

            # Call native implementation directly
            assert func.native_impl is not None, f"Function {func.name} has no native implementation"
            result = func.native_impl(args)

            # Pop the current frame and return result
            self.frames.pop()
            return result

        # Check arity for bytecode functions
        # Must keep: arity check (runtime-dependent)
        expected_arity = func.bytecode.param_count
        if func.bytecode.is_variadic:
            # Variadic: must have at least (param_count - 1) fixed args.
            # The last local receives all remaining args packed into a list.
            min_arity = expected_arity - 1
            if arity < min_arity:
                func_name = func.name or "<lambda>"
                raise AIFPLEvalError(
                    message=f"Function '{func_name}' expects at least {min_arity} arguments, got {arity}",
                    suggestion=f"Provide at least {min_arity} argument{'s' if min_arity != 1 else ''}"
                )
            # Pack excess args into a list and replace them on the stack with the list.
            rest_count = arity - min_arity
            if rest_count == 0:
                self.stack.append(AIFPLList(()))
            else:
                rest_elements = tuple(self.stack[-rest_count:])
                del self.stack[-rest_count:]
                self.stack.append(AIFPLList(rest_elements))
        elif arity != expected_arity:
            func_name = func.name or "<lambda>"
            raise AIFPLEvalError(
                message=f"Function '{func_name}' expects {expected_arity} arguments, got {arity}",
                suggestion=f"Provide exactly {expected_arity} argument{'s' if expected_arity != 1 else ''}"
            )

        # Remove function from stack (leave param_count args for new frame)
        del self.stack[-(expected_arity + 1)]

        # Return TailCall marker - execution loop will handle frame replacement
        return TailCall(func)

    def _op_return(
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """RETURN: Pop frame and return value from stack."""
        # Validator guarantees stack has a value to return
        self.frames.pop()
        return self.stack.pop()

    def _op_emit_trace(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """EMIT_TRACE: Pop value from stack and emit to trace watcher."""
        # Pop the message from stack
        message = self.stack.pop()

        # Emit trace if watcher is available
        if self.trace_watcher:
            self._emit_trace(message)

        # Continue execution (no return value)
        return None

    def _ensure_boolean(self, value: AIFPLValue, operation_name: str) -> bool:
        """
        Ensure value is a boolean, raise user-friendly error if not.

        Args:
            value: Value to check
            operation_name: Name of operation for error message (e.g., 'not', 'if')

        Returns:
            Python boolean value

        Raises:
            AIFPLEvalError: If value is not a boolean
        """
        if not isinstance(value, AIFPLBoolean):
            raise AIFPLEvalError(
                f"Function '{operation_name}' requires boolean arguments, got {value.type_name()}"
            )

        return value.value

    def _ensure_number(self, value: AIFPLValue, operation_name: str) -> int | float | complex:
        """
        Ensure value is a number, raise user-friendly error if not.

        Args:
            value: Value to check
            operation_name: Name of operation for error message (e.g., '+', '-')

        Returns:
            Python numeric value (int, float, or complex)

        Raises:
            AIFPLEvalError: If value is not a number
        """
        if not isinstance(value, (AIFPLInteger, AIFPLFloat, AIFPLComplex)):
            raise AIFPLEvalError(
                f"Function '{operation_name}' requires number arguments, got {value.type_name()}"
            )

        return value.value

    def _ensure_real_number(self, value: AIFPLValue, operation_name: str) -> int | float:
        """
        Ensure value is a real number, raise user-friendly error if not.

        Args:
            value: Value to check
            operation_name: Name of operation for error message (e.g., '+', '-')

        Returns:
            Python numeric value (int or float)

        Raises:
            AIFPLEvalError: If value is not a real number
        """
        if not isinstance(value, (AIFPLInteger, AIFPLFloat)):
            raise AIFPLEvalError(
                f"Function '{operation_name}' requires real number arguments, got {value.type_name()}"
            )

        return value.value

    def _ensure_list(self, value: AIFPLValue, function_name: str) -> AIFPLList:
        """Ensure value is a list, raise error if not."""
        if not isinstance(value, AIFPLList):
            raise AIFPLEvalError(f"Function '{function_name}' requires list arguments, got {value.type_name()}")

        return value

    def _ensure_string(self, value: AIFPLValue, function_name: str) -> str:
        """Ensure value is a string, raise error if not."""
        if not isinstance(value, AIFPLString):
            raise AIFPLEvalError(f"Function '{function_name}' requires string arguments, got {value.type_name()}")

        return value.value

    def _ensure_integer(self, value: AIFPLValue, function_name: str) -> int:
        """Ensure value is an integer, raise error if not."""
        if not isinstance(value, AIFPLInteger):
            raise AIFPLEvalError(f"Function '{function_name}' requires integer arguments, got {value.type_name()}")

        return value.value

    def _ensure_alist(self, value: AIFPLValue, function_name: str) -> AIFPLAList:
        """Ensure value is an alist, raise error if not."""
        if not isinstance(value, AIFPLAList):
            raise AIFPLEvalError(f"Function '{function_name}' requires alist arguments, got {value.type_name()}")

        return value

    def _wrap_numeric_result(self, result: int | float | complex) -> AIFPLValue:
        """Wrap Python numeric value in appropriate AIFPL type."""
        if isinstance(result, int):
            return AIFPLInteger(result)

        if isinstance(result, float):
            return AIFPLFloat(result)

        return AIFPLComplex(result)

    def _op_add(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ADD: Pop two values, add them, push result."""
        # Pop operands (b first, then a, so we compute a + b)
        b = self.stack.pop()
        a = self.stack.pop()

        a_val = self._ensure_number(a, '+')
        b_val = self._ensure_number(b, '+')
        result = a_val + b_val
        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_sub(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """SUB: Pop two values, subtract them, push result."""
        b = self.stack.pop()
        a = self.stack.pop()

        a_val = self._ensure_number(a, '-')
        b_val = self._ensure_number(b, '-')
        result = a_val - b_val
        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_mul(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """MUL: Pop two values, multiply them, push result."""
        b = self.stack.pop()
        a = self.stack.pop()

        a_val = self._ensure_number(a, '*')
        b_val = self._ensure_number(b, '*')
        result = a_val * b_val
        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_div(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """DIV: Pop two values, divide them, push result."""
        b = self.stack.pop()
        a = self.stack.pop()

        a_val = self._ensure_number(a, '/')
        b_val = self._ensure_number(b, '/')

        # Check for division by zero
        if b_val == 0:
            raise AIFPLEvalError("Division by zero")

        result = a_val / b_val
        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_number_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """NUMBER_P: Check if value is a number (integer, float, or complex)."""
        value = self.stack.pop()
        result = isinstance(value, (AIFPLInteger, AIFPLFloat, AIFPLComplex))
        self.stack.append(AIFPLBoolean(result))
        return None

    def _op_integer_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """INTEGER_P: Check if value is an integer."""
        value = self.stack.pop()
        self.stack.append(AIFPLBoolean(isinstance(value, AIFPLInteger)))
        return None

    def _op_float_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """FLOAT_P: Check if value is a float."""
        value = self.stack.pop()
        self.stack.append(AIFPLBoolean(isinstance(value, AIFPLFloat)))
        return None

    def _op_complex_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """COMPLEX_P: Check if value is a complex number."""
        value = self.stack.pop()
        self.stack.append(AIFPLBoolean(isinstance(value, AIFPLComplex)))
        return None

    def _op_string_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_P: Check if value is a string."""
        value = self.stack.pop()
        self.stack.append(AIFPLBoolean(isinstance(value, AIFPLString)))
        return None

    def _op_boolean_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """BOOLEAN_P: Check if value is a boolean."""
        value = self.stack.pop()
        self.stack.append(AIFPLBoolean(isinstance(value, AIFPLBoolean)))
        return None

    def _op_list_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LIST_P: Check if value is a list."""
        value = self.stack.pop()
        self.stack.append(AIFPLBoolean(isinstance(value, AIFPLList)))
        return None

    def _op_alist_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ALIST_P: Check if value is an alist."""
        value = self.stack.pop()
        self.stack.append(AIFPLBoolean(isinstance(value, AIFPLAList)))
        return None

    def _op_function_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """FUNCTION_P: Check if value is a function."""
        value = self.stack.pop()
        self.stack.append(AIFPLBoolean(isinstance(value, AIFPLFunction)))
        return None

    def _op_not(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """NOT: Logical NOT operation."""
        value = self.stack.pop()
        bool_val = self._ensure_boolean(value, "not")
        self.stack.append(AIFPLBoolean(not bool_val))
        return None

    def _op_sin(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """SIN: Compute sine of a number."""
        arg = self.stack.pop()
        arg_val = self._ensure_number(arg, 'sin')

        if isinstance(arg_val, complex):
            result = cmath.sin(arg_val)

        else:
            result = math.sin(arg_val)

        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_cos(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """COS: Compute cosine of a number."""
        arg = self.stack.pop()
        arg_val = self._ensure_number(arg, 'cos')

        if isinstance(arg_val, complex):
            result = cmath.cos(arg_val)

        else:
            result = math.cos(arg_val)

        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_tan(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """TAN: Compute tangent of a number."""
        arg = self.stack.pop()
        arg_val = self._ensure_number(arg, 'tan')

        if isinstance(arg_val, complex):
            result = cmath.tan(arg_val)

        else:
            result = math.tan(arg_val)

        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_log(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LOG: Compute natural logarithm of a number."""
        arg = self.stack.pop()
        arg_val = self._ensure_number(arg, 'log')

        if isinstance(arg_val, complex):
            result = cmath.log(arg_val)

        else:
            # Handle log(0) = -inf
            if isinstance(arg_val, (int, float)) and arg_val == 0:
                result = float('-inf')

            else:
                result = math.log(arg_val)

        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_log10(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LOG10: Compute base-10 logarithm of a number."""
        arg = self.stack.pop()
        arg_val = self._ensure_number(arg, 'log10')

        if isinstance(arg_val, complex):
            result = cmath.log10(arg_val)

        else:
            # Handle log(0) = -inf
            if isinstance(arg_val, (int, float)) and arg_val == 0:
                result = float('-inf')

            else:
                result = math.log10(arg_val)

        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_exp(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """EXP: Compute exponential of a number."""
        arg = self.stack.pop()
        arg_val = self._ensure_number(arg, 'exp')

        if isinstance(arg_val, complex):
            result = cmath.exp(arg_val)

        else:
            result = math.exp(arg_val)

        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_sqrt(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """SQRT: Compute square root of a number."""
        arg = self.stack.pop()
        arg_val = self._ensure_number(arg, 'sqrt')

        if isinstance(arg_val, complex):
            result = cmath.sqrt(arg_val)

        else:
            if arg_val < 0:
                raise AIFPLEvalError("Function 'sqrt' requires a non-negative argument")

            result = math.sqrt(float(arg_val))

        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_abs(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ABS: Get absolute value of a real number."""
        arg = self.stack.pop()
        arg_val = self._ensure_number(arg, 'abs')
        result = abs(arg_val)
        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_ceil(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """CEIL: Get the ceiling of a real number."""
        arg = self.stack.pop()
        arg_val = self._ensure_real_number(arg, 'ceil')
        result = math.ceil(arg_val)
        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_floor(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """FLOOR: Get the floor of a real number."""
        arg = self.stack.pop()
        arg_val = self._ensure_real_number(arg, 'floor')
        result = math.floor(arg_val)
        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_pow(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """POW: Calculate a ** b."""
        # Pop operands (b first, then a, so we compute a ** b)
        b = self.stack.pop()
        a = self.stack.pop()

        a_val = self._ensure_real_number(a, 'pow')
        b_val = self._ensure_real_number(b, 'pow')
        result = a_val ** b_val
        self.stack.append(self._wrap_numeric_result(result))
        return None

    def _op_cons(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """CONS: Pop two values, construct a new list with head and tail."""
        tail = self.stack.pop()
        head = self.stack.pop()

        list_val = self._ensure_list(tail, 'cons')
        self.stack.append(list_val.cons(head))
        return None

    def _op_length(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LENGTH: Pop a list or alist, push its length as an integer."""
        value = self.stack.pop()
        if isinstance(value, (AIFPLList, AIFPLAList)):
            self.stack.append(AIFPLInteger(value.length()))
            return None

        raise AIFPLEvalError(
            f"Function 'length' requires list or alist argument, got {value.type_name()}"
        )

    def _op_reverse(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """REVERSE: Pop a list, push a new list with elements in reversed order."""
        value = self.stack.pop()
        list_val = self._ensure_list(value, 'reverse')
        self.stack.append(list_val.reverse())
        return None

    def _op_first(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """FIRST: Pop a list, push its first element."""
        value = self.stack.pop()
        list_val = self._ensure_list(value, 'first')
        try:
            self.stack.append(list_val.first())

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

        return None

    def _op_rest(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """REST: Pop a list, push a new list of all elements except the first."""
        value = self.stack.pop()
        list_val = self._ensure_list(value, 'rest')
        try:
            self.stack.append(list_val.rest())

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

        return None

    def _op_last(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LAST: Pop a list, push its last element."""
        value = self.stack.pop()
        list_val = self._ensure_list(value, 'last')
        try:
            self.stack.append(list_val.last())

        except IndexError as e:
            raise AIFPLEvalError(str(e)) from e

        return None

    def _op_list_ref(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LIST_REF: Pop an integer index and a list, push the element at that index."""
        index_val = self.stack.pop()
        value = self.stack.pop()
        list_val = self._ensure_list(value, 'list-ref')
        if not isinstance(index_val, AIFPLInteger):
            raise AIFPLEvalError(
                f"Function 'list-ref' requires integer index, got {index_val.type_name()}"
            )

        index = index_val.value
        if index < 0:
            raise AIFPLEvalError(f"list-ref index out of range: {index}")

        try:
            self.stack.append(list_val.get(index))

        except IndexError as e:
            raise AIFPLEvalError(f"list-ref index out of range: {index}") from e

        return None

    def _op_floor_div(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """FLOOR_DIV: Pop two values, compute a // b, push result."""
        b = self.stack.pop()
        a = self.stack.pop()
        a_val = self._ensure_real_number(a, '//')
        b_val = self._ensure_real_number(b, '//')
        if b_val == 0:
            raise AIFPLEvalError("Division by zero")

        self.stack.append(self._wrap_numeric_result(a_val // b_val))
        return None

    def _op_mod(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """MOD: Pop two values, compute a % b, push result."""
        b = self.stack.pop()
        a = self.stack.pop()
        a_val = self._ensure_real_number(a, '%')
        b_val = self._ensure_real_number(b, '%')
        if b_val == 0:
            raise AIFPLEvalError("Modulo by zero")

        self.stack.append(self._wrap_numeric_result(a_val % b_val))
        return None

    def _op_star_star(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STAR_STAR: Pop two values, compute a ** b (complex-capable), push result."""
        b = self.stack.pop()
        a = self.stack.pop()
        a_val = self._ensure_number(a, '**')
        b_val = self._ensure_number(b, '**')
        self.stack.append(self._wrap_numeric_result(a_val ** b_val))
        return None

    def _op_bit_not(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """BIT_NOT: Pop an integer, push bitwise NOT."""
        a = self.stack.pop()
        a_val = self._ensure_integer(a, 'bit-not')
        self.stack.append(AIFPLInteger(~a_val))
        return None

    def _op_bit_shift_left(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """BIT_SHIFT_LEFT: Pop shift amount and value, push value << n."""
        n = self.stack.pop()
        a = self.stack.pop()
        a_val = self._ensure_integer(a, 'bit-shift-left')
        n_val = self._ensure_integer(n, 'bit-shift-left')
        self.stack.append(AIFPLInteger(a_val << n_val))
        return None

    def _op_bit_shift_right(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """BIT_SHIFT_RIGHT: Pop shift amount and value, push value >> n."""
        n = self.stack.pop()
        a = self.stack.pop()
        a_val = self._ensure_integer(a, 'bit-shift-right')
        n_val = self._ensure_integer(n, 'bit-shift-right')
        self.stack.append(AIFPLInteger(a_val >> n_val))
        return None

    def _op_round(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ROUND: Pop a real number, push rounded integer."""
        a = self.stack.pop()
        a_val = self._ensure_real_number(a, 'round')
        self.stack.append(AIFPLInteger(round(a_val)))
        return None

    def _op_to_integer(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """TO_INTEGER: Pop a real number, push truncated integer."""
        a = self.stack.pop()
        a_val = self._ensure_real_number(a, 'integer')
        self.stack.append(AIFPLInteger(int(a_val)))
        return None

    def _op_to_float(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """TO_FLOAT: Pop a real number, push as float."""
        a = self.stack.pop()
        a_val = self._ensure_real_number(a, 'float')
        self.stack.append(AIFPLFloat(float(a_val)))
        return None

    def _op_real(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """REAL: Pop a number, push its real part as float."""
        a = self.stack.pop()
        a_val = self._ensure_number(a, 'real')
        if isinstance(a_val, complex):
            self.stack.append(AIFPLFloat(a_val.real))
            return None

        self.stack.append(AIFPLFloat(float(a_val)))
        return None

    def _op_imag(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """IMAG: Pop a number, push its imaginary part as float."""
        a = self.stack.pop()
        a_val = self._ensure_number(a, 'imag')
        if isinstance(a_val, complex):
            self.stack.append(AIFPLFloat(a_val.imag))
            return None

        self.stack.append(AIFPLFloat(0.0))
        return None

    def _op_make_complex(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """MAKE_COMPLEX: Pop imaginary and real parts, push complex number."""
        imag = self.stack.pop()
        real = self.stack.pop()
        real_val = self._ensure_real_number(real, 'complex')
        imag_val = self._ensure_real_number(imag, 'complex')
        self.stack.append(AIFPLComplex(complex(real_val, imag_val)))
        return None

    def _op_bin(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """BIN: Pop an integer, push Scheme-style binary string."""
        a = self.stack.pop()
        a_val = self._ensure_integer(a, 'bin')
        if a_val < 0:
            self.stack.append(AIFPLString(f"-#b{bin(-a_val)[2:]}"))
            return None

        self.stack.append(AIFPLString(f"#b{bin(a_val)[2:]}"))
        return None

    def _op_hex(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """HEX: Pop an integer, push Scheme-style hex string."""
        a = self.stack.pop()
        a_val = self._ensure_integer(a, 'hex')
        if a_val < 0:
            self.stack.append(AIFPLString(f"-#x{hex(-a_val)[2:]}"))
            return None

        self.stack.append(AIFPLString(f"#x{hex(a_val)[2:]}"))
        return None

    def _op_oct(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """OCT: Pop an integer, push Scheme-style octal string."""
        a = self.stack.pop()
        a_val = self._ensure_integer(a, 'oct')
        if a_val < 0:
            self.stack.append(AIFPLString(f"-#o{oct(-a_val)[2:]}"))
            return None

        self.stack.append(AIFPLString(f"#o{oct(a_val)[2:]}"))
        return None

    def _op_null_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """NULL_P: Pop a list, push true if empty."""
        value = self.stack.pop()
        list_val = self._ensure_list(value, 'null?')
        self.stack.append(AIFPLBoolean(list_val.is_empty()))
        return None

    def _op_member_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """MEMBER_P: Pop a list and item, push true if item is in list."""
        list_val_raw = self.stack.pop()
        item = self.stack.pop()
        list_val = self._ensure_list(list_val_raw, 'member?')
        self.stack.append(AIFPLBoolean(list_val.contains(item)))
        return None

    def _op_position(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """POSITION: Pop a list and item, push index or #f if not found."""
        list_val_raw = self.stack.pop()
        item = self.stack.pop()
        list_val = self._ensure_list(list_val_raw, 'position')
        pos = list_val.position(item)
        self.stack.append(AIFPLInteger(pos) if pos is not None else AIFPLBoolean(False))
        return None

    def _op_take(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """TAKE: Pop a list and count n, push first n elements."""
        list_val_raw = self.stack.pop()
        n_val = self.stack.pop()
        n = self._ensure_integer(n_val, 'take')
        list_val = self._ensure_list(list_val_raw, 'take')
        if n < 0:
            raise AIFPLEvalError(f"take count cannot be negative: {n}")

        self.stack.append(list_val.take(n))
        return None

    def _op_drop(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """DROP: Pop a list and count n, push list without first n elements."""
        list_val_raw = self.stack.pop()
        n_val = self.stack.pop()
        n = self._ensure_integer(n_val, 'drop')
        list_val = self._ensure_list(list_val_raw, 'drop')
        if n < 0:
            raise AIFPLEvalError(f"drop count cannot be negative: {n}")

        self.stack.append(list_val.drop(n))
        return None

    def _op_remove(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """REMOVE: Pop a list and item, push list with all occurrences of item removed."""
        list_val_raw = self.stack.pop()
        item = self.stack.pop()
        list_val = self._ensure_list(list_val_raw, 'remove')
        self.stack.append(list_val.remove_all(item))
        return None

    def _op_string_length(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_LENGTH: Pop a string, push its length."""
        a = self.stack.pop()
        self.stack.append(AIFPLInteger(len(self._ensure_string(a, 'string-length'))))
        return None

    def _op_string_upcase(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_UPCASE: Pop a string, push uppercased string."""
        a = self.stack.pop()
        self.stack.append(AIFPLString(self._ensure_string(a, 'string-upcase').upper()))
        return None

    def _op_string_downcase(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_DOWNCASE: Pop a string, push lowercased string."""
        a = self.stack.pop()
        self.stack.append(AIFPLString(self._ensure_string(a, 'string-downcase').lower()))
        return None

    def _op_string_trim(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_TRIM: Pop a string, push whitespace-trimmed string."""
        a = self.stack.pop()
        self.stack.append(AIFPLString(self._ensure_string(a, 'string-trim').strip()))
        return None

    def _op_string_to_number(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_TO_NUMBER: Pop a string, push parsed number."""
        a = self.stack.pop()
        s = self._ensure_string(a, 'string->number')
        try:
            if '.' not in s and 'e' not in s.lower() and 'j' not in s.lower():
                self.stack.append(AIFPLInteger(int(s)))
                return None

            if 'j' in s.lower():
                self.stack.append(AIFPLComplex(complex(s)))
                return None

            self.stack.append(AIFPLFloat(float(s)))
            return None

        except ValueError as e:
            raise AIFPLEvalError(f"Cannot convert string to number: '{s}'") from e

    def _op_number_to_string(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """NUMBER_TO_STRING: Pop a number, push string representation."""
        a = self.stack.pop()
        a_val = self._ensure_number(a, 'number->string')
        if isinstance(a_val, complex):
            self.stack.append(AIFPLString(str(a_val).strip('()')))
            return None

        self.stack.append(AIFPLString(str(a_val)))
        return None

    def _op_string_to_list(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_TO_LIST: Pop a string, push list of single-character strings."""
        a = self.stack.pop()
        s = self._ensure_string(a, 'string->list')
        self.stack.append(AIFPLList(tuple(AIFPLString(ch) for ch in s)))
        return None

    def _op_list_to_string(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LIST_TO_STRING: Pop a list of strings, push concatenated string."""
        a = self.stack.pop()
        list_val = self._ensure_list(a, 'list->string')
        self.stack.append(AIFPLString(''.join(str(elem.to_python()) for elem in list_val.elements)))
        return None

    def _op_string_ref(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_REF: Pop an index and string, push character at index."""
        index_val = self.stack.pop()
        a = self.stack.pop()
        s = self._ensure_string(a, 'string-ref')
        index = self._ensure_integer(index_val, 'string-ref')
        if index < 0 or index >= len(s):
            raise AIFPLEvalError(f"string-ref index out of range: {index}")

        self.stack.append(AIFPLString(s[index]))
        return None

    def _op_string_contains_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_CONTAINS_P: Pop substring and string, push true if string contains substring."""
        substr_val = self.stack.pop()
        a = self.stack.pop()
        s = self._ensure_string(a, 'string-contains?')
        substr = self._ensure_string(substr_val, 'string-contains?')
        self.stack.append(AIFPLBoolean(substr in s))
        return None

    def _op_string_prefix_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_PREFIX_P: Pop prefix and string, push true if string starts with prefix."""
        prefix_val = self.stack.pop()
        a = self.stack.pop()
        s = self._ensure_string(a, 'string-prefix?')
        prefix = self._ensure_string(prefix_val, 'string-prefix?')
        self.stack.append(AIFPLBoolean(s.startswith(prefix)))
        return None

    def _op_string_suffix_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_SUFFIX_P: Pop suffix and string, push true if string ends with suffix."""
        suffix_val = self.stack.pop()
        a = self.stack.pop()
        s = self._ensure_string(a, 'string-suffix?')
        suffix = self._ensure_string(suffix_val, 'string-suffix?')
        self.stack.append(AIFPLBoolean(s.endswith(suffix)))
        return None

    def _op_string_split(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_SPLIT: Pop delimiter and string, push list of parts."""
        delim_val = self.stack.pop()
        a = self.stack.pop()
        s = self._ensure_string(a, 'string-split')
        delim = self._ensure_string(delim_val, 'string-split')
        if delim == "":
            self.stack.append(AIFPLList(tuple(AIFPLString(ch) for ch in s)))
            return None

        self.stack.append(AIFPLList(tuple(AIFPLString(part) for part in s.split(delim))))
        return None

    def _op_string_join(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_JOIN: Pop separator and list of strings, push joined string."""
        sep_val = self.stack.pop()
        a = self.stack.pop()
        list_val = self._ensure_list(a, 'string-join')
        sep = self._ensure_string(sep_val, 'string-join')
        parts = []
        for item in list_val.elements:
            if not isinstance(item, AIFPLString):
                raise AIFPLEvalError(f"string-join requires list of strings, found {item.type_name()}")

            parts.append(item.value)

        self.stack.append(AIFPLString(sep.join(parts)))
        return None

    def _op_substring(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """SUBSTRING: Pop end, start, and string, push substring."""
        end_val = self.stack.pop()
        start_val = self.stack.pop()
        a = self.stack.pop()
        s = self._ensure_string(a, 'substring')
        start = self._ensure_integer(start_val, 'substring')
        end = self._ensure_integer(end_val, 'substring')
        n = len(s)
        if start < 0:
            raise AIFPLEvalError(f"substring start index cannot be negative: {start}")

        if end < 0:
            raise AIFPLEvalError(f"substring end index cannot be negative: {end}")

        if start > n:
            raise AIFPLEvalError(f"substring start index out of range: {start} (string length: {n})")

        if end > n:
            raise AIFPLEvalError(f"substring end index out of range: {end} (string length: {n})")

        if start > end:
            raise AIFPLEvalError(f"substring start index ({start}) cannot be greater than end index ({end})")

        self.stack.append(AIFPLString(s[start:end]))
        return None

    def _op_string_replace(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_REPLACE: Pop new, old, and string, push string with replacements."""
        new_val = self.stack.pop()
        old_val = self.stack.pop()
        a = self.stack.pop()
        s = self._ensure_string(a, 'string-replace')
        old = self._ensure_string(old_val, 'string-replace')
        new = self._ensure_string(new_val, 'string-replace')
        self.stack.append(AIFPLString(s.replace(old, new)))
        return None

    def _op_alist_keys(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ALIST_KEYS: Pop an alist, push list of its keys."""
        a = self.stack.pop()
        self.stack.append(AIFPLList(self._ensure_alist(a, 'alist-keys').keys()))
        return None

    def _op_alist_values(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ALIST_VALUES: Pop an alist, push list of its values."""
        a = self.stack.pop()
        self.stack.append(AIFPLList(self._ensure_alist(a, 'alist-values').values()))
        return None

    def _op_alist_length(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ALIST_LENGTH: Pop an alist, push its length."""
        a = self.stack.pop()
        self.stack.append(AIFPLInteger(self._ensure_alist(a, 'alist-length').length()))
        return None

    def _op_alist_has_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ALIST_HAS_P: Pop a key and alist, push true if alist contains key."""
        key = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLBoolean(self._ensure_alist(a, 'alist-has?').has_key(key)))
        return None

    def _op_alist_remove(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ALIST_REMOVE: Pop a key and alist, push new alist without that key."""
        key = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(self._ensure_alist(a, 'alist-remove').remove(key))
        return None

    def _op_alist_merge(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ALIST_MERGE: Pop two alists, push merged alist (second wins on conflicts)."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(self._ensure_alist(a, 'alist-merge').merge(self._ensure_alist(b, 'alist-merge')))
        return None

    def _op_alist_set(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ALIST_SET: Pop value, key, and alist, push new alist with key set to value."""
        value = self.stack.pop()
        key = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(self._ensure_alist(a, 'alist-set').set(key, value))
        return None

    def _op_alist_get(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ALIST_GET: Pop default, key, and alist, push value or default if not found."""
        default = self.stack.pop()
        key = self.stack.pop()
        a = self.stack.pop()
        result = self._ensure_alist(a, 'alist-get').get(key)
        self.stack.append(result if result is not None else default)
        return None

    def _op_bit_or(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """BIT_OR: Pop two integers, push a | b."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLInteger(self._ensure_integer(a, 'bit-or') | self._ensure_integer(b, 'bit-or')))
        return None

    def _op_bit_and(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """BIT_AND: Pop two integers, push a & b."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLInteger(self._ensure_integer(a, 'bit-and') & self._ensure_integer(b, 'bit-and')))
        return None

    def _op_bit_xor(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """BIT_XOR: Pop two integers, push a ^ b."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLInteger(self._ensure_integer(a, 'bit-xor') ^ self._ensure_integer(b, 'bit-xor')))
        return None

    def _op_append(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """APPEND: Pop two lists, push concatenated list."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(self._ensure_list(a, 'append').append_list(self._ensure_list(b, 'append')))
        return None

    def _op_string_append(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_APPEND: Pop two strings, push concatenated string."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLString(self._ensure_string(a, 'string-append') + self._ensure_string(b, 'string-append')))
        return None

    def _op_min(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """MIN: Pop two real numbers, push the smaller."""
        b = self.stack.pop()
        a = self.stack.pop()
        a_val = self._ensure_real_number(a, 'min')
        b_val = self._ensure_real_number(b, 'min')
        self.stack.append(self._wrap_numeric_result(a_val if a_val <= b_val else b_val))
        return None

    def _op_max(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """MAX: Pop two real numbers, push the larger."""
        b = self.stack.pop()
        a = self.stack.pop()
        a_val = self._ensure_real_number(a, 'max')
        b_val = self._ensure_real_number(b, 'max')
        self.stack.append(self._wrap_numeric_result(a_val if a_val >= b_val else b_val))
        return None

    def _op_eq(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """EQ: Pop two values, push true if they are equal."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLBoolean(a == b))
        return None

    def _op_neq(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """NEQ: Pop two values, push true if they are not equal."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLBoolean(a != b))
        return None

    def _op_lt(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LT: Pop two real numbers, push true if a < b."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLBoolean(self._ensure_real_number(a, '<') < self._ensure_real_number(b, '<')))
        return None

    def _op_gt(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """GT: Pop two real numbers, push true if a > b."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLBoolean(self._ensure_real_number(a, '>') > self._ensure_real_number(b, '>')))
        return None

    def _op_lte(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LTE: Pop two real numbers, push true if a <= b."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLBoolean(self._ensure_real_number(a, '<=') <= self._ensure_real_number(b, '<=')))
        return None

    def _op_gte(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """GTE: Pop two real numbers, push true if a >= b."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLBoolean(self._ensure_real_number(a, '>=') >= self._ensure_real_number(b, '>=')))
        return None

    def _op_string_eq_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """STRING_EQ_P: Pop two strings, push true if they are equal."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLBoolean(self._ensure_string(a, 'string=?') == self._ensure_string(b, 'string=?')))
        return None

    def _op_number_eq_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """NUMBER_EQ_P: Pop two numbers, push true if they are numerically equal."""
        b = self.stack.pop()
        a = self.stack.pop()
        self._ensure_number(a, 'number=?')
        self._ensure_number(b, 'number=?')
        self.stack.append(AIFPLBoolean(a == b))
        return None

    def _op_integer_eq_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """INTEGER_EQ_P: Pop two values, push true if both are integers and equal."""
        b = self.stack.pop()
        a = self.stack.pop()
        if not isinstance(a, AIFPLInteger):
            raise AIFPLEvalError(f"Function 'integer=?' requires integer arguments, got {a.type_name()}")

        if not isinstance(b, AIFPLInteger):
            raise AIFPLEvalError(f"Function 'integer=?' requires integer arguments, got {b.type_name()}")

        self.stack.append(AIFPLBoolean(a.value == b.value))
        return None

    def _op_float_eq_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """FLOAT_EQ_P: Pop two values, push true if both are floats and equal."""
        b = self.stack.pop()
        a = self.stack.pop()
        if not isinstance(a, AIFPLFloat):
            raise AIFPLEvalError(f"Function 'float=?' requires float arguments, got {a.type_name()}")

        if not isinstance(b, AIFPLFloat):
            raise AIFPLEvalError(f"Function 'float=?' requires float arguments, got {b.type_name()}")

        self.stack.append(AIFPLBoolean(a.value == b.value))
        return None

    def _op_complex_eq_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """COMPLEX_EQ_P: Pop two values, push true if both are complex and equal."""
        b = self.stack.pop()
        a = self.stack.pop()
        if not isinstance(a, AIFPLComplex):
            raise AIFPLEvalError(f"Function 'complex=?' requires complex arguments, got {a.type_name()}")

        if not isinstance(b, AIFPLComplex):
            raise AIFPLEvalError(f"Function 'complex=?' requires complex arguments, got {b.type_name()}")

        self.stack.append(AIFPLBoolean(a.value == b.value))
        return None

    def _op_boolean_eq_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """BOOLEAN_EQ_P: Pop two values, push true if both are booleans and equal."""
        b = self.stack.pop()
        a = self.stack.pop()
        bool_a = self._ensure_boolean(a, 'boolean=?')
        bool_b = self._ensure_boolean(b, 'boolean=?')
        self.stack.append(AIFPLBoolean(bool_a == bool_b))
        return None

    def _op_list_eq_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """LIST_EQ_P: Pop two values, push true if both are lists and equal."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLBoolean(self._ensure_list(a, 'list=?') == self._ensure_list(b, 'list=?')))
        return None

    def _op_alist_eq_p(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """ALIST_EQ_P: Pop two values, push true if both are alists and equal."""
        b = self.stack.pop()
        a = self.stack.pop()
        self.stack.append(AIFPLBoolean(self._ensure_alist(a, 'alist=?') == self._ensure_alist(b, 'alist=?')))
        return None

    def _op_build_list(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, n: int, _arg2: int
    ) -> AIFPLValue | None:
        """BUILD_LIST n: Pop n values from stack (top is last element), push AIFPLList."""
        if n == 0:
            self.stack.append(AIFPLList(()))
            return None

        elements = self.stack[-n:]
        del self.stack[-n:]
        self.stack.append(AIFPLList(tuple(elements)))
        return None

    def _op_build_alist(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, n: int, _arg2: int
    ) -> AIFPLValue | None:
        """BUILD_ALIST n: Pop n 2-element AIFPLList pair objects, push AIFPLAList.

        Each pair on the stack is a 2-element list (list key value), matching the
        existing (alist (list k1 v1) (list k2 v2) ...) calling convention.
        """
        if n == 0:
            self.stack.append(AIFPLAList(()))
            return None

        pair_lists = self.stack[-n:]
        del self.stack[-n:]
        pairs = []
        for i, pair_list in enumerate(pair_lists):
            if not isinstance(pair_list, AIFPLList):
                raise AIFPLEvalError(
                    f"AList pair {i + 1} must be a list"
                )

            if len(pair_list.elements) != 2:
                raise AIFPLEvalError(
                    f"AList pair {i + 1} must have exactly 2 elements"
                )

            pairs.append((pair_list.elements[0], pair_list.elements[1]))

        self.stack.append(AIFPLAList(tuple(pairs)))
        return None

    def _op_range(  # pylint: disable=useless-return
        self, _frame: Frame, _code: CodeObject, _arg1: int, _arg2: int
    ) -> AIFPLValue | None:
        """RANGE: Pop step, end, and start integers, push list of integers."""
        step_val = self.stack.pop()
        end_val = self.stack.pop()
        start_val = self.stack.pop()
        start = self._ensure_integer(start_val, 'range')
        end = self._ensure_integer(end_val, 'range')
        step = self._ensure_integer(step_val, 'range')
        if step == 0:
            raise AIFPLEvalError("Range step cannot be zero")

        self.stack.append(AIFPLList(tuple(AIFPLInteger(v) for v in range(start, end, step))))
        return None
