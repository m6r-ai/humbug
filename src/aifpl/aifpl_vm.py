"""AIFPL Virtual Machine - executes bytecode."""

import difflib
from dataclasses import dataclass, field
from typing import List, Dict, Any, cast, Optional, Protocol

from aifpl.aifpl_builtins import AIFPLBuiltinRegistry
from aifpl.aifpl_bytecode import CodeObject, Opcode
from aifpl.aifpl_bytecode_validator import validate_bytecode
from aifpl.aifpl_error import AIFPLEvalError
from aifpl.aifpl_value import (
    AIFPLValue, AIFPLBoolean, AIFPLString, AIFPLList, AIFPLFunction,
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

        # Create builtin registry and get function array for fast lookup
        builtin_registry = AIFPLBuiltinRegistry()

        # Get builtin function array for O(1) direct indexing in CALL_BUILTIN
        self._builtin_function_array = builtin_registry.get_function_array()

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
        table[Opcode.CALL_BUILTIN] = self._op_call_builtin
        table[Opcode.TAIL_CALL_FUNCTION] = self._op_tail_call_function
        table[Opcode.RETURN] = self._op_return
        table[Opcode.EMIT_TRACE] = self._op_emit_trace
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
        self.stack = []
        self.frames = [Frame(code)]

        # Execute until we return
        return self._execute_frame()

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

        while True:
            # Re-fetch code and instructions each iteration in case frame.code changes (mutual recursion TCO)
            code = frame.code
            instructions = code.instructions
            if frame.ip >= len(instructions):
                break

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
        self,
        _frame: Frame,
        code: CodeObject,
        arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_CONST: Push constant from pool onto stack."""
        # Validator guarantees arg1 is in bounds
        # No bounds check needed - direct access for maximum performance
        self.stack.append(code.constants[arg1])
        return None

    def _op_load_true(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        _arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_TRUE: Push boolean true onto stack."""
        self.stack.append(AIFPLBoolean(True))
        return None

    def _op_load_false(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        _arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_FALSE: Push boolean false onto stack."""
        self.stack.append(AIFPLBoolean(False))
        return None

    def _op_load_empty_list(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        _arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """LOAD_EMPTY_LIST: Push empty list onto stack."""
        self.stack.append(AIFPLList(()))
        return None

    def _op_load_var(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        _arg1: int,
        index: int
    ) -> AIFPLValue | None:
        """LOAD_VAR: Load variable from current frame at index."""
        # Validator guarantees index is in bounds AND variable is initialized
        current_frame = self.frames[-1]
        value = current_frame.locals[index]
        self.stack.append(cast(AIFPLValue, value))
        return None

    def _op_store_var(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        _arg1: int,
        index: int
    ) -> AIFPLValue | None:
        """STORE_VAR: Store top of stack to variable in current frame at index."""
        # Validator guarantees index is in bounds and stack has value
        value = self.stack.pop()
        self.frames[-1].locals[index] = value
        return None

    def _op_load_parent_var(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        depth: int,
        index: int
    ) -> AIFPLValue | None:
        """
        LOAD_PARENT_VAR: Load variable from parent frame.

        This is used for recursive closures in letrec - the closure references
        a binding from its parent frame rather than capturing it.

        Args:
            depth - how many parent frames to walk up
            index - variable index in the target parent frame
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
        self,
        _frame: Frame,
        code: CodeObject,
        arg1: int,
        _arg2: int
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
        self,
        frame: Frame,
        _code: CodeObject,
        target: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """JUMP: Unconditional jump to instruction."""
        frame.ip = target
        return None

    def _op_jump_if_false(  # pylint: disable=useless-return
        self,
        frame: Frame,
        _code: CodeObject,
        target: int,
        _arg2: int
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
        self,
        frame: Frame,
        _code: CodeObject,
        target: int,
        _arg2: int
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
        self,
        _frame: Frame,
        code: CodeObject,
        arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """RAISE_ERROR: Raise error with message from constant pool."""
        # Validator guarantees arg1 is in bounds
        # Type check could be removed if we validate constant types, but keep for now
        error_msg = code.constants[arg1]
        if not isinstance(error_msg, AIFPLString):
            raise AIFPLEvalError("RAISE_ERROR requires a string constant")

        raise AIFPLEvalError(error_msg.value)

    def _op_make_closure(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        code: CodeObject,
        arg1: int,
        capture_count: int
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
            parent_frame=current_frame  # Store parent frame for LOAD_PARENT_VAR
        )
        self.stack.append(closure)
        return None

    def _op_call_function(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        arity: int,
        _arg2: int
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
        if arity != expected_arity:
            func_name = func.name or "<lambda>"
            raise AIFPLEvalError(
                message=f"Function '{func_name}' expects {expected_arity} arguments, got {arity}",
                suggestion=f"Provide exactly {expected_arity} argument{'s' if expected_arity != 1 else ''}"
            )

        # Remove function from stack
        del self.stack[-(arity + 1)]

        self._setup_call_frame(func)

        result = self._execute_frame()
        self.stack.append(result)
        return None

    def _op_tail_call_function(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        arity: int,
        _arg2: int
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

        # Handle builtin functions
        # Builtins can't be tail-call-optimized, but we can return their result directly
        if func.is_native:
            args = [self.stack.pop() for _ in range(arity)]
            args.reverse()
            self.stack.pop()  # Pop function
            assert func.native_impl is not None, f"Function {func.name} has no native implementation"
            result = func.native_impl(args)
            return result  # Return directly (we're in tail position)

        # Check arity for bytecode functions
        # Must keep: arity check (runtime-dependent)
        expected_arity = func.bytecode.param_count
        if arity != expected_arity:
            func_name = func.name or "<lambda>"
            raise AIFPLEvalError(
                message=f"Function '{func_name}' expects {expected_arity} arguments, got {arity}",
                suggestion=f"Provide exactly {expected_arity} argument{'s' if expected_arity != 1 else ''}"
            )

        # Remove function from stack (leave arguments for new frame)
        del self.stack[-(arity + 1)]

        # Return TailCall marker - execution loop will handle frame replacement
        return TailCall(func)

    def _op_call_builtin(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        builtin_index: int,
        arity: int
    ) -> AIFPLValue | None:
        """CALL_BUILTIN: Call builtin function by index."""
        # Validator guarantees builtin_index is valid and stack has enough values
        # Direct array access without bounds checking
        # Get arguments from stack
        if arity == 0:
            args = []

        else:
            # Slice gets args in correct order, then delete them from stack
            args = self.stack[-arity:]
            del self.stack[-arity:]

        # Direct array indexing - O(1) lookup, no bounds check needed
        func = self._builtin_function_array[builtin_index]
        result = func(args)
        self.stack.append(result)
        return None

    def _op_return(
        self,
        _frame: Frame,
        _code: CodeObject,
        _arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """RETURN: Pop frame and return value from stack."""
        # Validator guarantees stack has a value to return
        self.frames.pop()
        return self.stack.pop()

    def _op_emit_trace(  # pylint: disable=useless-return
        self,
        _frame: Frame,
        _code: CodeObject,
        _arg1: int,
        _arg2: int
    ) -> AIFPLValue | None:
        """EMIT_TRACE: Pop value from stack and emit to trace watcher."""
        # Pop the message from stack
        message = self.stack.pop()

        # Emit trace if watcher is available
        if self.trace_watcher:
            self._emit_trace(message)

        # Continue execution (no return value)
        return None

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
